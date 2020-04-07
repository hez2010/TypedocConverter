module Printer

open Entity
open Helpers
open Definitions

let append spliter accu next = accu + spliter + next

let arrangeComment (comment: string) blankSpace = 
    let blanks = 
        seq {
            for _ = 1 to blankSpace do yield " "
        } |> Seq.reduce (append "")
    comment.Split("\n")
    |> Array.map(fun x -> blanks + x)
    |> Array.reduce (append "\n")

let rec arrangeType (config: Config) (typeInfo: Entity) =
    let pascalizeTypeName name =
        match name with
        | "object" | "string" | "double" | "void" | "bool" | "ulong"
        | "uint" | "ushort" | "byte" | "long" | "int" | "short" | "char" -> name
        | _ -> toPascalCase name
    match typeInfo with
    | TypeEntity("System.Array", innerTypes) ->
        match config.ArrayType with
        | "Array" -> 
            (match innerTypes with
            | [x] -> arrangeType config x
            | _ -> "object") + "[]"
        | _ ->
            match innerTypes with
                | [] -> config.ArrayType
                | types -> config.ArrayType + "<" + System.String.Join(", ", types |> List.map (arrangeType config)) + ">"
    | TypeEntity("System.ValueTuple", innerTypes) ->
        "(" + System.String.Join(", ", innerTypes |> List.map (arrangeType config)) + ")"
    | TypeEntity(name, innerTypes) ->
        match innerTypes with
        | [] -> pascalizeTypeName name
        | types -> pascalizeTypeName name + "<" + System.String.Join(", ", types |> List.map (arrangeType config)) + ">"
    | _ -> "object"

let printConverter (writer: System.IO.TextWriter) (entity: Entity) = 
    match entity with
    | StringUnionEntity(_, name, _, _, members) ->
        fprintfn writer "    class %s%s" (toPascalCase name) "Converter : Newtonsoft.Json.JsonConverter"
        fprintfn writer "    {"
        fprintfn writer "        public override bool CanConvert(System.Type t) => t == typeof(%s) || t == typeof(%s?);" (toPascalCase name) (toPascalCase name)
        fprintfn writer ""
        fprintfn writer "        public override object ReadJson(Newtonsoft.Json.JsonReader reader, System.Type t, object? existingValue, Newtonsoft.Json.JsonSerializer serializer)"
        fprintfn writer "            => reader.TokenType switch"
        fprintfn writer "            {"
        fprintfn writer "                Newtonsoft.Json.JsonToken.String =>"
        fprintfn writer "                    serializer.Deserialize<string>(reader) switch"
        fprintfn writer "                    {"
        members
        |> List.iter
            (
                fun x ->
                    match x with
                    | EnumMemberEntity(eName, _, _) ->
                        fprintfn writer "                        \"%s\" => %s.%s," eName (toPascalCase name) (toPascalCase eName)
                    | _ -> ()
            )
        fprintfn writer "                        _ => throw new System.Exception(\"Cannot unmarshal type %s\")" (toPascalCase name)
        fprintfn writer "                    },"
        fprintfn writer "                _ => throw new System.Exception(\"Cannot unmarshal type %s\")" (toPascalCase name)
        fprintfn writer "            };"
        fprintfn writer ""
        fprintfn writer "        public override void WriteJson(Newtonsoft.Json.JsonWriter writer, object? untypedValue, Newtonsoft.Json.JsonSerializer serializer)"
        fprintfn writer "        {"
        fprintfn writer "            if (untypedValue is null) { serializer.Serialize(writer, null); return; }"
        fprintfn writer "            var value = (%s)untypedValue;" (toPascalCase name)
        fprintfn writer "            switch (value)"
        fprintfn writer "            {"
        members
        |> List.iter
            (
                fun x ->
                    match x with
                    | EnumMemberEntity(eName, _, _) ->
                        fprintfn writer  "                case %s.%s: serializer.Serialize(writer, \"%s\"); return;" (toPascalCase name) (toPascalCase eName) eName
                    | _ -> ()
            )
        fprintfn writer "                default: break;"
        fprintfn writer "            }"
        fprintfn writer "            throw new System.Exception(\"Cannot marshal type %s\");" (toPascalCase name)
        fprintfn writer "        }"
        fprintfn writer "    }"
    | _ -> ()

let getNamespaceAndName entity =
    match entity with
    | ClassInterfaceEntity(ns, name, _, _, _, _, _, _, _, _) -> Some (ns, name)
    | EnumEntity(ns, name, _, _, _) -> Some (ns, name)
    | _ -> None

let printEntity (writer: System.IO.TextWriter) (config: Config) (references: string list) (entity: Entity) = 
    let printHeader ns comment =
        fprintfn writer "namespace %s\n{" (toPascalCase ns)
        references
        |> List.where(fun x -> x <> ns)
        |> List.iter(fun x -> fprintfn writer "    using %s;" (toPascalCase x))
        fprintfn writer  ""
        if (comment <> "") then fprintfn writer "%s" (arrangeComment comment 4) else ()

    let printName eType modifier name exts tps = 
        fprintfn writer "    %s%s%s %s%s%s\n    {"
            (
                match modifier with
                | [] -> ""
                | _ -> modifier |> List.reduce (fun a b -> a + b + " ")
            )
            (if modifier = [] then "" else " ")
            eType (toPascalCase name)
            (
                match tps with
                | [] -> ""
                | paras -> 
                    "<" + System.String.Join(", ", paras |> List.collect(fun x -> 
                        match x with | TypeParameterEntity(tpName) -> [toPascalCase tpName] | _ -> [])
                    ) + ">"
            )
            (
                match exts with
                | [] -> ""
                | paras -> " : " + System.String.Join(", ", paras |> List.map (arrangeType config))
            )

    let printEnumMember name comment value isLastOne = 
        if comment <> "" then fprintfn writer "%s" (arrangeComment comment 8) else ()
        fprintfn writer "        %s%s%s"
            (toPascalCase name)
            (
                match value with
                | None -> ""
                | Some value -> " = " + string value
            )
            (if isLastOne then "" else ",")

    let printProperty name comment modifier eType withGet withSet isOptional initValue isInInterface =
        if (comment <> "") then fprintfn writer "%s" (arrangeComment comment 8) else ()
        fprintfn writer "        [Newtonsoft.Json.JsonProperty(\"%s\", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]" name
        fprintfn writer "        %s%s%s %s { %s%s}%s"
            (
                if modifier = [] then 
                    if isInInterface then "" else "public "
                else System.String.Join(" ", modifier) + " "
            )
            (arrangeType config eType)
            (if isOptional then "?" else "")
            (toPascalCase name)
            (
                if isInInterface
                then
                    if withGet then "get; " else ""
                else
                    if withGet then "get => throw new System.NotImplementedException(); " else ""
            )
            (
                if isInInterface 
                then
                    if withSet then "set; " else ""
                else
                    if withSet then "set => throw new System.NotImplementedException(); " else ""
            )
            (
                match initValue with
                | None -> ""
                | Some value -> " = " + value + ";"
            )
        fprintfn writer ""

    let printEvent name comment modifier isOptional eType isInInterface = 
        if (comment <> "") then fprintfn writer "%s" (arrangeComment comment 8) else ()
        fprintfn writer "        %sevent %s%s %s;"
            (
                if modifier = [] then 
                    if isInInterface then "" else "public "
                else System.String.Join(" ", modifier) + " "
            )
            (arrangeType config eType)
            (if isOptional then "?" else "")
            (toPascalCase name)
        fprintfn writer ""

    let printMethod name comment modifier tps paras mType isInInterface = 
        if (comment <> "") then fprintfn writer "%s" (arrangeComment comment 8) else ()
        fprintfn writer "        %s%s %s%s(%s)%s;"
            (
                if modifier = [] then 
                    if isInInterface then "" else "public "
                else System.String.Join(" ", modifier) + " "
            )
            (arrangeType config mType)
            (toPascalCase name)
            (
                match tps with
                | [] -> ""
                | tps -> "<" + System.String.Join(", ", tps |> List.collect (fun x -> 
                    match x with | TypeParameterEntity(tpName) -> [toPascalCase tpName] | _ -> [])) + ">"
            )
            (
                let mutable args = []
                let conflictNames = ["abstract"; "as"; "base"; "bool"; "break"; "byte";
                    "case"; "catch"; "char"; "checked"; "class"; "const"; "continue"; 
                    "decimal"; "default"; "delegate"; "do"; "double"; "else"; "enum"; 
                    "event"; "explicit"; "extern"; "false"; "finally"; "fixed"; "float";
                    "for"; "foreach"; "goto"; "if"; "implicit"; "in"; "int"; "interface"; 
                    "internal"; "is"; "lock"; "long"; "namespace"; "new"; "null"; "object";
                    "operator"; "out"; "override"; "params"; "private"; "protected"; "public";
                    "readonly"; "ref"; "return"; "sbyte"; "sealed"; "short"; "sizeof"; 
                    "stackalloc"; "static"; "string"; "struct"; "switch"; "this"; "throw"; 
                    "true"; "try"; "typeof"; "uint"; "ulong"; "unchecked"; "unsafe"; 
                    "ushort"; "using"; "virtual"; "void"; "volatile"; "while";]

                let rec getArg arg cnt = 
                    let outArg = 
                        if args |> List.contains (if cnt = 0 then arg else arg + string cnt)
                        then getArg arg (cnt + 1)
                        else 
                            let newArg = if cnt = 0 then arg else arg + string cnt
                            args <- args @ [newArg]
                            newArg
                    if conflictNames |> List.contains outArg then "@" + outArg else outArg
                System.String.Join(
                    ", ", 
                    paras |> List.collect 
                        (fun i -> 
                            match i with
                            | ParameterEntity(pName, pType) ->
                                [(arrangeType config pType) + " " + getArg pName 0]
                            | _ -> []
                        )
                )
            )
            (
                if isInInterface then ""
                else " => throw new System.NotImplementedException()"
            )
        fprintfn writer ""

    match entity with
    | EnumEntity(ns, name, comment, modifier, members) ->
        printHeader ns comment
        printName "enum" modifier name [] []
        members |> List.iteri
            (
                fun i x ->
                    match x with
                    | EnumMemberEntity(eName, eComment, eValue) -> printEnumMember eName eComment eValue (i = members.Length - 1)
                    | _ -> ()
            )
        fprintfn writer "    }"
        fprintfn writer "}\n"
    | StringUnionEntity(ns, name, comment, modifier, members) ->
        printHeader ns comment
        fprintfn writer "    [Newtonsoft.Json.JsonConverter(typeof(%s%s))]" (toPascalCase name) "Converter"
        printName "enum" modifier name [] []
        members |> List.iteri
            (
                fun i x ->
                    match x with
                    | EnumMemberEntity(eName, eComment, eValue) -> printEnumMember eName eComment eValue (i = members.Length - 1)
                    | _ -> ()
            )
        fprintfn writer "    }"
        fprintfn writer ""
        printConverter writer entity
        fprintfn writer "}\n"
    | ClassInterfaceEntity(ns, name, comment, modifier, methods, properties, events, exts, tps, isInterface) ->
        printHeader ns comment
        if isInterface then printName "interface" modifier name exts tps
        else printName "class" modifier name exts tps
        properties
        |> List.iter
            (
                fun x ->
                    match x with
                    | PropertyEntity(pName, pComment, pModifier, pType, withGet, withSet, isOptional, initValue) ->
                        printProperty pName pComment pModifier pType withGet withSet isOptional initValue isInterface
                    | _ -> ()
            )
        events
        |> List.iter
            (
                fun x ->
                    match x with
                    | EventEntity(eName, eComment, eModifier, isOptional, eType) ->
                        printEvent eName eComment eModifier isOptional eType isInterface
                    | _ -> ()
            )
        methods
        |> List.iter
            (
                fun x ->
                    match x with
                    | MethodEntity(mName, mComment, mModifier, mTps, paras, mType) ->
                        printMethod mName mComment mModifier mTps paras mType isInterface
                    | _ -> ()
            )
        fprintfn writer "    }"
        fprintfn writer "}\n"
    | _ -> ()

let printEntities (splitFile: bool) (output: string) (config: Config) (entities: Entity list) = 
    let namespaces = 
        entities 
        |> List.map getNamespaceAndName
        |> List.collect (fun x -> match x with | Some(v, _) -> [v] | _ -> [])
        |> List.distinct
    if (splitFile) then
        entities 
        |> List.iter
            (
                fun x ->
                    let info = getNamespaceAndName x
                    match info with
                    | Some(ns, name) -> 
                        let path = System.IO.Path.Combine([output]@((toPascalCase ns).Split(".") |> List.ofArray) |> Array.ofList)
                        if not (System.IO.Directory.Exists path) 
                        then System.IO.Directory.CreateDirectory path |> ignore
                        else ()
                        use file = new System.IO.FileStream(System.IO.Path.Combine(path, toPascalCase name + ".cs"), System.IO.FileMode.Create)
                        use textWriter = new System.IO.StreamWriter(file)
                        printEntity textWriter config namespaces x
                    | _ -> ()
            )
    else
        let path = output
        let dir = System.IO.Path.GetDirectoryName path
        if dir <> "" && not (System.IO.Directory.Exists dir) 
        then System.IO.Directory.CreateDirectory dir |> ignore
        else ()
        use file = new System.IO.FileStream(path, System.IO.FileMode.Create)
        use textWriter = new System.IO.StreamWriter(file)
        entities 
        |> List.iter(printEntity textWriter config namespaces)
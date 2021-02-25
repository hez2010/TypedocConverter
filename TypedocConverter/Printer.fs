module Printer

open Entity
open Helpers
open Definitions

let mutable deferredEntities : Entity Set = Set.empty

let append spliter accu next = accu + spliter + next

let arrangeComment (comment: string) blankSpace = 
    let blanks = 
        seq {
            for _ = 1 to blankSpace do yield " "
        } |> Seq.reduce (append "")
    comment.Split("\n")
    |> Array.map(fun x -> blanks + x)
    |> Array.reduce (append "\n")

let rec arrangeType (config: Config) (writer: System.IO.TextWriter) (typeInfo: Entity) =
    let pascalizeTypeName name =
        match name with
        | "object" | "string" | "double" | "void" | "bool" | "ulong"
        | "uint" | "ushort" | "byte" | "long" | "int" | "short" | "char" -> name
        | _ -> toPascalCase name
    let arrangedType = 
        match typeInfo with
        | TypeEntity(_, "System.Array", _, innerTypes, _, _) ->
            match config.ArrayType with
            | "Array" -> 
                (match innerTypes with
                | [x] -> arrangeType config writer x
                | _ -> "object") + "[]"
            | _ ->
                match innerTypes with
                    | [] -> config.ArrayType
                    | types -> config.ArrayType + "<" + System.String.Join(", ", types |> List.map (arrangeType config writer)) + ">"
        | TypeEntity(_, "System.ValueTuple", _, innerTypes, _, _) ->
            "(" + System.String.Join(", ", innerTypes |> List.map (arrangeType config writer)) + ")"
        | TypeEntity(_, name, _, innerTypes, Plain, _) ->
            match innerTypes with
            | [] -> pascalizeTypeName name
            | types -> pascalizeTypeName name + "<" + System.String.Join(", ", types |> List.map (arrangeType config writer)) + ">"
        | TypeEntity(id, name, _, innerTypes, Literal, _) ->
            match innerTypes with
            | [] -> "object"
            | types ->
                let entity = 
                    ClassInterfaceEntity(
                        id, "TypedocConverter.GeneratedTypes", name, "", [],
                        types |> List.map(fun t ->
                            let literal = 
                                match t with
                                | TypeLiteralElementEntity(_, eName, eType) -> (eName, eType)
                                | _ -> failwith "Unexpected entity"
                            PropertyEntity(id, fst literal, "", [], snd literal, true, true, false, None)
                        ), [], [], true
                    )
                deferredEntities <- Set.add entity deferredEntities
                "TypedocConverter.GeneratedTypes." + name
        | UnionTypeEntity _ -> arrangeUnionType config writer typeInfo
        | _ -> "object"
    match typeInfo with
    | TypeEntity(_, _, _, _, _, Some(annotatedName)) -> arrangedType + " " + (pascalizeTypeName annotatedName)
    | _ -> arrangedType
and arrangeUnionType (config: Config) (writer: System.IO.TextWriter) (typeInfo: Entity) = 
    let printUnionType (name: string) (types: string list) =
        printWarning ("Taking " + name + " for union type "+ System.String.Join(" | ", types) + ".")
    match typeInfo with
    | UnionTypeEntity(_, _, inner, _) ->
        let types = inner |> List.map (arrangeType config writer) |> List.distinct
        let objRemoved = types |> List.where(fun x -> x <> "object")
        match objRemoved with
        | [] -> "object"
        | [x] -> x
        | _ ->
            let takenType = inner |> List.sortWith typeSorter |> List.head
            let name = arrangeType config writer takenType
            printUnionType name types
            name
    | _ -> "object"
and typeSorter typeA typeB = 
    let typesOrder = ["array"; "tuple"; "reference"; "reflection"; "stringLiteral"; "intrinsic"]
    let compare typeIdA typeIdB = 
        let indexA = typesOrder |> List.tryFindIndex (fun x -> x = typeIdA)
        let indexB = typesOrder |> List.tryFindIndex (fun x -> x = typeIdB)
        match (indexA, indexB) with
        | (None, None) -> 0
        | (Some _, None) -> -1
        | (None, Some _) -> 1
        | (Some a, Some b) -> a.CompareTo b
    match (typeA, typeB) with
    | (TypeEntity(_, _, typeIdA, _, _, _), TypeEntity(_, _, typeIdB, _, _, _)) -> compare typeIdA typeIdB
    | (TypeEntity(_, _, typeIdA, _, _, _), UnionTypeEntity(_, typeIdB, _, _)) -> compare typeIdA typeIdB
    | (UnionTypeEntity(_, typeIdA, _, _), TypeEntity(_, _, typeIdB, _, _, _)) -> compare typeIdA typeIdB
    | (UnionTypeEntity(_, typeIdA, _, _), UnionTypeEntity(_, typeIdB, _, _)) -> compare typeIdA typeIdB
    | _ -> 0

let arrangeParameterList config (writer: System.IO.TextWriter) paras = 
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
                | ParameterEntity(_, pName, pType) ->
                    [(arrangeType config writer pType) + " " + getArg pName 0]
                | _ -> []
            )
    )

let printNewtonsoftJsonConverter (writer: System.IO.TextWriter) (entity: Entity) (config: Config) = 
    match entity with
    | StringUnionEntity(_, _, name, _, _, members) ->
        fprintfn writer "    class %s%s" (toPascalCase name) "Converter : Newtonsoft.Json.JsonConverter"
        fprintfn writer "    {"
        fprintfn writer "        public override bool CanConvert(System.Type t) => t == typeof(%s) || t == typeof(%s?);" (toPascalCase name) (toPascalCase name)
        fprintfn writer ""
        fprintfn writer "        public override object ReadJson(Newtonsoft.Json.JsonReader reader, System.Type t, object%s existingValue, Newtonsoft.Json.JsonSerializer serializer)" (if config.NrtDisabled then "" else "?")
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
                    | EnumMemberEntity(_, eName, _, _) ->
                        fprintfn writer "                        \"%s\" => %s.%s," eName (toPascalCase name) (toPascalCase eName)
                    | _ -> ()
            )
        fprintfn writer "                        _ => throw new System.NotSupportedException(\"Cannot unmarshal type %s\")" (toPascalCase name)
        fprintfn writer "                    },"
        fprintfn writer "                _ => throw new System.NotSupportedException(\"Cannot unmarshal type %s\")" (toPascalCase name)
        fprintfn writer "            };"
        fprintfn writer ""
        fprintfn writer "        public override void WriteJson(Newtonsoft.Json.JsonWriter writer, object%s untypedValue, Newtonsoft.Json.JsonSerializer serializer)" (if config.NrtDisabled then "" else "?")
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
                    | EnumMemberEntity(_, eName, _, _) ->
                        fprintfn writer  "                case %s.%s: serializer.Serialize(writer, \"%s\"); return;" (toPascalCase name) (toPascalCase eName) eName
                    | _ -> ()
            )
        fprintfn writer "                default: break;"
        fprintfn writer "            }"
        fprintfn writer "            throw new System.NotSupportedException(\"Cannot marshal type %s\");" (toPascalCase name)
        fprintfn writer "        }"
        fprintfn writer "    }"
    | _ -> ()

let printSystemJsonConverter (writer: System.IO.TextWriter) (entity: Entity) (config: Config) = 
    match entity with
    | StringUnionEntity(_, _, name, _, _, members) ->
        fprintfn writer "    class %s%s<%s>" (toPascalCase name) "Converter : System.Text.Json.Serialization.JsonConverter" (toPascalCase name)
        fprintfn writer "    {"
        fprintfn writer "        public override %s Read(ref System.Text.Json.Utf8JsonReader reader, System.Type typeToConvert, System.Text.Json.JsonSerializerOptions options)" (toPascalCase name)
        fprintfn writer "            => reader.TokenType switch"
        fprintfn writer "            {"
        fprintfn writer "                System.Text.Json.JsonTokenType.String =>"
        fprintfn writer "                    System.Text.Json.JsonSerializer.Deserialize<string>(ref reader, options) switch"
        fprintfn writer "                    {"
        members
        |> List.iter
            (
                fun x ->
                    match x with
                    | EnumMemberEntity(_, eName, _, _) ->
                        fprintfn writer "                        \"%s\" => %s.%s," eName (toPascalCase name) (toPascalCase eName)
                    | _ -> ()
            )
        fprintfn writer "                        _ => throw new System.NotSupportedException(\"Cannot unmarshal type %s\")" (toPascalCase name)
        fprintfn writer "                    },"
        fprintfn writer "                _ => throw new System.NotSupportedException(\"Cannot unmarshal type %s\")" (toPascalCase name)
        fprintfn writer "            };"
        fprintfn writer ""
        fprintfn writer "        public override void Write(System.Text.Json.Utf8JsonWriter writer, %s%s value, System.Text.Json.JsonSerializerOptions options)" (if config.NrtDisabled then "" else "[System.Diagnostics.CodeAnalysis.DisallowNull] ") (toPascalCase name)
        fprintfn writer "        {"
        fprintfn writer "            switch (value)"
        fprintfn writer "            {"
        members
        |> List.iter
            (
                fun x ->
                    match x with
                    | EnumMemberEntity(_, eName, _, _) ->
                        fprintfn writer  "                case %s.%s: System.Text.Json.JsonSerializer.Serialize<string>(writer, \"%s\", options); return;" (toPascalCase name) (toPascalCase eName) eName
                    | _ -> ()
            )
        fprintfn writer "                default: break;"
        fprintfn writer "            }"
        fprintfn writer "            throw new System.NotSupportedException(\"Cannot marshal type %s\");" (toPascalCase name)
        fprintfn writer "        }"
        fprintfn writer "    }"
    | _ -> ()

let printEntity (writer: System.IO.TextWriter) (config: Config) (references: string list) (entity: Entity) = 
    let printHeader ns comment =
        fprintfn writer "namespace %s\n{" (toPascalCase ns)
        references
        |> List.where(fun x -> x <> ns)
        |> List.iter(fun x -> fprintfn writer "    using %s;" (toPascalCase x))
        fprintfn writer  ""
        if (comment <> "") then fprintfn writer "%s" (arrangeComment comment 4) else ()

    let printName eType modifiers name exts tps = 
        fprintfn writer "    %s%s%s %s%s%s\n    {"
            (
                match modifiers with
                | [] -> ""
                | _ -> modifiers |> List.reduce (fun a b -> a + b + " ")
            )
            (if List.isEmpty modifiers then "" else " ")
            eType (toPascalCase name)
            (
                match tps with
                | [] -> ""
                | paras -> 
                    "<" + System.String.Join(", ", paras |> List.collect(fun x -> 
                        match x with | TypeParameterEntity(_, tpName) -> [toPascalCase tpName] | _ -> [])
                    ) + ">"
            )
            (
                match exts with
                | [] -> ""
                | paras -> " : " + System.String.Join(", ", paras |> List.map (arrangeType config writer))
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

    let isValueType (typeInfo: Entity) =
        let valueTypes = ["double"; "bool"; "ulong"; "uint"; "ushort"; "byte"; "long"; "int"; "short"; "char"; "System.DateTime"; "System.ValueTuple"]
        match typeInfo with
        | TypeEntity(_, name, _, _, _, _) -> valueTypes |> List.contains name
        | _ -> false

    let printConstructor name comment modifiers paras config =
        if (comment <> "") then fprintfn writer "%s" (arrangeComment comment 8) else ()
        fprintfn writer "        %s%s(%s) => throw new System.NotImplementedException();"
            (
                if List.isEmpty modifiers then "public "
                else System.String.Join(" ", modifiers) + " "
            )
            (toPascalCase name)
            (arrangeParameterList config writer paras)
        fprintfn writer ""

    let printProperty name comment modifiers eType withGet withSet isOptional initValue isInInterface config =
        if (comment <> "") then fprintfn writer "%s" (arrangeComment comment 8) else ()
        if config.UseSystemJson then fprintfn writer "        [System.Text.Json.Serialization.JsonPropertyName(\"%s\")]" name
        else fprintfn writer "        [Newtonsoft.Json.JsonProperty(\"%s\", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]" name
        fprintfn writer "        %s%s%s %s { %s%s}%s"
            (
                if List.isEmpty modifiers then 
                    if isInInterface then "" else "public "
                else System.String.Join(" ", modifiers) + " "
            )
            (arrangeType config writer eType)
            (
                if isOptional then
                    if config.NrtDisabled then 
                        if isValueType eType then "?"
                        else ""
                    else "?"
                else ""
            )
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

    let printEvent name comment modifiers isOptional eType isInInterface = 
        if (comment <> "") then fprintfn writer "%s" (arrangeComment comment 8) else ()
        fprintfn writer "        %sevent %s%s %s;"
            (
                if List.isEmpty modifiers then 
                    if isInInterface then "" else "public "
                else System.String.Join(" ", modifiers) + " "
            )
            (arrangeType config writer eType)
            (if isOptional then "?" else "")
            (toPascalCase name)
        fprintfn writer ""

    let printMethod name comment modifiers tps paras mType isInInterface = 
        if (comment <> "") then fprintfn writer "%s" (arrangeComment comment 8) else ()
        fprintfn writer "        %s%s %s%s(%s)%s;"
            (
                if List.isEmpty modifiers then 
                    if isInInterface then "" else "public "
                else System.String.Join(" ", modifiers) + " "
            )
            (arrangeType config writer mType)
            (toPascalCase name)
            (
                match tps with
                | [] -> ""
                | tps -> "<" + System.String.Join(", ", tps |> List.collect (fun x -> 
                    match x with | TypeParameterEntity(_, tpName) -> [toPascalCase tpName] | _ -> [])) + ">"
            )
            (arrangeParameterList config writer paras)
            (
                if isInInterface then ""
                else " => throw new System.NotImplementedException()"
            )
        fprintfn writer ""

    match entity with
    | EnumEntity(_, ns, name, comment, modifiers, members) ->
        printHeader ns comment
        printName "enum" modifiers name [] []
        members |> List.iteri
            (
                fun i x ->
                    match x with
                    | EnumMemberEntity(_, eName, eComment, eValue) -> printEnumMember eName eComment eValue (i = members.Length - 1)
                    | _ -> ()
            )
        fprintfn writer "    }"
        fprintfn writer "}\n"
    | StringUnionEntity(_, ns, name, comment, modifiers, members) ->
        printHeader ns comment
        if config.UseSystemJson then fprintfn writer "    [System.Text.Json.Serialization.JsonConverter(typeof(%s%s))]" (toPascalCase name) "Converter"
        else fprintfn writer "    [Newtonsoft.Json.JsonConverter(typeof(%s%s))]" (toPascalCase name) "Converter"
        printName "enum" modifiers name [] []
        members |> List.iteri
            (
                fun i x ->
                    match x with
                    | EnumMemberEntity(_, eName, eComment, eValue) -> printEnumMember eName eComment eValue (i = members.Length - 1)
                    | _ -> ()
            )
        fprintfn writer "    }"
        fprintfn writer ""
        if config.UseSystemJson then printSystemJsonConverter writer entity config
        else printNewtonsoftJsonConverter writer entity config
        fprintfn writer "}\n"
    | ClassInterfaceEntity(_, ns, name, comment, modifiers, members, exts, tps, isInterface) ->
        printHeader ns comment
        if isInterface then printName "interface" modifiers name exts tps
        else printName "class" modifiers name exts tps
        members
        |> List.iter
            (
                fun x ->
                    match x with
                    | ConstructorEntity(_, cName, cComment, cModifier, cParas) ->
                        printConstructor cName cComment cModifier cParas config
                    | PropertyEntity(_, pName, pComment, pModifier, pType, withGet, withSet, isOptional, initValue) ->
                        printProperty pName pComment pModifier pType withGet withSet isOptional initValue isInterface config
                    | EventEntity(_, eName, eComment, eModifier, isOptional, eType) ->
                        printEvent eName eComment eModifier isOptional eType isInterface
                    | MethodEntity(_, mName, mComment, mModifier, mTps, mParas, mType) ->
                        printMethod mName mComment mModifier mTps mParas mType isInterface
                    | _ -> ()
            )
        fprintfn writer "    }"
        fprintfn writer "}\n"
    | _ -> ()

let printEntities (splitFile: bool) (output: string) (config: Config) (entities: Entity list) (namespaces: string list) = 
    deferredEntities <- Set.empty
    if (splitFile) then
        entities 
        |> List.iter
            (
                fun x ->
                    let info = Helpers.getNamespaceAndName x
                    match info with
                    | Some(ns, name) -> 
                        let path = System.IO.Path.Combine([output]@((toPascalCase ns).Split(".") |> List.ofArray) |> Array.ofList)
                        if not (System.IO.Directory.Exists path) 
                        then System.IO.Directory.CreateDirectory path |> ignore
                        else ()
                        use file = new System.IO.FileStream(System.IO.Path.Combine(path, toPascalCase name + ".cs"), System.IO.FileMode.OpenOrCreate)
                        file.Seek(int64 0, System.IO.SeekOrigin.End) |> ignore
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
        use file = new System.IO.FileStream(path, System.IO.FileMode.OpenOrCreate)
        file.Seek(int64 0, System.IO.SeekOrigin.End) |> ignore
        use textWriter = new System.IO.StreamWriter(file)
        entities 
        |> List.iter(printEntity textWriter config namespaces)

    deferredEntities
module Printer

open Entity
open Helpers

let append spliter accu next = accu + spliter + next

let arrangeComment (comment: string) blankSpace = 
    let blanks = 
        seq {
            for _ = 1 to blankSpace do yield " "
        } |> Seq.reduce (append "")
    comment.Split("\n")
    |> Array.map(fun x -> blanks + x)
    |> Array.reduce (append "\n")

let rec arrangeType (typeInfo: EntityBodyType) =
    let pascalizeTypeName name =
        match name with
        | "object" | "string" | "double" | "void" | "bool" | "ulong"
        | "uint" | "ushort" | "byte" | "long" | "int" | "short" | "char" -> name
        | _ -> toPascalCase name
    match typeInfo.Type with
    | "System.Array" -> 
        (match typeInfo.InnerTypes with
        | [x] -> arrangeType x
        | _ -> "object") + "[]"
    | "System.ValueTuple" ->
        "(" + System.String.Join(", ", typeInfo.InnerTypes |> List.map arrangeType) + ")"
    | _ -> 
        match typeInfo.InnerTypes with
        | [] -> pascalizeTypeName typeInfo.Type
        | types -> pascalizeTypeName typeInfo.Type + "<" + System.String.Join(", ", types |> List.map arrangeType) + ">"

let printConverter (writer: System.IO.TextWriter) (entity: Entity) = 
    fprintfn writer "    class %s%s" (toPascalCase entity.Name) "Converter : Newtonsoft.Json.JsonConverter"
    fprintfn writer "    {"
    fprintfn writer "        public override bool CanConvert(System.Type t) => t == typeof(%s) || t == typeof(%s?);" (toPascalCase entity.Name) (toPascalCase entity.Name)
    fprintfn writer "        public override object ReadJson(Newtonsoft.Json.JsonReader reader, System.Type t, object? existingValue, Newtonsoft.Json.JsonSerializer serializer)"
    fprintfn writer "            => reader.TokenType switch"
    fprintfn writer "            {"
    fprintfn writer "                Newtonsoft.Json.JsonToken.String =>"
    fprintfn writer "                    serializer.Deserialize<string>(reader) switch"
    fprintfn writer "                    {"
    entity.Enums
    |> List.iter
        (
            fun x ->
                fprintfn writer "                        \"%s\" => %s.%s," x.Name (toPascalCase entity.Name) (toPascalCase x.Name)
        )
    fprintfn writer "                        _ => throw new System.Exception(\"Cannot unmarshal type %s\")" (toPascalCase entity.Name)
    fprintfn writer "                    },"
    fprintfn writer "                _ => throw new System.Exception(\"Cannot unmarshal type %s\")" (toPascalCase entity.Name)
    fprintfn writer "            };"
    fprintfn writer "        public override void WriteJson(Newtonsoft.Json.JsonWriter writer, object? untypedValue, Newtonsoft.Json.JsonSerializer serializer)"
    fprintfn writer "        {"
    fprintfn writer "            if (untypedValue is null) { serializer.Serialize(writer, null); return; }"
    fprintfn writer "            var value = (%s)untypedValue;" (toPascalCase entity.Name)
    fprintfn writer "            switch (value)"
    fprintfn writer "            {"
    entity.Enums
    |> List.iter
        (
            fun x ->
                fprintf writer  "                case %s.%s: serializer.Serialize(writer, \"%s\"); return;" (toPascalCase entity.Name) (toPascalCase x.Name) x.Name
        )
    fprintfn writer "                default: break;"
    fprintfn writer "            }"
    fprintfn writer "            throw new System.Exception(\"Cannot marshal type %s\");" (toPascalCase entity.Name)
    fprintfn writer "        }"
    fprintfn writer "    }"

let printEntity (writer: System.IO.TextWriter) (references: string list) (entity: Entity) = 
    let thisNamespace = toPascalCase entity.Namespace
    fprintfn writer "namespace %s\n{" thisNamespace
    references
    |> List.where(fun x -> x <> thisNamespace)
    |> List.iter(fun x -> fprintfn writer "    using %s;" (x))
    fprintfn writer  ""
    
    if entity.Type = EntityType.StringEnum then fprintfn writer "    [Newtonsoft.Json.JsonConverter(typeof(%s%s))]" (toPascalCase entity.Name) "Converter" else ()

    if (entity.Comment <> "") then fprintfn writer "%s" (arrangeComment entity.Comment 4) else ()
    fprintfn writer "    %s%s%s %s%s%s\n    {" 
        (
            match entity.Modifier with
            | [] -> ""
            | modifier -> modifier |> List.reduce (fun a b -> a + b + " ")
        )
        (if entity.Modifier = [] then "" else " ")
        (
            match entity.Type with
            | EntityType.Class -> "class"
            | EntityType.Interface -> "interface"
            | EntityType.Enum | EntityType.StringEnum -> "enum"
        )
        (toPascalCase entity.Name)
        (
            match entity.TypeParameter with
            | [] -> ""
            | paras -> "<" + System.String.Join(", ", paras |> List.map toPascalCase) + ">"
        )
        (
            match entity.InheritedFrom with
            | [] -> ""
            | paras -> " : " + System.String.Join(", ", paras |> List.map arrangeType)
        )

    entity.Enums
    |> List.iteri
        (
            fun i x ->
                if x.Comment <> "" then fprintfn writer "%s" (arrangeComment x.Comment 8) else ()
                fprintfn writer "        [Newtonsoft.Json.JsonProperty(\"%s\", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]" x.Name
                fprintfn writer "        %s%s%s"
                    (toPascalCase x.Name)
                    (
                        match x.Value with
                        | None -> ""
                        | Some value -> " = " + string value
                    )
                    (if i = entity.Enums.Length - 1 then "" else ",")

        )

    entity.Properties
    |> List.iter
        (
            fun x ->
                if (x.Comment <> "") then fprintfn writer "%s" (arrangeComment x.Comment 8) else ()
                fprintfn writer "        [Newtonsoft.Json.JsonProperty(\"%s\", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]" x.Name
                fprintfn writer "        %s%s%s %s { %s%s}%s"
                    (if x.Modifier = [] then "" else System.String.Join(" ", x.Modifier) + " ")
                    (arrangeType x.Type)
                    (if x.IsOptional then "?" else "")
                    (toPascalCase x.Name)
                    (
                        if entity.Type = EntityType.Interface 
                        then
                            if x.WithGet then "get; " else ""
                        else
                            if x.WithGet then "get => throw new System.NotImplementedException(); " else ""
                    )
                    (
                        if entity.Type = EntityType.Interface 
                        then
                            if x.WithGet then "set; " else ""
                        else
                            if x.WithGet then "set => throw new System.NotImplementedException(); " else ""
                    )
                    (
                        match x.InitialValue with
                        | None -> ""
                        | Some value -> " = " + string value + ";"
                    )
        )

    entity.Events
    |> List.iter
        (
            fun x ->
                if (x.Comment <> "") then fprintfn writer "%s" (arrangeComment x.Comment 8) else ()
                fprintfn writer "        %sevent %s%s %s;"
                    (if x.Modifier = [] then "" else System.String.Join(" ", x.Modifier) + " ")
                    (arrangeType x.DelegateType)
                    (if x.IsOptional then "?" else "")
                    (toPascalCase x.Name)
        )

    entity.Methods
    |> List.iter
        (
            fun x ->
                if (x.Comment <> "") then fprintfn writer "%s" (arrangeComment x.Comment 8) else ()
                fprintfn writer "        %s%s %s%s(%s)%s;"
                    (if x.Modifier = [] then "" else System.String.Join(" ", x.Modifier) + " ")
                    (arrangeType x.Type)
                    (toPascalCase x.Name)
                    (
                        match x.TypeParameter with
                        | [] -> ""
                        | tps -> "<" + System.String.Join(", ", tps |> List.map toPascalCase) + ">"
                    )
                    (
                        let mutable args = []
                        let conflictNames = ["abstract"; "as"; "base"; "bool"; "break"; "byte"; "case"; "catch"; "char"; "checked"; "class"; "const"; "continue"; "decimal"; "default"; "delegate"; "do"; "double"; "else"; "enum"; "event"; "explicit"; "extern"; "false"; "finally"; "fixed"; "float"; "for"; "foreach"; "goto"; "if"; "implicit"; "in"; "int"; "interface"; "internal"; "is"; "lock"; "long"; "namespace"; "new"; "null"; "object"; "operator"; "out"; "override"; "params"; "private"; "protected"; "public"; "readonly"; "ref"; "return"; "sbyte"; "sealed"; "short"; "sizeof"; "stackalloc"; "static"; "string"; "struct"; "switch"; "this"; "throw"; "true"; "try"; "typeof"; "uint"; "ulong"; "unchecked"; "unsafe"; "ushort"; "using"; "virtual"; "void"; "volatile"; "while";]
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
                            x.Parameter |> List.map 
                                (fun i -> 
                                    (arrangeType i) + " " + 
                                        (
                                            match i.Name with
                                            | Some name -> getArg name 0
                                            | _ -> getArg "arg" 0
                                        )
                                )
                        )
                    )
                    (
                        if entity.Type = EntityType.Interface then ""
                        else " => throw new System.NotImplementedException()"
                    )
        )

    fprintfn writer "    }"
    
    if entity.Type = EntityType.StringEnum 
    then 
        fprintf writer ""
        printConverter writer entity 
    else ()
    fprintfn writer "}\n"

let printEntities (splitFile: bool) (output: string) (entities: Entity list) = 
    let namespaces = 
        entities 
        |> List.map(fun x -> x.Namespace) 
        |> List.distinct 
        |> List.map toPascalCase
    if (splitFile) then
        entities 
        |> List.iter
            (
                fun x ->
                    let path = System.IO.Path.Combine([output]@((toPascalCase x.Namespace).Split(".") |> List.ofArray) |> Array.ofList)
                    if not (System.IO.Directory.Exists path) 
                    then System.IO.Directory.CreateDirectory path |> ignore
                    else ()
                    use file = new System.IO.FileStream(System.IO.Path.Combine(path, toPascalCase x.Name + ".cs"), System.IO.FileMode.Create)
                    use textWriter = new System.IO.StreamWriter(file)
                    printEntity textWriter namespaces x
            )
    else
        let path = output
        let dir = System.IO.Path.GetDirectoryName path
        if not (System.IO.Directory.Exists dir) 
        then System.IO.Directory.CreateDirectory dir |> ignore
        else ()
        use file = new System.IO.FileStream(path, System.IO.FileMode.Create)
        use textWriter = new System.IO.StreamWriter(file)
        entities 
        |> List.iter(printEntity textWriter namespaces)
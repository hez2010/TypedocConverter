module Printer

open Entity
open Helpers
open Definitions

let mutable deferredEntities : Entity Set = Set.empty
let mutable printedFiles : string Set = Set.empty
let mutable unionTypes : string List Set = Set.empty

let append spliter accu next = accu + spliter + next

let arrangeComment (comment: string) blankSpace = 
    let blanks = 
        seq {
            for _ = 1 to blankSpace do yield " "
        } |> Seq.reduce (append "")
    comment.Split("\n")
    |> Array.map(fun x -> blanks + x)
    |> Array.reduce (append "\n")

let escapeTypeSegment (t: string) =
    t.Split([|"."; ","; "<"; ">"; " " |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> x.Substring(0, 1).ToUpper() + x.Substring 1)
    |> Array.reduce (( + ))

let getUnionTypeFieldName (t: string) = 
    let escapedName = escapeTypeSegment t
    "_" + escapedName.Substring(0, 1).ToLowerInvariant() + escapedName.Substring 1 + "Value"
let getUnionTypePropName (t: string) = escapeTypeSegment t + "Value"
let rec getUnionTypeName (names: string list) =
    match names with
    | head::tails -> escapeTypeSegment (head.Split "." |> Array.last) + getUnionTypeName tails
    | _ -> "Union"


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
                        ), [], [], true, None
                    )
                deferredEntities <- Set.add entity deferredEntities
                "TypedocConverter.GeneratedTypes." + name
        | UnionTypeEntity _ -> arrangeUnionType config writer typeInfo
        | _ -> "object"
    match typeInfo with
    | TypeEntity(_, _, _, _, _, Some(annotatedName)) -> arrangedType + " " + (pascalizeTypeName annotatedName)
    | _ -> arrangedType
and arrangeUnionType (config: Config) (writer: System.IO.TextWriter) (typeInfo: Entity) = 
    match typeInfo with
    | UnionTypeEntity(_, _, inner, _) ->
        let types = inner |> List.map (arrangeType config writer) |> List.distinct
        let objRemoved = types |> List.where(fun x -> x <> "object")
        match objRemoved with
        | [] -> "object"
        | [x] -> x
        | _ ->
            let typeNameList = inner
                            |> List.sortWith typeSorter
                            |> List.map (arrangeType config writer)
            unionTypes <- unionTypes |> Set.add typeNameList
            let name = "TypedocConverter.GeneratedTypes." + (typeNameList |> getUnionTypeName)
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

let arrangeParameterList config (writer: System.IO.TextWriter) withName paras = 
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
                    if withName then [(arrangeType config writer pType) + " " + getArg pName 0]
                    else [arrangeType config writer pType]
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
            (arrangeParameterList config writer true paras)
        fprintfn writer ""

    let printProperty name comment modifiers eType withGet withSet isOptional initValue isInInterface config =
        if (comment <> "") then fprintfn writer "%s" (arrangeComment comment 8) else ()
        let isPrivate = modifiers |> List.contains "private"
        if isPrivate then ()
        else
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
            (
                if isPrivate then 
                    if name.StartsWith("#") then "_" + name.Substring(1)
                    else name
                else toPascalCase name
            )
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
            (arrangeParameterList config writer true paras)
            (
                if isInInterface then ""
                else " => throw new System.NotImplementedException()"
            )
        fprintfn writer ""

    let printIndexer comment (modifiers: string list) mType (paras: Entity list) = 
        if (comment <> "") then fprintfn writer "%s" (arrangeComment comment 8) else ()
        fprintfn writer "        %s%s this[%s] { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }"
            (System.String.Join(" ", modifiers) + " ")
            (arrangeType config writer mType)
            (arrangeParameterList config writer false paras)

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
    | ClassInterfaceEntity(_, ns, name, comment, modifiers, members, exts, tps, isInterface, indexer) ->
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
        match indexer with
        | Some(IndexerEntity(_, iComment, iModifier, iType, iParas)) -> printIndexer iComment iModifier iType iParas
        | _ -> ()
        fprintfn writer "    }"
        fprintfn writer "}\n"
    | _ -> ()

let printUnionTypeSystemJsonConverter (writer: System.IO.TextWriter) (unionType: string list) = 
    let name = getUnionTypeName unionType

    fprintfn writer "    class %sJsonConverter : System.Text.Json.Serialization.JsonConverter<%s>" name name
    fprintfn writer "    {"
    fprintfn writer "        public override %s Read(ref System.Text.Json.Utf8JsonReader reader, System.Type type, System.Text.Json.JsonSerializerOptions options)" name
    fprintfn writer "        {"
    unionType |> List.iter (fun t -> fprintfn writer "            try { return new %s { %s = System.Text.Json.JsonSerializer.Deserialize<%s>(ref reader, options) }; } catch (System.Text.Json.JsonException) { }" name (getUnionTypePropName t) t)
    fprintfn writer "            return default;"
    fprintfn writer "        }"
    fprintfn writer "        public override void Write(System.Text.Json.Utf8JsonWriter writer, %s value, System.Text.Json.JsonSerializerOptions options)" name
    fprintfn writer "        {"
    unionType |> List.iter (fun t -> fprintfn writer "            if (value.Type == typeof(%s)) { System.Text.Json.JsonSerializer.Serialize(writer, value.%s, options); return; }" t (getUnionTypePropName t))
    fprintfn writer "            writer.WriteNullValue();"
    fprintfn writer "        }"
    fprintfn writer "    }"

let printUnionTypeNewtonsoftJsonConverter (writer: System.IO.TextWriter) (unionType: string list) = 
    let name = getUnionTypeName unionType

    fprintfn writer "    class %sJsonConverter : Newtonsoft.Json.JsonConverter<%s>" name name
    fprintfn writer "    {"
    fprintfn writer "        public override %s ReadJson(Newtonsoft.Json.JsonReader reader, System.Type type, %s value, bool hasExistingValue, Newtonsoft.Json.JsonSerializer serializer)" name name
    fprintfn writer "        {"
    unionType |> List.iter (fun t -> fprintfn writer "            try { return new %s { %s = serializer.Deserialize<%s>(reader) }; } catch (Newtonsoft.Json.JsonException) { }" name (getUnionTypePropName t) t)
    fprintfn writer "            return default;"
    fprintfn writer "        }"
    fprintfn writer "        public override void WriteJson(Newtonsoft.Json.JsonWriter writer, %s value, Newtonsoft.Json.JsonSerializer serializer)" name
    fprintfn writer "        {"
    unionType |> List.iter (fun t -> fprintfn writer "            if (value.Type == typeof(%s)) { serializer.Serialize(writer, value.%s); return; }" t (getUnionTypePropName t))
    fprintfn writer "            writer.WriteNull();"
    fprintfn writer "        }"
    fprintfn writer "    }"

let printUnionType (writer: System.IO.TextWriter) (config: Config) (references: string list) (unionType: string list) = 
    fprintfn writer "namespace TypedocConverter.GeneratedTypes"
    fprintfn writer "{"
    references |> List.iter (fun x -> fprintfn writer "    using %s;" x)
    let name = getUnionTypeName unionType
    let typeMark = if config.NrtDisabled then "" else "?"
    if config.UseSystemJson then
        printUnionTypeSystemJsonConverter writer unionType
        fprintfn writer "    [System.Text.Json.Serialization.JsonConverter(typeof(%sJsonConverter))]" name
    else
        printUnionTypeNewtonsoftJsonConverter writer unionType
        fprintfn writer "    [Newtonsoft.Json.JsonConverter(typeof(%sJsonConverter))]" name
    fprintfn writer "    struct %s" name
    fprintfn writer "    {"
    fprintfn writer "        public System.Type%s Type { get; set; }" typeMark
    unionType
    |> List.iter 
        (
            fun t -> 
                fprintfn writer "        private %s%s %s;" t typeMark (getUnionTypeFieldName t)
                fprintfn writer "        public %s%s %s" t typeMark (getUnionTypePropName t)
                fprintfn writer "        {"
                fprintfn writer "            get => %s;" (getUnionTypeFieldName t)
                fprintfn writer "            set"
                fprintfn writer "            {"
                fprintfn writer "                ClearValue();"
                fprintfn writer "                %s = value;" (getUnionTypeFieldName t)
                fprintfn writer "                Type = typeof(%s);" t
                fprintfn writer "            }"
                fprintfn writer "        }"
                fprintfn writer "        public static implicit operator %s(%s value) => new %s { %s = value };" name t name (getUnionTypePropName t)
                fprintfn writer "        public static implicit operator %s%s(%s value) => value.%s;" t typeMark name (getUnionTypePropName t)
                fprintfn writer ""
        )

    fprintfn writer "        public override string%s ToString()" typeMark
    fprintfn writer "        {"
    unionType |> List.iter (fun t -> fprintfn writer "            if (Type == typeof(%s)) return %s%s.ToString();" t (getUnionTypePropName t) typeMark)
    fprintfn writer "            return default;"
    fprintfn writer "        }"
    
    fprintfn writer "        public override int GetHashCode()"
    fprintfn writer "        {"
    unionType |> List.iter (fun t -> fprintfn writer "            if (Type == typeof(%s)) return %s%s.GetHashCode() ?? 0;" t (getUnionTypePropName t) typeMark)
    fprintfn writer "            return 0;"
    fprintfn writer "        }"

    fprintfn writer "        private void ClearValue()"
    fprintfn writer "        {"
    unionType |> List.iter (fun t -> fprintfn writer "            %s = default;" (getUnionTypeFieldName t))
    fprintfn writer "        }"

    fprintfn writer "    }"
    fprintfn writer "}"

let truncateFileName (fileName: string) = 
    if fileName.Length > 200 then $"{fileName.Substring(0, 200)}_{fileName.GetHashCode():X}"
    else fileName

let printUnionTypes (config: Config) (namespaces: string list) (unionTypes: string list Set) =
    if config.SplitFiles then
        unionTypes 
        |> Set.iter
            (
                fun x ->
                    let ns = "TypedocConverter.GeneratedTypes"
                    let name = getUnionTypeName x
                    let path = System.IO.Path.Combine([config.OutputDir]@((toPascalCase ns).Split(".") |> List.ofArray) |> Array.ofList)
                    if not (System.IO.Directory.Exists path) 
                    then System.IO.Directory.CreateDirectory path |> ignore
                    else ()
                    let fileName = System.IO.Path.Combine(path, truncateFileName (toPascalCase name) + ".cs")
                    if not (printedFiles |> Set.contains fileName) then 
                        System.IO.File.Delete fileName
                        printedFiles <- Set.add fileName printedFiles
                    else ()
                    use file = new System.IO.FileStream(fileName, System.IO.FileMode.OpenOrCreate)
                    file.Seek(int64 0, System.IO.SeekOrigin.End) |> ignore
                    use textWriter = new System.IO.StreamWriter(file)
                    printUnionType textWriter config namespaces x
            )
    else
        let path = config.OutputFile
        let dir = System.IO.Path.GetDirectoryName path
        if dir <> "" && not (System.IO.Directory.Exists dir) 
        then System.IO.Directory.CreateDirectory dir |> ignore
        else ()
        if not (printedFiles |> Set.contains path) then 
            System.IO.File.Delete path
            printedFiles <- Set.add path printedFiles
        else ()
        use file = new System.IO.FileStream(path, System.IO.FileMode.OpenOrCreate)
        file.Seek(int64 0, System.IO.SeekOrigin.End) |> ignore
        use textWriter = new System.IO.StreamWriter(file)
        unionTypes |> Set.iter(printUnionType textWriter config namespaces)

let printEntities (config: Config) (entities: Entity list) (namespaces: string list) = 
    deferredEntities <- Set.empty
    unionTypes <- Set.empty
    if config.SplitFiles then
        entities 
        |> List.iter
            (
                fun x ->
                    let info = Helpers.getNamespaceAndName x
                    match info with
                    | Some(ns, name) -> 
                        let path = System.IO.Path.Combine([config.OutputDir]@((toPascalCase ns).Split(".") |> List.ofArray) |> Array.ofList)
                        if not (System.IO.Directory.Exists path) 
                        then System.IO.Directory.CreateDirectory path |> ignore
                        else ()
                        let fileName = System.IO.Path.Combine(path, truncateFileName (toPascalCase name) + ".cs")
                        if not (printedFiles |> Set.contains fileName) then 
                            System.IO.File.Delete fileName
                            printedFiles <- Set.add fileName printedFiles
                        else ()
                        use file = new System.IO.FileStream(fileName, System.IO.FileMode.OpenOrCreate)
                        file.Seek(int64 0, System.IO.SeekOrigin.End) |> ignore
                        use textWriter = new System.IO.StreamWriter(file)
                        printEntity textWriter config namespaces x
                    | _ -> ()
            )
    else
        let path = config.OutputFile
        let dir = System.IO.Path.GetDirectoryName path
        if dir <> "" && not (System.IO.Directory.Exists dir) 
        then System.IO.Directory.CreateDirectory dir |> ignore
        else ()
        if not (printedFiles |> Set.contains path) then 
            System.IO.File.Delete path
            printedFiles <- Set.add path printedFiles
        else ()
        use file = new System.IO.FileStream(path, System.IO.FileMode.OpenOrCreate)
        file.Seek(int64 0, System.IO.SeekOrigin.End) |> ignore
        use textWriter = new System.IO.StreamWriter(file)
        entities |> List.iter(printEntity textWriter config namespaces)

    (deferredEntities, unionTypes)
module Helpers

open Definitions
open Entity

let getNamespaceAndName entity =
    match entity with
    | ClassInterfaceEntity(_, ns, name, _, _, _, _, _, _) -> Some (ns, name)
    | EnumEntity(_, ns, name, _, _, _) -> Some (ns, name)
    | _ -> None

let printWarning (warn: string) = 
    let backup = System.Console.ForegroundColor
    System.Console.ForegroundColor <- System.ConsoleColor.Yellow
    System.Console.Error.WriteLine ("[Warning] " + warn)
    System.Console.ForegroundColor <- backup
    
let printError (err: string) = 
    let backup = System.Console.ForegroundColor
    System.Console.ForegroundColor <- System.ConsoleColor.Red
    System.Console.Error.WriteLine ("[Error] " + err)
    System.Console.ForegroundColor <- backup

let rec toPascalCase (str: string) =
    if str.Length = 0
    then str
    else if str.Contains "." then 
         str.Split "." |> Array.map toPascalCase |> Array.reduce (fun a n -> a + "." + n)
         else 
            str.Split([| "-"; "_" |], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun x -> x.Substring(0, 1).ToUpper() + x.Substring 1)
            |> Array.reduce (fun accu next -> accu + next)

let escapeSymbols (text: string) = 
    if isNull text then ""
    else text
            .Replace("&", "&amp;")
            .Replace("<", "&lt;")
            .Replace(">", "&gt;")

let toCommentText (text: string) = 
    if isNull text then ""
    else text.Split "\n" |> Array.map (fun t -> "/// " + escapeSymbols t) |> Array.reduce(fun accu next -> accu + "\n" + next)

let getXmlDocComment (comment: Comment) =
    let prefix = "/// <summary>\n"
    let suffix = "\n/// </summary>"
    let summary = 
        match comment.Text with
        | Some text -> prefix + toCommentText comment.ShortText + toCommentText text + suffix
        | _ -> 
            match comment.ShortText with
            | "" -> ""
            | _ -> prefix + toCommentText comment.ShortText + suffix
    let returns = 
        match comment.Returns with
        | Some text -> "\n/// <returns>\n" + toCommentText text + "\n/// </returns>"
        | _ -> ""
    summary + returns

let getCommentFromSignature (node: Reflection) =
    let signature = 
        match node.Comment with
        | Some comment -> [getXmlDocComment comment]
        | _ -> []
    match signature with
    | [] -> ""
    | _ -> signature |> List.reduce(fun accu next -> accu + "\n" + next)

let getComment (node: Reflection) =
    match node.Comment with
    | Some comment -> getXmlDocComment comment
    | _ -> ""

let rec getType (config: Config) (annotatedName: string option) (typeInfo: Type): Entity = 
    let containerType =
        match typeInfo.Type with
        | "literal" ->
            TypeEntity(typeInfo.Id, "string", typeInfo.Type, [], Plain, annotatedName)
        | "intrinsic" -> 
            match typeInfo.Name with
            | Some name -> 
                match name with
                | "number" -> TypeEntity(typeInfo.Id, config.NumberType, typeInfo.Type, [], Plain, annotatedName)
                | "boolean" -> TypeEntity(typeInfo.Id, "bool", typeInfo.Type, [], Plain, annotatedName)
                | "string" -> TypeEntity(typeInfo.Id, "string", typeInfo.Type, [], Plain, annotatedName)
                | "void" -> TypeEntity(typeInfo.Id, "void", typeInfo.Type, [], Plain, annotatedName)
                | "any" -> TypeEntity(typeInfo.Id, config.AnyType, typeInfo.Type, [], Plain, annotatedName)
                | _ -> TypeEntity(typeInfo.Id, "object", typeInfo.Type, [], Plain, annotatedName)
            | _ -> TypeEntity(typeInfo.Id, "object", typeInfo.Type, [], Plain, annotatedName)
        | "reference" | "typeParameter" -> 
            match typeInfo.Name with
            | Some name -> 
                match name with
                | "Promise" -> TypeEntity(typeInfo.Id, "System.Threading.Tasks.Task", typeInfo.Type, [], Plain, annotatedName)
                | "Set" -> TypeEntity(typeInfo.Id, "System.Collections.Generic.ISet", typeInfo.Type, [], Plain, annotatedName)
                | "Map" -> TypeEntity(typeInfo.Id, "System.Collections.Generic.IDictionary", typeInfo.Type, [], Plain, annotatedName)
                | "Array" -> TypeEntity(typeInfo.Id, "System.Array", typeInfo.Type, [], Plain, annotatedName)
                | "Date" -> TypeEntity(typeInfo.Id, "System.DateTime", typeInfo.Type, [], Plain, annotatedName)
                | "BigUint64Array" -> TypeEntity(typeInfo.Id, "System.Array", typeInfo.Type, [TypeEntity(0, "ulong", "intrinsic", [], Plain, None)], Plain, annotatedName)
                | "Uint32Array" -> TypeEntity(typeInfo.Id, "System.Array", typeInfo.Type, [TypeEntity(0, "uint", "intrinsic", [], Plain, None)], Plain, annotatedName)
                | "Uint16Array" -> TypeEntity(typeInfo.Id, "System.Array", typeInfo.Type, [TypeEntity(0, "ushort", "intrinsic", [], Plain, None)], Plain, annotatedName)
                | "Uint8Array" -> TypeEntity(typeInfo.Id, "System.Array", typeInfo.Type, [TypeEntity(0, "byte", "intrinsic", [], Plain, None)], Plain, annotatedName)
                | "BigInt64Array" -> TypeEntity(typeInfo.Id, "System.Array", typeInfo.Type, [TypeEntity(0, "long", "intrinsic", [], Plain, None)], Plain, annotatedName)
                | "Int32Array" -> TypeEntity(typeInfo.Id, "System.Array", typeInfo.Type, [TypeEntity(0, "int", "intrinsic", [], Plain, None)], Plain, annotatedName)
                | "Int16Array" -> TypeEntity(typeInfo.Id, "System.Array", typeInfo.Type, [TypeEntity(0, "short", "intrinsic", [], Plain, None)], Plain, annotatedName)
                | "Int8Array" -> TypeEntity(typeInfo.Id, "System.Array", typeInfo.Type, [TypeEntity(0, "char", "intrinsic", [], Plain, None)], Plain, annotatedName)
                | "RegExp" -> TypeEntity(typeInfo.Id, "string", typeInfo.Type, [], Plain, None);
                | x -> TypeEntity(typeInfo.Id, x, typeInfo.Type, [], Plain, None);
            | _ -> TypeEntity(typeInfo.Id, "object", typeInfo.Type, [], Plain, None)
        | "array" -> 
            match typeInfo.ElementType with
            | Some elementType -> TypeEntity(typeInfo.Id, "System.Array", typeInfo.Type, [getType config None elementType], Plain, annotatedName)
            | _ -> TypeEntity(typeInfo.Id, "System.Array", typeInfo.Type, [TypeEntity(0, "object", "intrinsic", [], Plain, None)], Plain, annotatedName)
        | "stringLiteral" -> TypeEntity(typeInfo.Id, "string", typeInfo.Type, [], Plain, annotatedName)
        | "tuple" ->
            match typeInfo.Types with
            | Some innerTypes -> 
                match innerTypes with
                | [] -> TypeEntity(typeInfo.Id, "object", typeInfo.Type, [], Plain, annotatedName)
                | _ -> TypeEntity(typeInfo.Id, "System.ValueTuple", typeInfo.Type, innerTypes |> List.map (getType config None), Plain, None)
            | _ -> TypeEntity(typeInfo.Id, "object", typeInfo.Type, [], Plain, annotatedName)
        | "union" -> 
            match typeInfo.Types with
            | Some innerTypes -> 
                match innerTypes with
                | [] -> TypeEntity(typeInfo.Id, "object", typeInfo.Type, [], Plain, annotatedName)
                | _ -> UnionTypeEntity(typeInfo.Id, typeInfo.Type, innerTypes |> List.map (getType config None), annotatedName)
            | _ -> TypeEntity(typeInfo.Id, "object", typeInfo.Type, [], Plain, annotatedName)
        | "intersection" -> 
            let types = 
                match typeInfo.Types with
                | Some innerTypes -> 
                    innerTypes 
                    |> List.map (getType config None) 
                    |> List.map (fun x ->
                        match x with 
                        | TypeEntity(_, name, _, _, _, _) -> name 
                        | UnionTypeEntity _ -> "union"
                        | _ -> "object"
                    )
                | _ -> []

            printWarning ("Intersection type " + System.String.Join(" & ", types) + " is not supported.")
            TypeEntity(typeInfo.Id, "object", typeInfo.Type, [], Plain, annotatedName) // TODO: generate intersections
        | "reflection" -> 
            match typeInfo.Declaration with
            | Some dec -> 
                match dec.Signatures with
                | Some [signature] -> 
                    let paras = 
                        match signature.Parameters with
                        | Some p -> 
                            p 
                            |> List.map
                                (fun pi -> 
                                    match pi.Type with 
                                    | Some pt -> Some (getType config None pt)
                                    | _ -> None
                                )
                            |> List.collect
                                (fun x -> 
                                    match x with
                                    | Some s -> [s]
                                    | _ -> []
                                )
                        | _ -> []
                    let rec getDelegateParas (paras: Entity list): Entity list =
                        match paras with
                        | [x] -> [x]
                        | (front::tails) -> [front] @ getDelegateParas tails
                        | _ -> []
                    let returnsType = 
                        match signature.Type with
                        | Some t -> getType config None t
                        | _ -> TypeEntity(0, "void", "intrinsic", [], Plain, None)
                    let typeParas = getDelegateParas paras
                    match typeParas with
                    | [] -> TypeEntity(typeInfo.Id, "System.Action", typeInfo.Type, [], Plain, annotatedName)
                    | _ -> 
                        match returnsType with
                        | TypeEntity(_, "void", _, _, t, _) -> TypeEntity(typeInfo.Id, "System.Action", typeInfo.Type, typeParas, t, None) 
                        | _ -> TypeEntity(typeInfo.Id, "System.Func", typeInfo.Type, typeParas @ [returnsType], Plain, None)
                | _ -> 
                    match dec.IndexSignature with
                    | Some (index) -> 
                        let indexRetType = 
                            match index.Type with
                            | None -> TypeEntity(index.Id, "object", typeInfo.Type, [], Plain, None)
                            | Some(indexType) -> getType config annotatedName indexType
                        let indexParams = 
                            match index.Parameters with
                            | None | Some([]) -> TypeEntity(index.Id, "object", typeInfo.Type, [], Plain, None)
                            | Some([param]) -> 
                                match param.Type with
                                | None -> TypeEntity(param.Id, "object", typeInfo.Type, [], Plain, None)
                                | Some(pType) -> getType config None pType
                            | Some(paramList) -> 
                                TypeEntity(index.Id, "System.ValueTuple", typeInfo.Type, 
                                    paramList
                                    |> List.map(fun p -> 
                                        match p.Type with 
                                        | None -> TypeEntity(p.Id, "object", typeInfo.Type, [], Plain, Some(p.Name))
                                        | Some(pType) -> getType config (Some(p.Name)) pType
                                    ), Plain, None)
                        TypeEntity(index.Id, "System.Collections.Generic.IDictionary", "reflection", [indexParams; indexRetType], Plain, annotatedName)
                    | _ ->
                        match dec.Children with
                        | None | Some [] -> TypeEntity(typeInfo.Id, "object", typeInfo.Type, [], Plain, None)
                        | Some children -> 
                            let typeName = "Literal" + System.String.Join("", children |> List.map(fun c -> getQualifiedName config None c))
                            // printWarning ("Type literal { " + System.String.Join(", ", children |> List.map(fun c -> c.Name)) + " } is not supported.")
                            TypeEntity(typeInfo.Id, typeName, typeInfo.Type, children |> List.collect(fun c -> match c.Type with | Some t -> [TypeLiteralElementEntity (typeInfo.Id, c.Name, getType config None t)] | _ -> []), Literal, annotatedName)
            | _ -> TypeEntity(typeInfo.Id, "object", typeInfo.Type, [], Plain, None)
        | _ -> TypeEntity(typeInfo.Id, "object", typeInfo.Type, [], Plain, None)
    handlePromiseType containerType typeInfo config annotatedName
and getQualifiedName (config: Config) (annotatedName: string option) (node: Reflection) = 
    let rec expandTypeName (entity: Entity) = 
        match entity with
        | TypeEntity(_, name, _, _, _, _) -> toPascalCase name |> String.collect (fun c -> if c = '.' then "" else c.ToString())
        | UnionTypeEntity(_, _, types, _) -> "Union" + System.String.Join("", types |> List.map expandTypeName)
        | _ -> ""
    let tName = 
        match node.Type with
        | Some nType -> expandTypeName (getType config annotatedName nType)
        | _ -> ""
    let nName = node.Name
    toPascalCase tName + toPascalCase nName
and handlePromiseType (containerType: Entity) (typeInfo: Type) (config: Config) (annotatedName: string option): Entity =
    let mutable container = containerType
    let mutable innerTypes = 
        match typeInfo.TypeArguments with
        | Some args -> getGenericTypeArguments config annotatedName args
        | _ -> []
    match container with
    | TypeEntity(id, "System.Threading.Tasks.Task", typeId, _, _, _) ->
        match innerTypes with
        | [front] | (front::_) -> 
            match front with
            | TypeEntity(_, "void", _, _, t, a) ->
                innerTypes <- []
                if config.UseWinRTPromise
                then 
                    container <- TypeEntity(id, "Windows.Foundation.IAsyncAction", typeId, [], t, a)
                else 
                    container <- TypeEntity(id, "System.Threading.Tasks.Task", typeId, [], t, a)
            | TypeEntity(_, _, _, inner, t, a) ->
                if config.UseWinRTPromise
                then 
                    container <- TypeEntity(id, "Windows.Foundation.IAsyncOperation", typeId, inner, t, a)
                else 
                    container <- TypeEntity(id, "System.Threading.Tasks.Task", typeId, inner, t, a)
            | _ -> ()
        | _ -> ()
    | UnionTypeEntity(id, typeId, inner, _) -> 
        container <- UnionTypeEntity(id, typeId, inner |> List.map(fun x -> handlePromiseType x typeInfo config None), annotatedName)
    | _ -> ()
    match container with
    | TypeEntity(id, name, typeId, inner, t, a) -> TypeEntity(id, name, typeId, (if innerTypes = [] then inner else innerTypes), t, a)
    | UnionTypeEntity(id, typeId, inner, a) -> UnionTypeEntity(id, typeId, inner, a)
    | _ -> TypeEntity(typeInfo.Id, "object", typeInfo.Type, [], Plain, None)
and getGenericTypeArguments (config: Config) (annotatedName: string option) (typeInfos: Type list): Entity list = 
    typeInfos |> List.map (getType config annotatedName)
and getGenericTypeParameters (nodes: Reflection list) = // TODO: generate constaints
    let types = 
        nodes 
        |> List.where(fun x -> x.Kind = ReflectionKind.TypeParameter)
    types |> List.map (fun x -> TypeParameterEntity(x.Id, x.Name))

let getMethodParameters (config: Config) (annotatedName: string option) (parameters: Reflection list) = 
    parameters
    |> List.where(fun x -> x.Kind = ReflectionKind.Parameter)
    |> List.map(fun x ->
        let name = if isNull x.Name then "" else x.Name
        match x.Type with
        | Some typeInfo -> 
            let typeMeta = getType config annotatedName typeInfo
            ParameterEntity(x.Id, name, typeMeta)
        | _ -> ParameterEntity(x.Id, name, TypeEntity(0, "object", "intrinsic", [], Plain, None))
    )

let getModifier (flags: ReflectionFlags) = 
    let mutable modifier = []
    match flags.IsPublic with
    | Some flag -> if flag then modifier <- modifier |> List.append [ "public" ] else ()
    | _ -> ()
    match flags.IsAbstract with
    | Some flag -> if flag then modifier <- modifier |> List.append [ "abstract" ] else ()
    | _ -> ()
    match flags.IsPrivate with
    | Some flag -> if flag then modifier <- modifier |> List.append [ "private" ] else ()
    | _ -> ()
    match flags.IsProtected with
    | Some flag -> if flag then modifier <- modifier |> List.append [ "protected" ] else ()
    | _ -> ()
    match flags.IsStatic with
    | Some flag -> if flag then modifier <- modifier |> List.append [ "static" ] else ()
    | _ -> ()
    modifier

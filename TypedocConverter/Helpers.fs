module Helpers

open Definitions
open Entity

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

let rec getType (config: Config) (typeInfo: Type): Entity = 
    let mutable containerType =
        match typeInfo.Type with
        | "intrinsic" -> 
            match typeInfo.Name with
            | Some name -> 
                match name with
                | "number" -> TypeEntity(typeInfo.Id, config.NumberType, [])
                | "boolean" -> TypeEntity(typeInfo.Id, "bool", [])
                | "string" -> TypeEntity(typeInfo.Id, "string", [])
                | "void" -> TypeEntity(typeInfo.Id, "void", [])
                | "any" -> TypeEntity(typeInfo.Id, config.AnyType, [])
                | _ -> TypeEntity(typeInfo.Id, "object", [])
            | _ -> TypeEntity(typeInfo.Id, "object", [])
        | "reference" | "typeParameter" -> 
            match typeInfo.Name with
            | Some name -> 
                match name with
                | "Promise" -> TypeEntity(typeInfo.Id, "System.Threading.Tasks.Task", [])
                | "Set" -> TypeEntity(typeInfo.Id, "System.Collections.Generic.ISet", [])
                | "Map" -> TypeEntity(typeInfo.Id, "System.Collections.Generic.IDictionary", [])
                | "Array" -> TypeEntity(typeInfo.Id, "System.Array", [])
                | "Date" -> TypeEntity(typeInfo.Id, "System.DateTime", [])
                | "BigUint64Array" -> TypeEntity(typeInfo.Id, "System.Array", [TypeEntity(0, "ulong", [])])
                | "Uint32Array" -> TypeEntity(typeInfo.Id, "System.Array", [TypeEntity(0, "uint", [])])
                | "Uint16Array" -> TypeEntity(typeInfo.Id, "System.Array", [TypeEntity(0, "ushort", [])])
                | "Uint8Array" -> TypeEntity(typeInfo.Id, "System.Array", [TypeEntity(0, "byte", [])])
                | "BigInt64Array" -> TypeEntity(typeInfo.Id, "System.Array", [TypeEntity(0, "long", [])])
                | "Int32Array" -> TypeEntity(typeInfo.Id, "System.Array", [TypeEntity(0, "int", [])])
                | "Int16Array" -> TypeEntity(typeInfo.Id, "System.Array", [TypeEntity(0, "short", [])])
                | "Int8Array" -> TypeEntity(typeInfo.Id, "System.Array", [TypeEntity(0, "char", [])])
                | "RegExp" -> TypeEntity(typeInfo.Id, "string", []);
                | x -> TypeEntity(typeInfo.Id, x, []);
            | _ -> TypeEntity(typeInfo.Id, "object", [])
        | "array" -> 
            match typeInfo.ElementType with
            | Some elementType -> TypeEntity(typeInfo.Id, "System.Array", [getType config elementType])
            | _ -> TypeEntity(typeInfo.Id, "System.Array", [TypeEntity(0, "object", [])])
        | "stringLiteral" -> TypeEntity(typeInfo.Id, "string", [])
        | "tuple" ->
            match typeInfo.Types with
            | Some innerTypes -> 
                match innerTypes with
                | [] -> TypeEntity(typeInfo.Id, "object", [])
                | _ -> TypeEntity(typeInfo.Id, "System.ValueTuple", innerTypes |> List.map (getType config))
            | _ -> TypeEntity(typeInfo.Id, "object", [])
        | "union" -> 
            match typeInfo.Types with
            | Some innerTypes -> 
                match innerTypes with
                | [] -> TypeEntity(typeInfo.Id, "object", [])
                | _ -> 
                    let takenType = 
                        innerTypes 
                        |> List.sortWith typeSorter
                        |> List.head
                    match takenType.Name with
                    | Some name -> printWarning ("Taking type " + name + " for the entire union type.")
                    | _ -> printWarning ("Taking type " + takenType.Type + " for the entire union type.")
                    getType config takenType
            | _ -> TypeEntity(typeInfo.Id, "object", [])
        | "intersection" -> 
            printWarning ("Intersection type is not supported.")
            TypeEntity(typeInfo.Id, "object", []) // TODO: generate intersections
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
                                    | Some pt -> Some (getType config pt)
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
                        | Some t -> getType config t
                        | _ -> TypeEntity(0, "void", [])
                    let typeParas = getDelegateParas paras
                    match typeParas with
                    | [] -> TypeEntity(typeInfo.Id, "System.Action", [])
                    | _ -> 
                        match returnsType with
                        | TypeEntity(_, "void", _) -> TypeEntity(typeInfo.Id, "System.Action", typeParas) 
                        | _ -> TypeEntity(typeInfo.Id, "System.Func", typeParas @ [returnsType])
                | _ -> TypeEntity(typeInfo.Id, "object", [])
            | _ -> TypeEntity(typeInfo.Id, "object", [])
        | _ -> TypeEntity(typeInfo.Id, "object", [])
    let mutable innerTypes = 
        match typeInfo.TypeArguments with
        | Some args -> getGenericTypeArguments config args
        | _ -> []
    match containerType with
    | TypeEntity(id, "System.Threading.Tasks.Task", _) ->
        match innerTypes with
        | [front] | (front::_) -> 
            match front with
            | TypeEntity(_, "void", _) ->
                innerTypes <- []
                if config.UseWinRTPromise
                then 
                    containerType <- TypeEntity(id, "Windows.Foundation.IAsyncAction", [])
                else 
                    containerType <- TypeEntity(id, "System.Threading.Tasks.Task", [])
            | TypeEntity(_, _, inner) ->
                if config.UseWinRTPromise
                then 
                    containerType <- TypeEntity(id, "Windows.Foundation.IAsyncOperation", inner)
                else 
                    containerType <- TypeEntity(id, "System.Threading.Tasks.Task", inner)
            | _ -> ()
        | _ -> ()
    | _ -> ()
    match containerType with
    | TypeEntity(id, name, inner) -> TypeEntity(id, name, if innerTypes = [] then inner else innerTypes)
    | _ -> TypeEntity(typeInfo.Id, "object", [])
and getGenericTypeArguments (config: Config) (typeInfos: Type list): Entity list = 
    typeInfos |> List.map (getType config)
and getGenericTypeParameters (nodes: Reflection list) = // TODO: generate constaints
    let types = 
        nodes 
        |> List.where(fun x -> x.Kind = ReflectionKind.TypeParameter)
    types |> List.map (fun x -> TypeParameterEntity(x.Id, x.Name))
and typeSorter typeA typeB = 
    let typesOrder = ["array"; "tuple"; "reference"; "reflection"; "stringLiteral"; "intrinsic"]
    let indexA = typesOrder |> List.tryFindIndex (fun x -> x = typeA.Type)
    let indexB = typesOrder |> List.tryFindIndex (fun x -> x = typeB.Type)
    match (indexA, indexB) with
    | (None, None) -> 0
    | (Some _, None) -> -1
    | (None, Some _) -> 1
    | (Some a, Some b) -> a.CompareTo b

let getMethodParameters (config: Config) (parameters: Reflection list) = 
    parameters
    |> List.where(fun x -> x.Kind = ReflectionKind.Parameter)
    |> List.map(fun x ->
        let name = if isNull x.Name then "" else x.Name
        match x.Type with
        | Some typeInfo -> 
            let typeMeta = getType config typeInfo
            ParameterEntity(x.Id, name, typeMeta)
        | _ -> ParameterEntity(x.Id, name, TypeEntity(0, "object", []))
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

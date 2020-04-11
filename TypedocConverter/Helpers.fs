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
                | "number" -> TypeEntity(config.NumberType, [])
                | "boolean" -> TypeEntity("bool", [])
                | "string" -> TypeEntity("string", [])
                | "void" -> TypeEntity("void", [])
                | "any" -> TypeEntity(config.AnyType, [])
                | _ -> TypeEntity("object", [])
            | _ -> TypeEntity("object", [])
        | "reference" | "typeParameter" -> 
            match typeInfo.Name with
            | Some name -> 
                match name with
                | "Promise" -> TypeEntity("System.Threading.Tasks.Task", [])
                | "Set" -> TypeEntity("System.Collections.Generic.ISet", [])
                | "Map" -> TypeEntity("System.Collections.Generic.IDictionary", [])
                | "Array" -> TypeEntity("System.Array", [])
                | "Date" -> TypeEntity("System.DateTime", [])
                | "BigUint64Array" -> TypeEntity("System.Array", [TypeEntity("ulong", [])])
                | "Uint32Array" -> TypeEntity("System.Array", [TypeEntity("uint", [])])
                | "Uint16Array" -> TypeEntity("System.Array", [TypeEntity("ushort", [])])
                | "Uint8Array" -> TypeEntity("System.Array", [TypeEntity("byte", [])])
                | "BigInt64Array" -> TypeEntity("System.Array", [TypeEntity("long", [])])
                | "Int32Array" -> TypeEntity("System.Array", [TypeEntity("int", [])])
                | "Int16Array" -> TypeEntity("System.Array", [TypeEntity("short", [])])
                | "Int8Array" -> TypeEntity("System.Array", [TypeEntity("char", [])])
                | "RegExp" -> TypeEntity("string", []);
                | x -> TypeEntity(x, []);
            | _ -> TypeEntity("object", [])
        | "array" -> 
            match typeInfo.ElementType with
            | Some elementType -> TypeEntity("System.Array", [getType config elementType])
            | _ -> TypeEntity("System.Array", [TypeEntity("object", [])])
        | "stringLiteral" -> TypeEntity("string", [])
        | "tuple" ->
            match typeInfo.Types with
            | Some innerTypes -> 
                match innerTypes with
                | [] -> TypeEntity("object", [])
                | _ -> TypeEntity("System.ValueTuple", innerTypes |> List.map (getType config))
            | _ -> TypeEntity("object", [])
        | "union" -> 
            match typeInfo.Types with
            | Some innerTypes -> 
                match innerTypes with
                | [] -> TypeEntity("object", [])
                | _ -> 
                    let takenType = 
                        innerTypes 
                        |> List.sortWith typeSorter
                        |> List.head
                    match takenType.Name with
                    | Some name -> printWarning ("Taking type " + name + " for the entire union type.")
                    | _ -> printWarning ("Taking type " + takenType.Type + " for the entire union type.")
                    getType config takenType
            | _ -> TypeEntity("object", [])
        | "intersection" -> 
            printWarning ("Intersection type is not supported.")
            TypeEntity("object", []) // TODO: generate intersections
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
                        | _ -> TypeEntity("void", [])
                    let typeParas = getDelegateParas paras
                    match typeParas with
                    | [] -> TypeEntity("System.Action", [])
                    | _ -> 
                        match returnsType with
                        | TypeEntity("void", _) -> TypeEntity("System.Action", typeParas) 
                        | _ -> TypeEntity("System.Func", typeParas @ [returnsType])
                | _ -> TypeEntity("object", [])
            | _ -> TypeEntity("object", [])
        | _ -> TypeEntity("object", [])
    let mutable innerTypes = 
        match typeInfo.TypeArguments with
        | Some args -> getGenericTypeArguments config args
        | _ -> []
    match containerType with
    | TypeEntity("System.Threading.Tasks.Task", _) ->
        match innerTypes with
        | [front] | (front::_) -> 
            match front with
            | TypeEntity("void", _) ->
                innerTypes <- []
                if config.UseWinRTPromise
                then 
                    containerType <- TypeEntity("Windows.Foundation.IAsyncAction", [])
                else 
                    containerType <- TypeEntity("System.Threading.Tasks.Task", [])
            | TypeEntity(_, inner) ->
                if config.UseWinRTPromise
                then 
                    containerType <- TypeEntity("Windows.Foundation.IAsyncOperation", inner)
                else 
                    containerType <- TypeEntity("System.Threading.Tasks.Task", inner)
            | _ -> ()
        | _ -> ()
    | _ -> ()
    match containerType with
    | TypeEntity(name, inner) -> TypeEntity(name, if innerTypes = [] then inner else innerTypes)
    | _ -> TypeEntity("object", [])
and getGenericTypeArguments (config: Config) (typeInfos: Type list): Entity list = 
    typeInfos |> List.map (getType config)
and getGenericTypeParameters (nodes: Reflection list) = // TODO: generate constaints
    let types = 
        nodes 
        |> List.where(fun x -> x.Kind = ReflectionKind.TypeParameter)
        |> List.map (fun x -> x.Name)
    types |> List.map (fun x -> TypeParameterEntity(x))
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
            ParameterEntity(name, typeMeta)
        | _ -> ParameterEntity(name, TypeEntity("object", []))
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

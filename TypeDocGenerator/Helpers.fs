module Helpers

open Definitions
open Entity

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

let rec getType (typeInfo: Type): EntityBodyType = 
    let genericType =
        match typeInfo.Type with
        | "intrinsic" -> 
            match typeInfo.Name with
            | Some name -> 
                match name with
                | "number" -> { Type = "double"; InnerTypes = []; Name = None }
                | "boolean" -> { Type = "bool"; InnerTypes = []; Name = None }
                | "string" -> { Type = "string"; InnerTypes = []; Name = None }
                | "void" -> { Type = "void"; InnerTypes = []; Name = None }
                | _ -> { Type = "object"; InnerTypes = []; Name = None }
            | _ -> { Type = "object"; InnerTypes = []; Name = None }
        | "reference" | "typeParameter" -> 
            match typeInfo.Name with
            | Some name -> 
                match name with
                | "Promise" -> { Type = "System.Threading.Tasks.Task"; InnerTypes = []; Name = None }
                | "Set" -> { Type = "System.Collections.Generic.ISet"; InnerTypes = []; Name = None }
                | "Map" -> { Type = "System.Collections.Generic.IDictionary"; InnerTypes = []; Name = None }
                | "Array" -> { Type = "System.Array"; InnerTypes = []; Name = None }
                | "BigUint64Array" -> { Type = "System.Array"; InnerTypes = [{ Type = "ulong"; InnerTypes = [ ]; Name = None };]; Name = None };
                | "Uint32Array" -> { Type = "System.Array"; InnerTypes = [{ Type = "uint"; InnerTypes = [ ]; Name = None };]; Name = None };
                | "Uint16Array" -> { Type = "System.Array"; InnerTypes = [{ Type = "ushort"; InnerTypes = [ ]; Name = None };]; Name = None };
                | "Uint8Array" -> { Type = "System.Array"; InnerTypes = [{ Type = "byte"; InnerTypes = [ ]; Name = None };]; Name = None };
                | "BigInt64Array" -> { Type = "System.Array"; InnerTypes = [{ Type = "long"; InnerTypes = [ ]; Name = None };]; Name = None };
                | "Int32Array" -> { Type = "System.Array"; InnerTypes = [{ Type = "int"; InnerTypes = [ ]; Name = None };]; Name = None };
                | "Int16Array" -> { Type = "System.Array"; InnerTypes = [{ Type = "short"; InnerTypes = [ ]; Name = None };]; Name = None };
                | "Int8Array" -> { Type = "System.Array"; InnerTypes = [{ Type = "char"; InnerTypes = [ ]; Name = None };]; Name = None };
                | "RegExp" -> { Type = "string"; InnerTypes = []; Name = None };
                | x -> { Type = x; InnerTypes = []; Name = None };
            | _ -> { Type = "object"; InnerTypes = []; Name = None }
        | "array" -> 
            match typeInfo.ElementType with
            | Some elementType -> { Type = "System.Array"; InnerTypes = [getType elementType]; Name = None }
            | _ -> { Type = "System.Array"; InnerTypes = [{ Type = "object"; InnerTypes = []; Name = None }]; Name = None }
        | "stringLiteral" -> { Type = "string"; InnerTypes = []; Name = None }
        | "tuple" ->
            match typeInfo.Types with
            | Some innerTypes -> 
                match innerTypes with
                | [] -> { Type = "object"; InnerTypes = []; Name = None }
                | _ -> { Type = "System.ValueTuple"; InnerTypes = innerTypes |> List.map getType; Name = None }
            | _ -> { Type = "object"; InnerTypes = []; Name = None }
        | "union" -> 
            match typeInfo.Types with
            | Some innerTypes -> 
                match innerTypes with
                | [] -> { Type = "object"; InnerTypes = []; Name = None }
                | _ -> getType innerTypes.[0] // TODO: generate unions
            | _ ->{ Type = "object"; InnerTypes = []; Name = None }
        | "intersection" -> { Type = "object"; InnerTypes = []; Name = None } // TODO: generate intersections
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
                                    | Some pt -> Some (getType pt)
                                    | _ -> None
                                )
                            |> List.collect
                                (fun x -> 
                                    match x with
                                    | Some s -> [s]
                                    | _ -> []
                                )
                        | _ -> []
                    let rec getDelegateParas (paras: EntityBodyType list): EntityBodyType list =
                        match paras with
                        | [x] -> [{ Type = x.Type; InnerTypes = x.InnerTypes; Name = None }]
                        | (front::tails) -> [front] @ getDelegateParas tails
                        | _ -> []
                    let returnsType = 
                        match signature.Type with
                        | Some t -> getType t
                        | _ -> { Type = "void"; InnerTypes = []; Name = None }
                    let typeParas = getDelegateParas paras
                    match typeParas with
                    | [] -> { Type = "System.Action"; InnerTypes = []; Name = None }
                    | _ -> 
                        if returnsType.Type = "void" 
                        then { Type = "System.Action"; InnerTypes = typeParas; Name = None } 
                        else { Type = "System.Func"; InnerTypes = typeParas @ [returnsType]; Name = None }
                | _ -> { Type = "object"; InnerTypes = []; Name = None }
            | _ -> { Type = "object"; InnerTypes = []; Name = None }
        | _ -> { Type = "object"; InnerTypes = []; Name = None }
    let mutable innerTypes = 
        match typeInfo.TypeArguments with
        | Some args -> getGenericTypeArguments args
        | _ -> []
    if genericType.Type = "System.Threading.Tasks.Task"
    then 
        match innerTypes with
        | (front::_) -> if front.Type = "void" then innerTypes <- [] else ()
        | _ -> ()
    else ()
    { 
        Type = genericType.Type; 
        Name = None; 
        InnerTypes = if innerTypes = [] then genericType.InnerTypes else innerTypes; 
    }
and getGenericTypeArguments (typeInfos: Type list): EntityBodyType list = 
    typeInfos |> List.map getType
and getGenericTypeParameters (nodes: Reflection list) = // TODO: generate constaints
    let types = 
        nodes 
        |> List.where(fun x -> x.Kind = ReflectionKind.TypeParameter)
        |> List.map (fun x -> x.Name)
    types |> List.map (fun x -> {| Type = x; Constraint = "" |})

let getMethodParameters (parameters: Reflection list) = 
    parameters
    |> List.where(fun x -> x.Kind = ReflectionKind.Parameter)
    |> List.map(fun x ->
        let name = if isNull x.Name then "" else x.Name
        match x.Type with
        | Some typeInfo -> 
            let typeMeta = getType typeInfo;
            { Type = typeMeta.Type; InnerTypes = typeMeta.InnerTypes; Name = Some name }
        | _ -> { Type = "object"; InnerTypes = []; Name = Some name }
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

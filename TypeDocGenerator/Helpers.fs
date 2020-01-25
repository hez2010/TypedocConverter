module Helpers

open Definitions

let rec toPascalCase (str: string) =
    if str.Length = 0
    then str
    else if str.Contains "." then 
         str.Split "." |> Array.reduce (fun a n -> toPascalCase a + "." + toPascalCase n)
         else str.Substring(0, 1).ToUpper() + str.Substring 1

let getDocComment (comment: Comment) (prefixBlankCount: int) =
    let escapeSymbols (text: string) = 
        if isNull text then ""
        else text
                .Replace("<", "&lt;")
                .Replace(">", "&gt;")
                .Replace("&", "&amp;")
    let toCommentText (text: string) = 
        if isNull text then ""
        else text.Split "\n" |> Array.map (fun t -> "/// " + escapeSymbols t) |> Array.reduce(fun accu next -> accu + "\n" + next)
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
    let blanks = 
        if prefixBlankCount = 0 then ""
        else 
            seq {
                for _ = 1 to prefixBlankCount do yield " "
            } |> Seq.reduce(fun accu next -> accu + next)
    ((summary + returns).Split "\n" |> Array.map(fun x -> blanks + x) |> Array.reduce(fun accu next -> accu + "\n" + next)) + "\n"

let rec getType (typeInfo: Type) = 
    let genericType =
        match typeInfo.Type with
        | "intrinsic" -> 
            match typeInfo.Name with
            | Some name -> 
                match name with
                | "number" -> "double"
                | "boolean" -> "bool"
                | "string" -> "string"
                | "void" -> "void"
                | _ -> "object"
            | _ -> "object"
        | "reference" | "typeParameter" -> 
            match typeInfo.Name with
            | Some name -> toPascalCase name
            | _ -> "object"
        | "array" -> 
            match typeInfo.ElementType with
            | Some elementType -> getType elementType + "[]"
            | _ -> "object[]"
        | "stringLiteral" -> "string"
        | "tuple" ->
            match typeInfo.Types with
            | Some innerTypes -> 
                match innerTypes with
                | [] -> "object"
                | _ -> "(" + System.String.Join(", ", innerTypes |> List.map getType) + ")"
            | _ -> "object"
        | "union" -> 
            match typeInfo.Types with
            | Some innerTypes -> 
                match innerTypes with
                | [] -> "object"
                | _ -> getType innerTypes.[0] // TODO: generate unions
            | _ -> "object"
        | "intersection" -> "object" // TODO: generate intersections
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
                                    | Some pt -> getType pt 
                                    | _ -> ""
                                )
                            |> List.where(fun x -> x <> "")
                        | _ -> []
                    let rec getDelegateParas paras =
                        match paras with
                        | [x] -> x
                        | (front::tails) -> front + ", " + getDelegateParas tails
                        | _ -> ""
                    let returnsType = 
                        match signature.Type with
                        | Some t -> getType t
                        | _ -> "void"
                    let typeParas = getDelegateParas paras
                    match typeParas with
                    | "" -> "Action"
                    | _ -> if returnsType = "void" then "Action<" + typeParas + ">" else "Func<" + typeParas + ", " + returnsType + ">"
                | _ -> "object"
            | _ -> "object"
        | _ -> "object"
    let innerTypes = 
        match typeInfo.TypeArguments with
        | Some args -> getGenericTypeArguments args
        | _ -> ""
    genericType + innerTypes
and getGenericTypeArguments (typeInfos: Type list) = 
    let innerTypes = typeInfos |> List.map getType
    match innerTypes with
    | [] -> ""
    | _ -> "<" + System.String.Join(", ", innerTypes) + ">"
and getGenericTypeParameters (nodes: Reflection list) = // TODO: generate constaints
    let types = 
        nodes 
        |> List.where(fun x -> x.Kind = ReflectionKind.TypeParameter)
        |> List.map (fun x -> x.Name)
    ("<" + System.String.Join(", ", types) + ">", "") // types, contraints

let getMethodParameters (parameters: Reflection list) = 
    parameters
    |> List.where(fun x -> x.Kind = ReflectionKind.Parameter)
    |> List.map(fun x ->
        let name = if isNull x.Name then "" else x.Name
        match x.Type with
        | Some typeInfo -> {| Type = getType typeInfo; Name = name |}
        | _ -> {| Type = "object"; Name = name |}
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
    match modifier with
    | [] -> ""
    | _ -> (modifier |> List.reduce(fun accu next -> accu + " " + next))

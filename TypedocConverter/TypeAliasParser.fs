module TypeAliasParser

open Definitions
open Entity
open Helpers

let parseUnionTypeAlias (section: string) (node: Reflection) (nodes: Type list): Entity list =
    let notStringLiteral = nodes |> List.tryFind(fun x -> x.Type <> "stringLiteral")
    let members = 
        match notStringLiteral with
        | Some _ -> 
            printWarning ("Type alias " + node.Name + " is not fully supported.")
            nodes 
            |> List.where (fun x -> x.Type = "stringLiteral" || x.Type = "literal")
            |> List.collect
                (fun x ->
                    match x.Value with
                    | Some value ->
                        [EnumMemberEntity(x.Id, value.ToString(), "///<summary>\n" + toCommentText (value.ToString()) + "\n///</summary>", None)]
                    | _ -> []
                )
        | None ->
            nodes 
            |> List.collect
                (fun x ->
                    match x.Value with
                    | Some value ->
                        [EnumMemberEntity(x.Id, value.ToString(), "///<summary>\n" + toCommentText (value.ToString()) + "\n///</summary>", None)]
                    | _ -> []
                )
    if List.isEmpty members then []
    else 
        [StringUnionEntity(node.Id, section, node.Name, 
            (
                match node.Comment with
                | Some comment -> getXmlDocComment comment
                | _ -> ""
            ),
            getModifier node.Flags,
            members
        )]

let parseTypeAlias (section: string) (node: Reflection): Entity list =
    let typeInfo = node.Type
    match typeInfo with
    | Some aliasType ->
        match aliasType.Type with
        | "union" -> 
            match aliasType.Types with
            | Some types -> parseUnionTypeAlias section node types
            | _ -> 
                printWarning ("Type alias " + node.Name + " is not supported.")
                []
        | _ ->
            printWarning ("Type alias " + node.Name + " is not supported.")
            []
    | _ -> []
module TypeAliasParser

open Definitions
open Entity
open Helpers

let parseUnionTypeAlias (section: string) (node: Reflection) (nodes: Type list): Entity list =
    let notStringLiteral = nodes |> List.tryFind(fun x -> x.Type <> "stringLiteral")
    let enums = 
        match notStringLiteral with
        | Some _ -> 
            printWarning ("Type alias " + node.Name + " is not fully supported.")
            nodes 
            |> List.where (fun x -> x.Type = "stringLiteral")
            |> List.collect
                (fun x ->
                    match x.Value with
                    | Some value -> 
                        [{
                            Name = value
                            Comment = "///<summary>\n" + toCommentText value + "\n///</summary>"
                            Value = None
                        }]
                    | _ -> []
                )
        | None ->
            nodes 
            |> List.collect
                (fun x ->
                    match x.Value with
                    | Some value -> 
                        [{
                            Name = value
                            Comment = "///<summary>\n" + toCommentText value + "\n///</summary>"
                            Value = None
                        }]
                    | _ -> []
                )
    if enums = [] then []
    else 
        [
            {
                Namespace = section
                Name = node.Name
                Comment = 
                    match node.Comment with
                    | Some comment -> getXmlDocComment comment
                    | _ -> ""
                Methods = []
                Events = []
                Properties = []
                Enums = enums
                InheritedFrom = []
                Type = EntityType.StringEnum
                TypeParameter = []
                Modifier = getModifier node.Flags
            }
        ]

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
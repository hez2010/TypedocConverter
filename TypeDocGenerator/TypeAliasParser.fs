module TypeAliasParser

open Definitions
open Entity
open Helpers

let notSupported (typeString: string) = 
    let backup = System.Console.ForegroundColor
    System.Console.ForegroundColor <- System.ConsoleColor.Yellow
    System.Console.Error.WriteLine ("[Warning] Type alias " + typeString + " is not supported.")
    System.Console.ForegroundColor <- backup

let parseUnionTypeAlias (section: string) (node: Reflection) (nodes: Type list): Entity list =
    let notStringLiteral = nodes |> List.tryFind(fun x -> x.Type <> "stringLiteral")
    let enums = 
        match notStringLiteral with
        | Some _ -> 
            notSupported node.Name
            []
        | None ->
            nodes 
            |> List.collect
                (
                    fun x ->
                        match x.Value with
                        | Some value -> 
                            [
                                {
                                    Name = toPascalCase value
                                    Comment = "///<summary>\n" + toCommentText value + "\n///</summary>"
                                    Value = None
                                }
                            ]
                        | _ -> []
                )
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
                notSupported node.Name
                []
        | _ ->
            notSupported node.Name
            []
    | _ -> []
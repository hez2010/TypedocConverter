module Parser

open Definitions
open EnumParser
open InterfaceClassParser
open TypeAliasParser
open Entity

let rec parseNode (section: string) (node: Reflection) (config: Config): Entity list =
    let childrenName = 
        match section with
        | "" | null -> node.Name
        | _ -> section + "." + node.Name
    
    let sectionName = 
        match section with
        | "" | null -> "TypedocConverter"
        | _ -> section

    match node.Kind with
    | ReflectionKind.Global ->
        match node.Children with
        | Some children -> parseNodes childrenName children config
        | _ -> []
    | ReflectionKind.Module ->
        match node.Children with
        | Some children ->
            parseNodes childrenName children config
        | _ -> []
    | ReflectionKind.ExternalModule ->
        match node.Children with
        | Some children -> parseNodes childrenName children config
        | _ -> []
    | ReflectionKind.Enum -> [parseEnum sectionName node]
    | ReflectionKind.Interface -> 
        match node.Children with
        | Some children ->
            [parseInterfaceAndClass sectionName node true config] @ parseNodes childrenName children config
        | _ -> [parseInterfaceAndClass sectionName node true config]
    | ReflectionKind.Class -> 
        match node.Children with
        | Some children ->
            [parseInterfaceAndClass sectionName node false config] @ parseNodes childrenName children config
        | _ -> [parseInterfaceAndClass sectionName node false config]
    | ReflectionKind.TypeAlias -> 
        match node.Type with
        | Some _ -> parseTypeAlias sectionName node
        | _ -> []
    | _ -> []

and parseNodes section (nodes: Reflection list) (config: Config): Entity list =
    match nodes with
    | ([ front ]) -> parseNode section front config
    | (front :: tails) ->
        parseNode section front config @ parseNodes section tails config
    | _ -> []

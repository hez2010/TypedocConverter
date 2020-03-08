module Parser

open Definitions
open EnumParser
open InterfaceClassParser
open TypeAliasParser
open Entity

let rec parseNode (section: string) (node: Reflection) (config: Config): Entity list =
    match node.Kind with
    | ReflectionKind.Global ->
        match node.Children with
        | Some children -> parseNodes section children config
        | _ -> []
    | ReflectionKind.Module ->
        match node.Children with
        | Some children ->
            parseNodes (if section = "" then node.Name else section + "." + node.Name) children config
        | _ -> []
    | ReflectionKind.ExternalModule ->
        match node.Children with
        | Some children -> parseNodes section children config
        | _ -> []
    | ReflectionKind.Enum -> [parseEnum section node]
    | ReflectionKind.Interface -> [parseInterfaceAndClass section node true config]
    | ReflectionKind.Class -> [parseInterfaceAndClass section node false config]
    | ReflectionKind.TypeAlias -> 
        match node.Type with
        | Some _ -> parseTypeAlias section node
        | _ -> []
    | _ -> []

and parseNodes section (nodes: Reflection list) (config: Config): Entity list =
    match nodes with
    | ([ front ]) -> parseNode section front config
    | (front :: tails) ->
        parseNode section front config @ parseNodes section tails config
    | _ -> []

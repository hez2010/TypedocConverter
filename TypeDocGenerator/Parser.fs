module Parser

open Definitions
open EnumParser
open InterfaceClassParser
open TypeAliasParser
open Entity

let rec parseNode (section: string) (node: Reflection): Entity list =
    match node.Kind with
    | ReflectionKind.Global ->
        match node.Children with
        | Some children -> parseNodes section children
        | _ -> []
    | ReflectionKind.Module ->
        match node.Children with
        | Some children ->
            parseNodes (if section = "" then node.Name else section + "." + node.Name) children
        | _ -> []
    | ReflectionKind.ExternalModule ->
        match node.Children with
        | Some children -> parseNodes section children
        | _ -> []
    | ReflectionKind.Enum -> [parseEnum section node]
    | ReflectionKind.Interface -> [renderInterfaceAndClass section node true]
    | ReflectionKind.Class -> [renderInterfaceAndClass section node false]
    | ReflectionKind.TypeAlias -> 
        match node.Type with
        | Some _ -> parseTypeAlias section node
        | _ -> []
    | _ -> []

and parseNodes section (nodes: Reflection list): Entity list =
    match nodes with
    | ([ front ]) -> parseNode section front
    | (front :: tails) ->
        parseNode section front @ parseNodes section tails
    | _ -> []

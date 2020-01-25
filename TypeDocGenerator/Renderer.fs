module Renderer

open Definitions
open EnumRenderer


let renderInterface section node = 
    0

let rec renderNode (section: string) (node: Reflection): string =
    match node.Kind with
    | ReflectionKind.Enum -> renderEnum section node
    | ReflectionKind.Global ->
        match node.Children with
        | Some children -> renderNodes section children
        | _ -> ""
    | ReflectionKind.Module ->
        match node.Children with
        | Some children ->
            renderNodes (if section = "" then node.Name else section + "." + node.Name) children
        | _ -> ""
    | ReflectionKind.ExternalModule ->
        match node.Children with
        | Some children -> renderNodes section children
        | _ -> ""
    | ReflectionKind.Interface
    | _ -> ""

and renderNodes section (nodes: Reflection list) =
    match nodes with
    | ([ front ]) -> renderNode section front
    | (front :: tails) ->
        renderNode section front + renderNodes section tails
    | _ -> ""

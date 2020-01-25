module Renderer

open Definitions
open EnumRenderer
open InterfaceRenderer
open ClassRenderer

let rec renderNode (section: string) (node: Reflection): string =
    match node.Kind with
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
    | ReflectionKind.Enum -> renderEnum section node
    | ReflectionKind.Interface -> renderInterface section node
    | ReflectionKind.Class -> renderClass section node
    | _ -> ""

and renderNodes section (nodes: Reflection list) =
    match nodes with
    | ([ front ]) -> renderNode section front
    | (front :: tails) ->
        renderNode section front + renderNodes section tails
    | _ -> ""

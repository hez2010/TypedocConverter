module EnumParser

open Definitions
open Helpers
open System
open Entity

let rec getEnumReferencedValue (nodes: Reflection list) value name = 
    match nodes 
          |> List.where(fun x -> 
              match x.DefaultValue with
              | Some v -> v <> value && (name <> x.Name)
              | _ -> true
          ) 
          |> List.where(fun x -> x.Name = value)
          |> List.tryFind(fun x -> 
                            let mutable intValue = 0
                            match x.DefaultValue with
                            | Some y -> Int32.TryParse(y, &intValue)
                            | _ -> true
           ) with
    | Some t -> t.DefaultValue
    | _ -> None
    
let parseEnum (section: string) (node: Reflection): Entity =
    let values = 
        (match node.Children with
        | Some children ->
            children
            |> List.where (fun x -> x.Kind = ReflectionKind.EnumMember)
        | None -> [])
    let members =
        values |> List.map (fun x ->
            let comment = 
                match x.Comment with
                | Some comment -> getXmlDocComment comment
                | _ -> ""
            let mutable intValue = 0L
            match x.DefaultValue with
            | Some value -> if Int64.TryParse(value, &intValue) then EnumMemberEntity(x.Id, x.Name, comment, Some (intValue.ToString()))
                            else match getEnumReferencedValue values value x.Name with
                                 | Some t -> EnumMemberEntity(x.Id, x.Name, comment, Some t)
                                 | _ -> EnumMemberEntity(x.Id, x.Name, comment, None)
            | _ -> EnumMemberEntity(x.Id, x.Name, comment, None)
        )
    EnumEntity(node.Id, (if section = "" then "TypedocConverter" else section), node.Name, 
        (
            match node.Comment with
            | Some comment -> getXmlDocComment comment
            | _ -> ""
        ), 
        getModifier node.Flags, members)
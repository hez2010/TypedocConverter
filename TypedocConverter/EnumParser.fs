module EnumParser

open Definitions
open Helpers
open System
open Entity

let rec getEnumReferencedValue (nodes: Reflection list) value name = 
    match nodes 
          |> List.where(fun x -> 
              match x.DefaultValue with
              | Some v -> v <> value && not (name = x.Name)
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
    let values = match node.Children with
                 | Some children ->
                     children
                     |> List.where (fun x -> x.Kind = ReflectionKind.EnumMember)
                 | None -> []
    { 
        Type = EntityType.Enum;
        Namespace = if section = "" then "TypedocConverter" else section;
        Modifier = getModifier node.Flags;
        Name = node.Name
        Comment = 
            match node.Comment with
            | Some comment -> getXmlDocComment comment
            | _ -> ""
        Methods = []; Properties = []; Events = []; InheritedFrom = [];
        Enums = values |> List.map (fun x ->
            let comment = 
                match x.Comment with
                | Some comment -> getXmlDocComment comment
                | _ -> ""
            let mutable intValue = 0L
            match x.DefaultValue with
            | Some value -> if Int64.TryParse(value, &intValue) then { Comment = comment; Name = toPascalCase x.Name; Value = Some intValue; }
                            else match getEnumReferencedValue values value x.Name with
                                 | Some t -> { Comment = comment; Name = x.Name; Value = Some (int64 t); }
                                 | _ -> { Comment = comment; Name = x.Name; Value = None; }
            | _ -> { Comment = comment; Name = x.Name; Value = None; }
        );
        TypeParameter = []
    }
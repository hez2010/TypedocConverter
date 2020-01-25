module EnumRenderer

open Definitions
open Helper
open System
open System.Text

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
    
let renderEnum (section: string) (node: Reflection): string =
    let values = match node.Children with
                 | Some children ->
                     children
                     |> List.where (fun x -> x.Kind = ReflectionKind.EnumMember)
                 | None -> []
    
    let body = StringBuilder()
    body.AppendFormat("namespace {0}\n{{\n", toPascalCase section) |> ignore
    body.AppendFormat("    {0}enum {1}\n    {{\n", getModifier node.Flags, toPascalCase node.Name) |> ignore
    values |> List.iteri (fun i x ->
        let comma = if i = values.Length - 1 then "" else ","
        let mutable intValue = 0
        match x.DefaultValue with
        | Some value -> if Int32.TryParse(value, &intValue) then body.AppendFormat("        {0} = {1}{2}\n", toPascalCase x.Name, intValue, comma) |> ignore
                        else match getEnumReferencedValue values value x.Name with
                             | Some t -> body.AppendFormat("        {0} = {1}{2}\n", toPascalCase x.Name, t, comma) |> ignore
                             | _ -> body.AppendFormat("        {0}{1}\n", x.Name, comma) |> ignore
        | _ -> body.AppendFormat("        {0}{1}\n", x.Name, comma) |> ignore
    )
    body.AppendLine "    }\n}" |> ignore
    body.ToString()
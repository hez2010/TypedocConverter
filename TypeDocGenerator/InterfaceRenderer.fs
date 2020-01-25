module InterfaceRenderer

open Definitions
open Helpers
open System.Text

let renderInterface (section: string) (node: Reflection): string =
    let body = StringBuilder()
    body.AppendFormat("namespace {0}\n{{\n    using System;\n    using System.Threading.Tasks;\n    using System.Collections.Generic;\n\n", toPascalCase (if section = "" then "TypeDocGenerator" else section)) |> ignore
    match node.Comment with
    | Some comment -> body.AppendFormat("{0}", getDocComment comment 4) |> ignore
    | _ -> ()
    let exts = 
        match node.ExtendedTypes with
        | Some types -> " : " + System.String.Join(", ", types |> List.map(fun x -> getType x))
        | _ -> ""
    let genericType =
        let types = 
              match node.TypeParameter with
              | Some tp -> Some (getGenericTypeParameters tp)
              | _ -> None
        match types with
        | Some result -> result.Types
        | _ -> ""
    body.AppendFormat("    {0}interface {1}{2}{3}\n    {{\n", getModifier node.Flags, toPascalCase node.Name, genericType, exts) |> ignore
    let properties = 
        match node.Children with
        | Some children -> 
            children |> List.where(fun x -> x.Kind = ReflectionKind.Property)
                     |> List.where(fun x -> x.InheritedFrom = None) // exclude inhreited properties
                     |> List.where(fun x -> x.Overwrites = None) // exclude overrites methods
        | _ -> []
    let events = 
        match node.Children with
        | Some children -> 
             children |> List.where(fun x -> x.Kind = ReflectionKind.Event)
                      |> List.where(fun x -> x.InheritedFrom = None) // exclude inhreited properties
                      |> List.where(fun x -> x.Overwrites = None) // exclude overrites methods
        | _ -> []
    let methods = 
        match node.Children with
        | Some children -> 
            children |> List.where(fun x -> x.Kind = ReflectionKind.Method)
                     |> List.where(fun x -> x.InheritedFrom = None) // exclude inhreited methods
                     |> List.where(fun x -> x.Overwrites = None) // exclude overrites methods
        | _ -> []
    properties 
        |> List.iter (
            fun x -> 
                match x.Comment with
                | Some comment -> body.AppendFormat("{0}", getDocComment comment 8) |> ignore
                | _ -> ()
                body.AppendFormat("        {0}{1} {2} {{ get; set; }}\n", 
                    match x.Type with
                    | Some typeInfo -> getType typeInfo
                    | _ -> "object"
                    ,
                    match x.Flags.IsOptional with
                    | Some optional -> if optional then "?" else ""
                    | _ -> ""
                    ,
                    toPascalCase x.Name) |> ignore
        )
    events
        |> List.iter (
            fun x -> 
                let paras = 
                    match x.Signatures with
                    | Some sigs -> 
                        sigs 
                        |> List.where (fun x -> x.Kind = ReflectionKind.Event)
                        |> List.map(fun x -> x.Parameters)
                        |> List.collect (fun x ->
                            match x with
                            | Some paras -> paras
                            | _ -> [])
                    | _ -> []
                match x.Comment with
                | Some comment -> body.AppendFormat("{0}", getDocComment comment 8) |> ignore
                | _ -> ()
                body.AppendFormat("        event {0}{1} {2};\n", 
                    match paras with
                    | (front::_) -> 
                        match front.Type with
                        | Some typeInfo -> getType typeInfo
                        | _ -> "Delegate"
                    | _ -> 
                        match x.Type with
                        | Some typeInfo -> getType typeInfo
                        | _ -> "Delegate"
                    ,
                    match x.Flags.IsOptional with
                    | Some optional -> if optional then "?" else ""
                    | _ -> ""
                    ,
                    toPascalCase x.Name) |> ignore
        )
    methods 
        |> List.iter (
            fun x -> 
                match x.Comment with
                | Some comment -> body.AppendFormat("{0}", getDocComment comment 8) |> ignore
                | _ -> ()
                body.AppendFormat("        {0} {1}{2}({3});\n", 
                    match (match x.Signatures with
                          | Some signatures -> 
                              signatures 
                              |> List.where(fun x -> x.Kind = ReflectionKind.CallSignature)
                          | _ -> []) with
                    | [] -> "object"
                    | (front::_) ->
                        match front.Type with
                        | Some typeInfo -> getType typeInfo
                        | _ -> "object"
                , toPascalCase x.Name
                , match x.Signatures with
                  | Some (sigs::_) -> 
                      let types = 
                            match sigs.TypeParameter with
                            | Some tp -> Some (getGenericTypeParameters tp)
                            | _ -> None
                      match types with
                      | Some result -> result.Types
                      | _ -> ""
                  | _ -> ""
                , System.String.Join
                    (
                        ", ", 
                        getMethodParameters 
                            (match x.Signatures with
                            | Some signatures -> 
                                signatures 
                                |> List.where(fun x -> x.Kind = ReflectionKind.CallSignature) 
                                |> List.map(
                                    fun x -> 
                                        match x.Parameters with
                                        | Some parameters -> parameters |> List.where(fun p -> p.Kind = ReflectionKind.Parameter)
                                        | _ -> []
                                    )
                                |> List.reduce(fun accu next -> accu @ next)
                            | _ -> [])
                            |> List.map(fun x -> x.Type + " " + x.Name)
                    )
                ) |> ignore
        )
    body.AppendLine("    }\n}\n") |> ignore
    body.ToString()
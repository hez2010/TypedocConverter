module ClassRenderer

open Definitions
open Helpers
open System.Text

let renderClass (section: string) (node: Reflection): string =
    let body = StringBuilder()
    body.AppendFormat("namespace {0}\n{{\n", toPascalCase (if section = "" then "TypeDocGenerator" else section)) |> ignore
    match node.Comment with
    | Some comment -> body.AppendFormat("{0}", getDocComment comment 4) |> ignore
    | _ -> ()
    let exts = 
        match node.ExtendedTypes with
        | Some types -> " : " + System.String.Join(", ", types |> List.map(fun x -> getType x))
        | _ -> ""
    let genericType =
        match node.TypeParameter with
        | Some tps -> 
            let types, _ = getGenericTypeParameters tps
            types
        | _ -> ""
    body.AppendFormat("    {0}class {1}{2}{3}\n    {{\n", getModifier node.Flags, toPascalCase node.Name, genericType, exts) |> ignore
    let properties = 
        match node.Children with
        | Some children -> 
            children |> List.where(fun x -> x.Kind = ReflectionKind.Property)
                     |> List.where(fun x -> x.InheritedFrom = None) // exclude inhreited properties
        | _ -> []
    let methods = 
        match node.Children with
        | Some children -> 
            children |> List.where(fun x -> x.Kind = ReflectionKind.Method)
                     |> List.where(fun x -> x.InheritedFrom = None) // exclude inhreited methods
        | _ -> []
    properties 
        |> List.iter (
            fun x -> 
                match x.Comment with
                | Some comment -> body.AppendFormat("{0}", getDocComment comment 8) |> ignore
                | _ -> ()
                body.AppendFormat("        {0}{1} {2} {{ get; set; }}{3}\n", 
                    getModifier x.Flags,
                    match x.Type with
                    | Some typeInfo -> getType typeInfo
                    | _ -> "object"
                , toPascalCase x.Name,
                    match x.DefaultValue with
                    | Some value -> " = " + value + ";";
                    | _ -> "") |> ignore
        )

    methods 
        |> List.iter (
            fun x -> 
                match x.Comment with
                | Some comment -> body.AppendFormat("{0}", getDocComment comment 8) |> ignore
                | _ -> ()
                body.AppendFormat("        {0}{1} {2}{3}({4});\n",
                    getModifier x.Flags,
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
                , match x.TypeParameter with
                  | Some tps -> 
                      let types, _ = getGenericTypeParameters tps
                      types
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
    body.Append("    }\n}\n") |> ignore
    body.ToString()
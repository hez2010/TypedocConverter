module InterfaceClassParser

open Definitions
open Helpers
open Entity

let parseInterfaceAndClass (section: string) (node: Reflection) (isInterface: bool): Entity =
    let comment = 
        match node.Comment with
        | Some comment -> getXmlDocComment comment
        | _ -> ""
    let exts = 
        (match node.ExtendedTypes with
        | Some types -> types |> List.map(fun x -> getType x)
        | _ -> []) @
        (match node.ImplementedTypes with
        | Some types -> types |> List.map(fun x -> getType x)
        | _ -> [])
    let genericType =
        let types = 
              match node.TypeParameter with
              | Some tp -> Some (getGenericTypeParameters tp)
              | _ -> None
        match types with
        | Some result -> result
        | _ -> []
    let properties = 
        match node.Children with
        | Some children -> 
            if isInterface then
                children 
                |> List.where(fun x -> x.Kind = ReflectionKind.Property)
                |> List.where(fun x -> x.InheritedFrom = None) // exclude inhreited properties
                |> List.where(fun x -> x.Overwrites = None) // exclude overrites properties
            else children |> List.where(fun x -> x.Kind = ReflectionKind.Property)
        | _ -> []
    let events = 
        match node.Children with
        | Some children -> 
            if isInterface then
                children 
                |> List.where(fun x -> x.Kind = ReflectionKind.Event)
                |> List.where(fun x -> x.InheritedFrom = None) // exclude inhreited events
                |> List.where(fun x -> x.Overwrites = None) // exclude overrites events
            else children |> List.where(fun x -> x.Kind = ReflectionKind.Event)
        | _ -> []
    let methods = 
        match node.Children with
        | Some children -> 
            if isInterface then
                children 
                |> List.where(fun x -> x.Kind = ReflectionKind.Method)
                |> List.where(fun x -> x.InheritedFrom = None) // exclude inhreited methods
                |> List.where(fun x -> x.Overwrites = None) // exclude overrites methods
            else children |> List.where(fun x -> x.Kind = ReflectionKind.Method)
        | _ -> []
    {
        Type = if isInterface then EntityType.Interface else EntityType.Class;
        Namespace = if section = "" then "TypedocConverter" else section;
        Name = node.Name;
        Comment = comment;
        Modifier = getModifier node.Flags;
        InheritedFrom = exts;
        Methods = 
            methods 
            |> List.map (
                fun x -> 
                    let retType = 
                        match (
                                match x.Signatures with
                                | Some signatures -> 
                                    signatures |> List.where(fun x -> x.Kind = ReflectionKind.CallSignature)
                                | _ -> []) 
                            with
                            | [] -> { Type = "object"; InnerTypes = []; Name = None }
                            | (front::_) ->
                                match front.Type with
                                | Some typeInfo -> getType typeInfo
                                | _ -> { Type = "object"; InnerTypes = []; Name = None }
                    let typeParameter = 
                        match x.Signatures with
                        | Some (sigs::_) -> 
                            let types = 
                                  match sigs.TypeParameter with
                                  | Some tp -> Some (getGenericTypeParameters tp)
                                  | _ -> None
                            match types with
                            | Some result -> result
                            | _ -> []
                        | _ -> []
                        |> List.map (fun x -> x.Type)
                    let parameters = 
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
                    {
                        Comment = 
                            match x.Comment with
                            | Some comment -> getXmlDocComment comment
                            | _ -> ""
                        Modifier = if isInterface then [] else getModifier x.Flags;
                        Type = retType
                        Name = x.Name
                        TypeParameter = typeParameter
                        Parameter = parameters
                    }
            );
        Events = 
            events
            |> List.map (
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
                    { 
                        Name = x.Name; 
                        IsOptional = 
                            match x.Flags.IsOptional with
                            | Some optional -> optional
                            | _ -> false
                            ;
                        DelegateType = 
                            match paras with
                            | (front::_) -> 
                                match front.Type with
                                | Some typeInfo -> getType typeInfo
                                | _ -> { Type = "System.Delegate"; Name = None; InnerTypes = [] }
                            | _ -> 
                                match x.Type with
                                | Some typeInfo -> getType typeInfo
                                | _ -> { Type = "System.Delegate"; Name = None; InnerTypes = [] }
                            ;
                        Comment = 
                            match x.Comment with
                            | Some comment -> getXmlDocComment comment
                            | _ -> ""
                            ;
                        Modifier = if isInterface then [] else getModifier x.Flags;
                    }
            );
        Properties = 
            properties 
            |> List.map (
                fun x -> 
                    {
                        Comment = 
                            match x.Comment with
                            | Some comment -> getXmlDocComment comment
                            | _ -> ""
                        Modifier = if isInterface then [] else getModifier x.Flags;
                        Name = x.Name
                        Type = 
                            match x.Type with
                            | Some typeInfo -> getType typeInfo
                            | _ -> { Type = "object"; Name = None; InnerTypes = [] }
                        WithGet = true;
                        WithSet = true;
                        IsOptional =
                            match x.Flags.IsOptional with
                            | Some optional -> optional
                            | _ -> false
                            ;
                        InitialValue = 
                            match x.DefaultValue with
                            | Some value -> Some value
                            | _ -> None
                    }
            );
        Enums = [];
        TypeParameter = genericType |> List.map(fun x -> x.Type);
    }
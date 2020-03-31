module InterfaceClassParser

open Definitions
open Helpers
open Entity

let parseInterfaceAndClass (section: string) (node: Reflection) (isInterface: bool) (config: Config): Entity =
    let comment = getComment node
    let exts = 
        (match node.ExtendedTypes with
        | Some types -> types |> List.map(fun x -> getType config x)
        | _ -> []) @
        (match node.ImplementedTypes with
        | Some types -> types |> List.map(fun x -> getType config x)
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
            |> List.collect (
                fun x -> 
                    match x.Signatures with
                    | Some signatures -> 
                        signatures 
                        |> List.where(fun s -> s.Kind = ReflectionKind.CallSignature)
                        |> List.map (
                            fun s -> 
                                let retType = 
                                    match s.Type with
                                    | Some t -> getType config t
                                    | _ -> { Type = "object"; InnerTypes = []; Name = None }
                                let typeParameter = 
                                    let types = 
                                        match s.TypeParameter with
                                        | Some tp -> Some (getGenericTypeParameters tp)
                                        | _ -> None
                                    match types with
                                    | Some result -> result
                                    | _ -> []
                                    |> List.map (fun t -> t.Type)
                                let parameters = 
                                    getMethodParameters 
                                        config
                                        (match s.Parameters with
                                        | Some parameters -> parameters |> List.where(fun p -> p.Kind = ReflectionKind.Parameter)
                                        | _ -> [])
                                {
                                    Comment = getCommentFromSignature s
                                    Modifier = if isInterface then [] else getModifier x.Flags;
                                    Type = retType
                                    Name = s.Name
                                    TypeParameter = typeParameter
                                    Parameter = parameters
                                }
                        )
                    | _ -> []
                    
            );
        Events = 
            events
            |> List.collect (
                fun x -> 
                    match x.Signatures with
                    | Some signature -> 
                        signature 
                        |> List.where (fun s -> s.Kind = ReflectionKind.Event)
                        |> List.map(
                            fun s -> 
                                { 
                                    Name = x.Name; 
                                    IsOptional = 
                                        match x.Flags.IsOptional with
                                        | Some optional -> optional
                                        | _ -> false
                                        ;
                                    DelegateType = 
                                        match s.Parameters with
                                        | Some (front::_) -> 
                                            match front.Type with
                                            | Some pType -> getType config pType
                                            | _ -> { Type = "System.Delegate"; Name = None; InnerTypes = [] }
                                        | _ -> { Type = "System.Delegate"; Name = None; InnerTypes = [] }
                                        ;
                                    Comment = getCommentFromSignature s
                                        ;
                                    Modifier = if isInterface then [] else getModifier x.Flags;
                                }
                            )
                    | _ -> []
            );
        Properties = 
            properties 
            |> List.map (
                fun x -> 
                    {
                        Comment = getComment x
                        Modifier = if isInterface then [] else getModifier x.Flags;
                        Name = x.Name
                        Type = 
                            match x.Type with
                            | Some typeInfo -> getType config typeInfo
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
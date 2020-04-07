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
        (match node.Children with
        | Some children -> 
            if isInterface then
                children 
                |> List.where(fun x -> x.Kind = ReflectionKind.Property)
                |> List.where(fun x -> x.InheritedFrom = None) // exclude inhreited properties
                |> List.where(fun x -> x.Overwrites = None) // exclude overrites properties
            else children |> List.where(fun x -> x.Kind = ReflectionKind.Property)
        | _ -> [])
        |> List.map (
            fun x -> 
                PropertyEntity(x.Name, getComment x, 
                    (if isInterface then [] else getModifier x.Flags),
                    (
                        match x.Type with
                        | Some typeInfo -> getType config typeInfo
                        | _ -> TypeEntity("object", [])
                    ),
                    true,
                    true,
                    (
                        match x.Flags.IsOptional with
                        | Some optional -> optional
                        | _ -> false
                    ),
                    (
                        match x.DefaultValue with
                        | Some value -> Some value
                        | _ -> None
                    )
                )
        )
    let events = 
        (match node.Children with
        | Some children -> 
            if isInterface then
                children 
                |> List.where(fun x -> x.Kind = ReflectionKind.Event)
                |> List.where(fun x -> x.InheritedFrom = None) // exclude inhreited events
                |> List.where(fun x -> x.Overwrites = None) // exclude overrites events
            else children |> List.where(fun x -> x.Kind = ReflectionKind.Event)
        | _ -> [])
        |> List.collect (
            fun x -> 
                match x.Signatures with
                | Some signature -> 
                    signature 
                    |> List.where (fun s -> s.Kind = ReflectionKind.Event)
                    |> List.map(
                        fun s -> 
                            EventEntity(x.Name, getCommentFromSignature s, 
                                (if isInterface then [] else getModifier x.Flags), 
                                (
                                    match x.Flags.IsOptional with
                                    | Some optional -> optional
                                    | _ -> false
                                ),
                                (
                                    match s.Parameters with
                                    | Some [front] | Some (front::_) -> 
                                        match front.Type with
                                        | Some pType -> getType config pType
                                        | _ -> TypeEntity("System.Delegate", [])
                                    | _ -> TypeEntity("System.Delegate", [])
                                )
                            )
                        )
                | _ -> []
        )
    let methods = 
        (match node.Children with
        | Some children -> 
            if isInterface then
                children 
                |> List.where(fun x -> x.Kind = ReflectionKind.Method)
                |> List.where(fun x -> x.InheritedFrom = None) // exclude inhreited methods
                |> List.where(fun x -> x.Overwrites = None) // exclude overrites methods
            else children |> List.where(fun x -> x.Kind = ReflectionKind.Method)
        | _ -> [])
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
                                | _ -> TypeEntity("object", [])
                            let typeParameter = 
                                let types = 
                                    match s.TypeParameter with
                                    | Some tp -> Some (getGenericTypeParameters tp)
                                    | _ -> None
                                match types with
                                | Some result -> result
                                | _ -> []
                            let parameters = 
                                getMethodParameters 
                                    config
                                    (match s.Parameters with
                                    | Some parameters -> parameters |> List.where(fun p -> p.Kind = ReflectionKind.Parameter)
                                    | _ -> [])
                            MethodEntity(s.Name, getCommentFromSignature s, 
                                (if isInterface then [] else getModifier x.Flags), typeParameter, parameters, retType)
                    )
                | _ -> []
        )
    ClassInterfaceEntity(
        (if section = "" then "TypedocConverter" else section), node.Name, comment, getModifier node.Flags,
        methods, properties, events, exts, genericType, isInterface
    )
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
    let members = 
        match node.Children with
        | Some children ->
            children 
            |> List.where (fun x -> 
                if isInterface then x.InheritedFrom = None && x.Overwrites = None
                else true
            )
            |> List.collect (fun x ->
                match x.Kind with
                | ReflectionKind.Constructor ->
                    match x.Signatures with
                    | Some signature -> 
                        signature 
                        |> List.where (fun s -> s.Kind = ReflectionKind.ConstructorSignature)
                        |> List.map(fun s ->
                            let parameters = 
                                getMethodParameters 
                                    config
                                    (match s.Parameters with
                                    | Some parameters -> parameters |> List.where(fun p -> p.Kind = ReflectionKind.Parameter)
                                    | _ -> [])
                            ConstructorEntity(s.Id, node.Name, getComment x, [], parameters)
                        )
                    | _ -> []
                | ReflectionKind.Property -> 
                    [
                        PropertyEntity(x.Id, x.Name, getComment x, 
                            (if isInterface then [] else getModifier x.Flags),
                            (
                                match x.Type with
                                | Some typeInfo -> getType config typeInfo
                                | _ -> TypeEntity(0, "object", [])
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
                    ]
                | ReflectionKind.Event ->
                    match x.Signatures with
                    | Some signature -> 
                        signature 
                        |> List.where (fun s -> s.Kind = ReflectionKind.Event)
                        |> List.map(fun s -> 
                            EventEntity(s.Id, x.Name, getCommentFromSignature s, 
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
                                        | _ -> TypeEntity(0, "System.Delegate", [])
                                    | _ -> TypeEntity(0, "System.Delegate", [])
                                )
                            )
                        )
                    | _ -> []
                | ReflectionKind.Method ->
                    match x.Signatures with
                    | Some signatures -> 
                        signatures 
                        |> List.where(fun s -> s.Kind = ReflectionKind.CallSignature)
                        |> List.map (fun s -> 
                            let retType = 
                                match s.Type with
                                | Some t -> getType config t
                                | _ -> TypeEntity(0, "object", [])
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
                            MethodEntity(s.Id, s.Name, getCommentFromSignature s, 
                                (if isInterface then [] else getModifier x.Flags), typeParameter, parameters, retType)
                        )
                    | _ -> []
                | _ -> []
            )
        | _ -> []

    ClassInterfaceEntity(
        node.Id, (if section = "" then "TypedocConverter" else section), node.Name, comment, getModifier node.Flags,
        members, exts, genericType, isInterface
    )
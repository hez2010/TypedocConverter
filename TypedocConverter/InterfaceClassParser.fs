module InterfaceClassParser

open Definitions
open Helpers
open Entity

let parseInterfaceAndClass (section: string) (node: Reflection) (isInterface: bool) (config: Config): Entity =
    let mutable accessorMap : Map<string, bool * bool> = Map.empty
    let comment = getComment node
    let exts = 
        (match node.ExtendedTypes with
        | Some types -> types |> List.map(fun x -> getType config None x)
        | _ -> []) @
        (match node.ImplementedTypes with
        | Some types -> types |> List.map(fun x -> getType config None x)
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
                                    None
                                    (match s.Parameters with
                                    | Some parameters -> parameters |> List.where(fun p -> p.Kind = ReflectionKind.Parameter)
                                    | _ -> [])
                            ConstructorEntity(s.Id, node.Name, getComment x, [], parameters)
                        )
                    | _ -> []
                | ReflectionKind.Property -> 
                    let mutable access = if Map.containsKey x.Name accessorMap then accessorMap.[x.Name] else (false, false)
                    access <- (true, true)
                    accessorMap <- Map.add x.Name access accessorMap
                    [
                        PropertyEntity(x.Id, x.Name, getComment x, 
                            (if isInterface then [] else getModifier x.Flags),
                            (
                                match x.Type with
                                | Some typeInfo -> getType config None typeInfo
                                | _ -> TypeEntity(0, "object", "intrinsic", [], Plain, None)
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
                | ReflectionKind.Accessor -> 
                    match x.GetSignature with
                    | Some [signature] -> 
                        let mutable access = if Map.containsKey x.Name accessorMap then accessorMap.[x.Name] else (false, false)
                        access <- (true, snd access)
                        accessorMap <- Map.add x.Name access accessorMap
                        [PropertyEntity(x.Id, x.Name, getComment x, 
                            (if isInterface then [] else getModifier x.Flags),
                            (
                                match signature.Type with
                                | Some typeInfo -> getType config None typeInfo
                                | _ -> TypeEntity(0, "object", "intrinsic", [], Plain, None)
                            ),
                            true,
                            false,
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
                        )]
                    | _ -> 
                        match x.SetSignature with
                        | Some [signature] -> 
                            let mutable access = if Map.containsKey x.Name accessorMap then accessorMap.[x.Name] else (false, false)
                            access <- (fst access, true)
                            accessorMap <- Map.add x.Name access accessorMap
                            [PropertyEntity(x.Id, x.Name, getComment x, 
                                (if isInterface then [] else getModifier x.Flags),
                                (
                                    match signature.Type with
                                    | Some typeInfo -> getType config None typeInfo
                                    | _ -> TypeEntity(0, "object", "intrinsic", [], Plain, None)
                                ),
                                false,
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
                            )]
                        | _ -> []
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
                                        | Some pType -> getType config None pType
                                        | _ -> TypeEntity(0, "System.Delegate", "reflection", [], Plain, None)
                                    | _ -> TypeEntity(0, "System.Delegate", "reflection", [], Plain, None)
                                )
                            )
                        )
                    | _ -> 
                        match x.Type with
                        | Some eType -> 
                            [EventEntity(x.Id, x.Name, getComment x, 
                                (if isInterface then [] else getModifier x.Flags),
                                (
                                    match x.Flags.IsOptional with
                                    | Some optional -> optional
                                    | _ -> false
                                ),
                                getType config None eType
                            )]
                        | _ -> []
                | ReflectionKind.Method ->
                    match x.Signatures with
                    | Some signatures -> 
                        signatures 
                        |> List.where(fun s -> s.Kind = ReflectionKind.CallSignature)
                        |> List.map (fun s -> 
                            let retType = 
                                match s.Type with
                                | Some t -> getType config None t
                                | _ -> TypeEntity(0, "object", "intrinsic", [], Plain, None)
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
                                    None
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
        
    let indexer = 
        match node.IndexSignature with
        | None -> None
        | Some(index) -> 
            Some(
                IndexerEntity (
                    index.Id, 
                    getComment index, 
                    ["public"], 
                    (
                        match index.Type with 
                        | None -> TypeEntity(0, "object", "intrinsic", [], Plain, None)
                        | Some(t) -> getType config None t
                    ),
                    (
                        match index.Parameters with
                        | None | Some([]) -> [TypeEntity(0, "object", "intrinsic", [], Plain, None)]
                        | Some(ps) -> ps |> getMethodParameters config None
                    )
                )
            )
    
    let mutable processedProperties : string Set = Set.empty
    let mergedMembers = 
        members |> List.collect(
            fun x ->
                match x with 
                | PropertyEntity(id, name, comment, modifiers, pType, withGet, withSet, isOptional, defaultValue) -> 
                    if Set.contains name processedProperties then []
                    else 
                        processedProperties <- Set.add name processedProperties
                        let mutable access = if Map.containsKey name accessorMap then accessorMap.[name] else (false, false)
                        [PropertyEntity(id, name, comment, modifiers, pType, fst access || withGet, snd access || withSet, isOptional, defaultValue)]
                | _ -> [x]
        )

    ClassInterfaceEntity(
        node.Id, section, node.Name, comment, getModifier node.Flags,
        mergedMembers, exts, genericType, isInterface, indexer
    )
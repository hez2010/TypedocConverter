module Printer

open Entity
open Helpers

let append spliter accu next = accu + spliter + next

let arrangeComment (comment: string) blankSpace = 
    let blanks = 
        seq {
            for _ = 1 to blankSpace do yield " "
        } |> Seq.reduce (append "")
    comment.Split("\n")
    |> Array.map(fun x -> blanks + x)
    |> Array.reduce (append "\n")

let rec arrangeType (typeInfo: EntityBodyType) =
    let pascalizeTypeName name =
        match name with
        | "object" | "string" | "double" | "void" | "bool" -> name
        | _ -> toPascalCase name
    match typeInfo.Type with
    | "Array" -> 
        (match typeInfo.InnerTypes with
        | [x] -> arrangeType x
        | _ -> "object") + "[]"
    | "ValueTuple" ->
        "(" + System.String.Join(", ", typeInfo.InnerTypes |> List.map arrangeType) + ")"
    | _ -> 
        match typeInfo.InnerTypes with
        | [] -> pascalizeTypeName typeInfo.Type
        | types -> pascalizeTypeName typeInfo.Type + "<" + System.String.Join(", ", types |> List.map arrangeType) + ">"

let printEntity (references: string list) (entity: Entity) = 
    let thisNamespace = entity.Namespace.Split "." |> Array.map toPascalCase |> Array.reduce (append ".")
    printfn "namespace %s\n{" thisNamespace
    printfn "    using System;"
    printfn "    using System.Collections.Generic;"
    printfn "    using System.Threading.Tasks;"
    references
    |> List.where(fun x -> x <> thisNamespace)
    |> List.iter(fun x -> printfn "    using %s;" (x))
    printf "\n"

    if (entity.Comment <> "") then printfn "%s" (arrangeComment entity.Comment 4) else ()
    printfn "    %s%s%s %s%s%s\n    {" 
        (
            match entity.Modifier with
            | [] -> ""
            | modifier -> modifier |> List.reduce (fun a b -> a + b + " ")
        )
        (if entity.Modifier = [] then "" else " ")
        (
            match entity.Type with
            | EntityType.Class -> "class"
            | EntityType.Interface -> "interface"
            | EntityType.Enum -> "enum"
        )
        (toPascalCase entity.Name)
        (
            match entity.TypeParameter with
            | [] -> ""
            | paras -> "<" + System.String.Join(", ", paras |> List.map toPascalCase) + ">"
        )
        (
            match entity.InheritedFrom with
            | [] -> ""
            | paras -> " : " + System.String.Join(", ", paras |> List.map arrangeType)
        )

    entity.Enums
    |> List.iteri
        (
            fun i x ->
                if (x.Comment <> "") then printfn "%s" (arrangeComment x.Comment 8) else ()
                printfn "        %s%s%s"
                    (toPascalCase x.Name)
                    (
                        match x.Value with
                        | None -> ""
                        | Some value -> " = " + string value
                    )
                    (if i = entity.Enums.Length - 1 then "" else ",")

        )

    entity.Properties
    |> List.iter
        (
            fun x ->
                if (x.Comment <> "") then printfn "%s" (arrangeComment x.Comment 8) else ()
                printfn "        %s%s%s %s { %s%s}%s"
                    (if x.Modifier = [] then "" else System.String.Join(" ", x.Modifier) + " ")
                    (arrangeType x.Type)
                    (if x.IsOptional then "?" else "")
                    (toPascalCase x.Name)
                    (
                        if entity.Type = EntityType.Interface 
                        then
                            if x.WithGet then "get; " else ""
                        else
                            if x.WithGet then "get => throw new NotImplementedException(); " else ""
                    )
                    (
                        if entity.Type = EntityType.Interface 
                        then
                            if x.WithGet then "set; " else ""
                        else
                            if x.WithGet then "set => throw new NotImplementedException(); " else ""
                    )
                    (
                        match x.InitialValue with
                        | None -> ""
                        | Some value -> " = " + string value + ";"
                    )
        )

    entity.Events
    |> List.iter
        (
            fun x ->
                if (x.Comment <> "") then printfn "%s" (arrangeComment x.Comment 8) else ()
                printfn "        %sevent %s%s %s;"
                    (if x.Modifier = [] then "" else System.String.Join(" ", x.Modifier) + " ")
                    (arrangeType x.DelegateType)
                    (if x.IsOptional then "?" else "")
                    (toPascalCase x.Name)
        )

    entity.Methods
    |> List.iter
        (
            fun x ->
                if (x.Comment <> "") then printfn "%s" (arrangeComment x.Comment 8) else ()
                printfn "        %s%s %s%s(%s)%s"
                    (if x.Modifier = [] then "" else System.String.Join(" ", x.Modifier) + " ")
                    (arrangeType x.Type)
                    (toPascalCase x.Name)
                    (
                        match x.TypeParameter with
                        | [] -> ""
                        | tps -> "<" + System.String.Join(", ", tps |> List.map toPascalCase) + ">"
                    )
                    (
                        let mutable args = []
                        let rec getArg arg cnt = 
                            if args |> List.contains (if cnt = 0 then arg else arg + string cnt)
                            then getArg arg (cnt + 1)
                            else 
                                let newArg = if cnt = 0 then arg else arg + string cnt
                                args <- args @ [newArg]
                                newArg
                        System.String.Join(
                            ", ", 
                            x.Parameter |> List.map 
                                (fun i -> 
                                    (arrangeType i) + " " + 
                                        (
                                            match i.Name with
                                            | Some name -> getArg name 0
                                            | _ -> getArg "arg" 0
                                        )
                                )
                        )
                    )
                    (
                        if entity.Type = EntityType.Interface then ""
                        else " => throw new NotImplementedException();"
                    )
        )

    printfn "    }\n}\n"

let printEntities (entities: Entity list) = 
    let namespaces = 
        entities 
        |> List.map(fun x -> x.Namespace) 
        |> List.distinct 
        |> List.map (fun x -> x.Split "." |> Array.map toPascalCase |> Array.reduce (append "."))
    entities |> List.iter (printEntity namespaces)
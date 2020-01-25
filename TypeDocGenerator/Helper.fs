module Helper

open Definitions

let rec toPascalCase (str: string) =
    if str.Length = 0
    then str
    else if str.Contains "." then 
         str.Split(".") |> Array.reduce (fun a n -> toPascalCase(a) + "." + toPascalCase(n))
         else str.Substring(0, 1).ToUpper() + str.Substring(1)


let getModifier (flags: ReflectionFlags) = 
    let mutable modifier = []
    match flags.IsPublic with
    | Some flag -> if flag then modifier <- modifier |> List.append [ "public" ] else ()
    | _ -> ()
    match flags.IsAbstract with
    | Some flag -> if flag then modifier <- modifier |> List.append [ "abstract" ] else ()
    | _ -> ()
    match flags.IsPrivate with
    | Some flag -> if flag then modifier <- modifier |> List.append [ "private" ] else ()
    | _ -> ()
    match flags.IsProtected with
    | Some flag -> if flag then modifier <- modifier |> List.append [ "protected" ] else ()
    | _ -> ()
    match flags.IsStatic with
    | Some flag -> if flag then modifier <- modifier |> List.append [ "static" ] else ()
    | _ -> ()
    match modifier with
    | [] -> ""
    | _ -> (modifier |> List.reduce(fun accu next -> accu + " " + next)) + " "

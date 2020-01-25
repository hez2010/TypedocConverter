module Helper

let rec toPascalCase (str: string) =
    if str.Length = 0
    then str
    else if str.Contains "." then 
         str.Split(".") |> Array.reduce (fun a n -> toPascalCase(a) + "." + toPascalCase(n))
         else str.Substring(0, 1).ToUpper() + str.Substring(1)

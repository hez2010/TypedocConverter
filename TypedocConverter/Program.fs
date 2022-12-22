module Program

open System.IO
open Definitions
open Entity
open Helpers
open System.Diagnostics.CodeAnalysis
open System.Text.Json

[<ExcludeFromCodeCoverage>]
let rec parseArguments (state: string) (config: Config) (argv: string list) =
    match argv with
    | (front::tails) ->
        match front.StartsWith "--" with
        | true -> parseArguments ((front.Substring 2).ToLower()) config tails
        | false -> 
            match state with
            | "namespace" -> parseArguments "" { config with Namespace = front } tails
            | "splitfiles" -> parseArguments "" { config with SplitFiles = front = "true" } tails
            | "outputdir" -> parseArguments "" { config with OutputDir = front } tails
            | "outputfile" -> parseArguments "" { config with OutputFile = front } tails
            | "inputfile" -> parseArguments "" { config with InputFile = front } tails
            | "help" -> parseArguments "" { config with Help = true } tails
            | "number-type" -> parseArguments "" { config with NumberType = front } tails
            | "promise-type" -> parseArguments "" { config with UseWinRTPromise = (front.ToLowerInvariant() = "winrt") } tails
            | "any-type" -> parseArguments "" { config with AnyType = front } tails
            | "array-type" -> parseArguments "" { config with ArrayType = front } tails
            | "nrt-disabled" -> parseArguments "" { config with NrtDisabled = front = "true" } tails
            | "json-mode" -> parseArguments "" { config with JsonMode = match front.ToLowerInvariant() with | "both" -> JsonMode.Both | "newtonsoft" -> JsonMode.Newtonsoft | _ -> JsonMode.System } tails
            | _ -> 
                printfn "Not supported argument: --%s %s" state front
                parseArguments "" config tails
    | _ -> 
        if state = "help"
        then { config with Help = true }
        else config

[<ExcludeFromCodeCoverage>]
let printHelp () = 
    printfn "Typedoc Converter Arguments:"
    printfn ""
    printfn "--inputfile [file]: input file"
    printfn "--namespace [namespace]: specify namespace for generated code"
    printfn "--splitfiles [true|false]: whether to split code to different files"
    printfn "--outputdir [path]: used for place code files when splitfiles is true"
    printfn "--outputfile [path]: used for place code file when splitfiles is false"
    printfn "--number-type [int/decimal/double...]: config for number type mapping"
    printfn "--promise-type [CLR/WinRT]: config for promise type mapping, CLR for Task and WinRT for IAsyncAction/IAsyncOperation"
    printfn "--any-type [object/dynamic...]: config for any type mapping"
    printfn "--array-type [Array/IEnumerable/List...]: config for array type mapping"
    printfn "--nrt-disabled [true|false]: whether to disable Nullable Reference Types"
    printfn "--json-mode [system|newtonsoft|both]: whether to use System.Text.Json or Newtonsoft.Json or both"

[<EntryPoint>]
[<ExcludeFromCodeCoverage>]
let main argv =
    let jsonOptions = JsonSerializerOptions()
    jsonOptions.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
    let config = parseArguments "" { Help = false;
                                     InputFile = null; 
                                     Namespace = ""; 
                                     SplitFiles = false; 
                                     OutputDir = "."; 
                                     OutputFile = "TypedocConverter.cs" 
                                     NumberType = "double"
                                     UseWinRTPromise = false
                                     AnyType = "object"
                                     ArrayType = "Array"
                                     NrtDisabled = false
                                     JsonMode = JsonMode.System
                                   } (argv |> List.ofArray)
    if config.Help 
    then 
        printHelp ()
        0
    else
        if isNull config.InputFile
        then
            printError "No input file"
            printHelp ()
            1
        else
            let json = File.ReadAllText config.InputFile
            let root = JsonSerializer.Deserialize<Reflection>(json, jsonOptions)
            let entities = Parser.parseNode config.Namespace root config
            let mutable printedEntities : Entity Set = Set.empty
            let mutable generatedEntites : Entity list = List.empty
            let mutable unionTypes : string list Set = Set.empty
            let mutable finished = false

            let namespaces = 
                entities 
                |> List.map Helpers.getNamespaceAndName
                |> List.collect (fun x -> match x with | Some(v, _) -> [v] | _ -> [])
                |> List.distinct

            let result = 
                if config.SplitFiles 
                then Printer.printEntities config entities namespaces
                else Printer.printEntities config entities namespaces

            generatedEntites <- fst result |> Set.toList
            snd result |> Set.iter (fun x -> unionTypes <- unionTypes |> Set.add x)

            finished <- generatedEntites.IsEmpty
            while not finished do
                let newGenerated = generatedEntites
                let generated = newGenerated |> List.filter (fun x -> not (printedEntities |> Set.contains x))
                for i in generated do 
                    printedEntities <- printedEntities |> Set.add i
                finished <- generated.IsEmpty
                if finished then () else
                    let result = 
                        if config.SplitFiles 
                        then Printer.printEntities config generated namespaces
                        else Printer.printEntities config generated namespaces
                    generatedEntites <- fst result |> Set.toList
                    snd result |> Set.iter (fun x -> unionTypes <- unionTypes |> Set.add x)

            Printer.printUnionTypes config namespaces unionTypes

            printfn "Completed"
            0

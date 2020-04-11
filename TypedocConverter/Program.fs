module Program

open System.IO
open Newtonsoft.Json
open Converters
open Definitions
open Helpers
open System.Diagnostics.CodeAnalysis

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
            | "use-system-json" -> parseArguments "" { config with UseSystemJson = front = "true" } tails
            | _ -> 
                printfn "Not supported argument: --%s %s" state front
                parseArguments "" config tails
    | _ -> 
        if state = "help"
        then { config with Help = true }
        else config

[<ExcludeFromCodeCoverage>]
let printHelp () = 
    printfn "TypeDoc Generator Arguments:"
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
    printfn "--use-system-json [true|false]: whether to use System.Text.Json instead of Newtonsoft.Json"

[<EntryPoint>]
[<ExcludeFromCodeCoverage>]
let main argv =
    let jsonSettings = JsonSerializerSettings()
    jsonSettings.Converters.Add(OptionConverter())
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
                                     UseSystemJson = false
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
            let root = JsonConvert.DeserializeObject<Reflection>(json, jsonSettings)
            let entities = Parser.parseNode config.Namespace root config
            if config.SplitFiles 
            then Printer.printEntities true config.OutputDir config entities
            else Printer.printEntities false config.OutputFile config entities
            printfn "Completed"
            0

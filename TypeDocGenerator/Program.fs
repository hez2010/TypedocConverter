module Program

open System.IO
open Newtonsoft.Json
open Converters
open Definitions

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
            | _ -> 
                printfn "Not supported argument: --%s %s" state front
                parseArguments "" config tails
    | _ -> 
        if state = "help"
        then { config with Help = true }
        else config

let printHelp () = 
    printfn "TypeDoc Generator Arguments:"
    printfn ""
    printfn "--inputfile [file]: input file"
    printfn "--namespace [namespace]: specify namespace for generated code"
    printfn "--splitfiles [true|false]: whether split code to different files"
    printfn "--outputdir [path]: used for place code files when splitfiles is true"
    printfn "--outputfile [path]: used for place code file when splitfiles is false"


let printError (err: string) = 
    let backup = System.Console.ForegroundColor
    System.Console.ForegroundColor <- System.ConsoleColor.Red
    System.Console.Error.WriteLine ("[Error] " + err)
    System.Console.ForegroundColor <- backup


[<EntryPoint>]
let main argv =
    let jsonSettings = JsonSerializerSettings()
    jsonSettings.Converters.Add(OptionConverter())
    let config = parseArguments "" { Help = false; InputFile = null; Namespace = "TypeDocGenerator"; SplitFiles = false; OutputDir = "."; OutputFile = "TypeDocGenerator.cs" } (argv |> List.ofArray)
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
            let entities = Parser.parseNode config.Namespace root
            if config.SplitFiles 
            then Printer.printEntities true config.OutputDir entities
            else Printer.printEntities false config.OutputFile entities
            printfn "Completed"
            0

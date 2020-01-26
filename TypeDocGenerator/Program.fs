module Program

open System.IO
open Newtonsoft.Json
open Converters
open Definitions

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printfn "No input file"
        0
    else
        let jsonSettings = JsonSerializerSettings()
        jsonSettings.Converters.Add(OptionConverter())
        let json = File.ReadAllText argv.[0]
        let root = JsonConvert.DeserializeObject<Reflection>(json, jsonSettings)
        let entities = Parser.parseNode "" root
        Printer.printEntities entities 
        0

module TestHelpers

open Definitions
open Entity
open System.IO
open Xunit
open System
open Newtonsoft.Json
open System.Diagnostics
open Newtonsoft.Json.Serialization
open System.Diagnostics.CodeAnalysis

[<ExcludeFromCodeCoverage>]
let runCodegen fileName = 
    let configFileName = Path.Join(Path.GetTempPath(), "tsconfig.json")
    if File.Exists(configFileName) then ()
    else
        let json = {| CompilerOptions = {| Target = "es2017" |}; Include = [| fileName + ".ts" |] |}
        let jsonSettings = JsonSerializerSettings()
        jsonSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()
        use fs = new StreamWriter(new FileStream(configFileName, FileMode.OpenOrCreate))
        fprintf fs "%s" (JsonConvert.SerializeObject(json, jsonSettings))

    let startupInfo = ProcessStartInfo((if Environment.OSVersion.Platform = PlatformID.Win32NT then "typedoc.cmd" else "typedoc"))
    startupInfo.WorkingDirectory <- Path.GetTempPath()
    startupInfo.ArgumentList.Add (fileName + ".ts")
    startupInfo.ArgumentList.Add ("--json")
    startupInfo.ArgumentList.Add (fileName + ".json")
    use proc = Process.Start startupInfo
    proc.WaitForExitAsync() |> Async.AwaitTask |> Async.RunSynchronously
    proc.ExitCode
    
[<ExcludeFromCodeCoverage>]
let testCode input expected =
    let fileName = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString().Replace("-", ""))
    File.Delete (fileName + ".cs")
    File.Delete (fileName + ".ts")
    File.Delete (fileName + ".json")
    File.WriteAllTextAsync((fileName + ".ts"), input) |> Async.AwaitTask |> Async.RunSynchronously
    let config = { 
        Help = false; 
        InputFile = fileName + ".json";
        Namespace = "";
        SplitFiles = false; 
        OutputDir = ".";
        OutputFile = fileName + ".cs";
        AnyType = "object"; 
        NumberType = "double";
        UseWinRTPromise = false;
        ArrayType = "Array";
        UseSystemJson = false;
        NrtDisabled = false;
    }
    let asciilize str =
        str |> String.filter(fun c -> int c >= 33 && int c <= 126)

    let exitCode = runCodegen fileName

    Assert.Equal(0, exitCode)

    let json = File.ReadAllText config.InputFile
    let jsonSettings = JsonSerializerSettings()
    jsonSettings.Converters.Add(Converters.OptionConverter())
    let root = JsonConvert.DeserializeObject<Reflection>(json, jsonSettings)
    let entities = Parser.parseNode config.Namespace root config
    
    let mutable printedEntities : Entity Set = Set.empty
    let mutable generatedEntites : Entity list = List.empty
    let mutable finished = false

    let namespaces = 
        entities 
        |> List.map Helpers.getNamespaceAndName
        |> List.collect (fun x -> match x with | Some(v, _) -> [v] | _ -> [])
        |> List.distinct

    generatedEntites <- 
        (
            if config.SplitFiles 
            then Printer.printEntities true config.OutputDir config entities namespaces
            else Printer.printEntities false config.OutputFile config entities namespaces
        ) |> Set.toList
    finished <- generatedEntites.IsEmpty
    while not finished do
        let newGenerated = generatedEntites
        let generated = newGenerated |> List.filter (fun x -> not (printedEntities |> Set.contains x))
        for i in generated do 
            printedEntities <- printedEntities |> Set.add i
        finished <- generated.IsEmpty
        if finished then () else
            generatedEntites <-
                (
                    if config.SplitFiles 
                    then Printer.printEntities true config.OutputDir config generated namespaces
                    else Printer.printEntities false config.OutputFile config generated namespaces
                ) |> Set.toList

    let output = File.ReadAllText (fileName + ".cs")
    Assert.Equal(asciilize expected, asciilize output)
    
    File.Delete (fileName + ".cs")
    File.Delete (fileName + ".ts")
    File.Delete (fileName + ".json")
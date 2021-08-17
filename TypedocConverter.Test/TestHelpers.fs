module TestHelpers

open Definitions
open Entity
open System.IO
open Xunit
open System
open System.Diagnostics
open System.Diagnostics.CodeAnalysis
open System.Collections.Generic
open System.Text.Json

type TestComparer() =
    let asciilize str =
        str |> String.filter(fun c -> int c >= 33 && int c <= 126)

    interface IEqualityComparer<string> with
        member this.Equals(x: string, y: string): bool = 
            (asciilize x) = (asciilize y)
        member this.GetHashCode(obj: string): int = 
            (asciilize obj).GetHashCode()


[<ExcludeFromCodeCoverage>]
let runCodegen fileDir fileName = 
    let configFileName = Path.Join(fileDir, "tsconfig.json")
    if File.Exists(configFileName) then ()
    else
        let json = {| CompilerOptions = {| Target = "es2020" |} |}
        let jsonOptions = JsonSerializerOptions()
        jsonOptions.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        use fs = new StreamWriter(new FileStream(configFileName, FileMode.OpenOrCreate))
        fprintf fs "%s" (JsonSerializer.Serialize(json, jsonOptions))

    let startupInfo = ProcessStartInfo((if Environment.OSVersion.Platform = PlatformID.Win32NT then "typedoc.cmd" else "typedoc"))
    startupInfo.WorkingDirectory <- fileDir
    startupInfo.ArgumentList.Add (fileName + ".ts")
    startupInfo.ArgumentList.Add ("--json")
    startupInfo.ArgumentList.Add (fileName + ".json")
    use proc = Process.Start startupInfo
    proc.WaitForExitAsync() |> Async.AwaitTask |> Async.RunSynchronously
    proc.ExitCode
    
[<ExcludeFromCodeCoverage>]
let testCode input expected =
    let fileDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString().Replace("-", ""))
    Directory.CreateDirectory fileDir |> ignore
    let fileName = Path.Join(fileDir, "TypedocConverter")
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

    let exitCode = runCodegen fileDir fileName

    Assert.Equal(0, exitCode)

    let json = File.ReadAllText config.InputFile
    let jsonOptions = JsonSerializerOptions()
    jsonOptions.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
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

    let output = File.ReadAllText (fileName + ".cs")
    Assert.Equal(expected, output, TestComparer())
    
    Directory.Delete(fileDir, true)
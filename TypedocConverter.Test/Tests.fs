namespace TypeDocGenerator.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Helpers
open Definitions
open Entity
open System.IO
open Newtonsoft.Json

[<TestClass>]
type NamingTest () =

    [<TestMethod>]
    [<DataRow("camelCase")>]
    [<DataRow("snake_case")>]
    [<DataRow("kabe-case")>]
    [<DataRow("camelCase.snake_case.kabe-case")>]
    [<DataRow("wTh--aD_asd__as")>]
    [<DataRow("")>]
    member this.TestPascalCaseConverter (input: string) =
        let map = 
            Map.empty<string, string>.
                Add("camelCase", "CamelCase").
                Add("snake_case", "SnakeCase").
                Add("kabe-case", "KabeCase").
                Add("camelCase.snake_case.kabe-case", "CamelCase.SnakeCase.KabeCase").
                Add("wTh--aD_asd__as", "WThADAsdAs").
                Add("", "")
        Assert.AreEqual(map.[input], toPascalCase input)


[<TestClass>]
type ParserTest () =

    [<TestMethod>]
    member this.TestParser () =
        File.Delete "test.output"
        let expected = File.ReadAllText "test.expected"
        let config = { 
            Help = false; 
            InputFile = "test.input";
            Namespace = "TypedocConverter";
            SplitFiles = false; 
            OutputDir = ".";
            OutputFile = "test.output";
            AnyType = "object"; 
            NumberType = "double";
            UseWinRTPromise = false;
            ArrayType = "Array";
            UseSystemJson = false;
            NrtDisabled = false;
        }
        let asciilize str =
            str |> String.filter(fun c -> int c >= 33 && int c <= 126)
        let json = File.ReadAllText config.InputFile
        let jsonSettings = JsonSerializerSettings()
        jsonSettings.Converters.Add(Converters.OptionConverter())
        let root = JsonConvert.DeserializeObject<Reflection>(json, jsonSettings)
        let entities = Parser.parseNode config.Namespace root config
        
        let mutable printedEntities : Entity Set = Set.empty
        let mutable generatedEntites : Entity list = List.empty
        let mutable finished = false

        generatedEntites <- 
            (
                if config.SplitFiles 
                then Printer.printEntities true config.OutputDir config entities
                else Printer.printEntities false config.OutputFile config entities
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
                        then Printer.printEntities true config.OutputDir config generated
                        else Printer.printEntities false config.OutputFile config generated
                    ) |> Set.toList

        let output = File.ReadAllText "test.output"
        Assert.AreEqual(asciilize expected, asciilize output)

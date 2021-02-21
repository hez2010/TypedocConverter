open Helpers
open Definitions
open Entity
open System.IO
open Newtonsoft.Json
open Xunit

[<Theory>]
[<InlineData("camelCase")>]
[<InlineData("snake_case")>]
[<InlineData("kabe-case")>]
[<InlineData("camelCase.snake_case.kabe-case")>]
[<InlineData("wTh--aD_asd__as")>]
[<InlineData("")>]
let TestPascalCaseConverter (input: string) =
    let map = 
        Map.empty<string, string>.
            Add("camelCase", "CamelCase").
            Add("snake_case", "SnakeCase").
            Add("kabe-case", "KabeCase").
            Add("camelCase.snake_case.kabe-case", "CamelCase.SnakeCase.KabeCase").
            Add("wTh--aD_asd__as", "WThADAsdAs").
            Add("", "")
    Assert.Equal(map.[input], toPascalCase input)


[<Fact>]
let TestParser () =
    File.Delete "test.output"
    let expected = File.ReadAllText "test.expected"
    let config = { 
        Help = false; 
        InputFile = "test.input";
        Namespace = "";
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
    Assert.Equal(asciilize expected, asciilize output)

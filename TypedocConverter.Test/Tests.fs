namespace TypeDocGenerator.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Helpers

[<TestClass>]
type TestClass () =

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

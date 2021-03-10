open Helpers
open Xunit
open TestHelpers

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
let InterfaceTest () =
    let code = """interface Test {
    /**
    * string property
    */
    prop1: string,

    /**
    * number property
    */
    prop2: number,

    /**
    * boolean property
    */
    prop3: boolean,

    /**
    * array property
    */
    prop4: number[],
    
    /**
    * delegate property 1
    */
    prop5: (n: number, s: string) => boolean,
    
    /**
    * delegate property 2
    */
    prop6: (n: number, s: string) => void,

    /**
    * simple method 1
    */
    method1(s: string, n: number, b: boolean): void,

    /**
    * simple method 2
    */
    method2(s: string, n: number, b: boolean): boolean,

    /**
    * event
    * @event
    */
    onEvent1(listener: (e: number) => void): void
}
"""
    let expect = """namespace TypedocConverter
{
    interface Test
    {
        /// <summary>
        /// event
        /// </summary>
        event System.Action<double> OnEvent;
        
        /// <summary>
        /// string property
        /// </summary>
        [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        string Prop1 { get; set; }

        /// <summary>
        /// number property
        /// </summary>
        [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        double Prop2 { get; set; }

        /// <summary>
        /// boolean property
        /// </summary>
        [Newtonsoft.Json.JsonProperty("prop3", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        bool Prop3 { get; set; }

        /// <summary>
        /// array property
        /// </summary>
        [Newtonsoft.Json.JsonProperty("prop4", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        double[] Prop4 { get; set; }
        
        /// <summary>
        /// delegate property 1
        /// </summary>
        [Newtonsoft.Json.JsonProperty("prop5", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        System.Func<double, string, bool> Prop5 { get; set; }
        
        /// <summary>
        /// delegate property 2
        /// </summary>
        [Newtonsoft.Json.JsonProperty("prop6", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        System.Action<double, string> Prop6 { get; set; }

        /// <summary>
        /// simple method 1
        /// </summary>
        void Method1(string s, double n, bool b);

        /// <summary>
        /// simple method 2
        /// </summary>
        bool Method2(string s, double n, bool b);
    }
}
"""
    testCode code expect
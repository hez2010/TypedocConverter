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
let testPascalCaseConverter (input: string) =
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
let testSimpleInterface () =
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
    onEvent(listener: (e: number) => void): void
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


[<Fact>]
let testGenericInterface () =
    let code = """interface Test<T> {
    /**
    * @event
    */
    onEvent(listener: (e: T) => void): void
    prop: T,
    method1(v: T): void,
    method2(v: T): T
}
"""
    let expect = """namespace TypedocConverter
{
    interface Test<T>
    {
        /// <summary>
        /// </summary>
        event System.Action<T> OnEvent;
        [Newtonsoft.Json.JsonProperty("prop", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        T Prop { get; set; }
        void Method1(T v);
        T Method2(T v);
    }
}
"""
    testCode code expect

[<Fact>]
let testSimpleEnum () =
    let code = """enum Test { A, B, C, D }"""
    let expect = """namespace TypedocConverter
{
    enum Test
    {
        A = 0, B = 1, C = 2, D = 3
    }
}
"""
    testCode code expect

[<Fact>]
let testReferencedEnum () =
    let code = """enum Test { A, B, C, D = A, E = C }"""
    let expect = """namespace TypedocConverter
{
    enum Test
    {
        A = 0, B = 1, C = 2, D = 0, E = 2
    }
}
"""
    testCode code expect

[<Fact>]
let testSimpleType () =
    let code = """interface Test {
    prop1: number,
    prop2: string,
    prop3: boolean,
    prop4: any
}"""
    let expect = """namespace TypedocConverter
{
    interface Test
    {
        [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        double Prop1 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        string Prop2 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop3", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        bool Prop3 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop4", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        object Prop4 { get; set; }
    }
}"""
    testCode code expect

[<Fact>]
let testLiteralType () = 
    let code = """interface Test {
    prop: { prop1: string, prop2: number, prop3: boolean }
}"""
    let expect = """namespace TypedocConverter
{
    interface Test
    {
        [Newtonsoft.Json.JsonProperty("prop", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        TypedocConverter.GeneratedTypes.LiteralStringProp1DoubleProp2BoolProp3 Prop { get; set; }
    }
}

namespace TypedocConverter.GeneratedTypes
{
    using TypedocConverter;
    interface LiteralStringProp1DoubleProp2BoolProp3 
    {
        [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        string Prop1 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        double Prop2 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop3", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        bool Prop3 { get; set; }
    }
}"""

    testCode code expect

[<Fact>]
let testStringLiteralType () = 
    let code = """interface Test {
    prop: "a" | "b" | "c"
}"""
    let expect = """namespace TypedocConverter
    {
    interface Test
    {
        [Newtonsoft.Json.JsonProperty("prop", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        string Prop { get; set; }
    }
}"""

    testCode code expect
    
[<Fact>]
let testBuiltInType () = 
    let code = """interface Test {
    prop1: BigUint64Array,
    prop2: Uint32Array,
    prop3: Uint16Array,
    prop4: Uint8Array,
    prop5: BigInt64Array,
    prop6: Int32Array,
    prop7: Int16Array,
    prop8: Int8Array,
    prop9: RegExp,
    prop10: Set<number>,
    prop11: Map<number, string>,
    prop12: Promise<Test>,
    prop13: Promise<number>,
    prop14: Promise<Map<number, Promise<string>>>,
}"""

    let expect = """namespace TypedocConverter
{
    interface Test
    {
        [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        ulong[] Prop1 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop10", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        System.Collections.Generic.ISet<double> Prop10 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop11", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        System.Collections.Generic.IDictionary<double, string> Prop11 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop12", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        System.Threading.Tasks.Task<Test> Prop12 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop13", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        System.Threading.Tasks.Task<double> Prop13 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop14", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        System.Threading.Tasks.Task<System.Collections.Generic.IDictionary<double, System.Threading.Tasks.Task<string>>> Prop14 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        uint[] Prop2 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop3", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        ushort[] Prop3 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop4", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        byte[] Prop4 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop5", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        long[] Prop5 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop6", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        int[] Prop6 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop7", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        short[] Prop7 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop8", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        char[] Prop8 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop9", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        string Prop9 { get; set; }
    }
}"""

    testCode code expect

[<Fact>]
let testUnionType () = 
    let code = """interface Test {
    prop1: string | number | boolean,
    prop2: Map<string, boolean> | boolean,
    prop3: Set<string> | boolean,
    prop4: Map<string, number> | Set<string> | boolean,
}"""
    let expect = """namespace TypedocConverter
{
    interface Test
    {
        [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        string Prop1 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        System.Collections.Generic.IDictionary<string, bool> Prop2 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop3", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        System.Collections.Generic.ISet<string> Prop3 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop4", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        System.Collections.Generic.ISet<string> Prop4 { get; set; }
    }
}"""

    testCode code expect
    
[<Fact>]
let testTypeAlias () = 
    let code = """type Alias1 = "a" | "b" | "c";
type Alias2 = "a" | "b" | "c" | number;
interface Test {
    prop1: Alias1,
    prop2: Alias2,
}"""
    let expect = """namespace TypedocConverter
{
    interface Test
    {
        [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        Alias1 Prop1 { get; set; }
        [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        Alias2 Prop2 { get; set; }
    }
}

namespace TypedocConverter
{
    [Newtonsoft.Json.JsonConverter(typeof(Alias1Converter))]
    enum Alias1
    {
        ///<summary>
        /// a
        ///</summary>
        A,
        ///<summary>
        /// b
        ///</summary>
        B,
        ///<summary>
        /// c
        ///</summary>
        C
    }
    class Alias1Converter : Newtonsoft.Json.JsonConverter
    {
        public override bool CanConvert(System.Type t) => t == typeof(Alias1) || t == typeof(Alias1?);
        public override object ReadJson(Newtonsoft.Json.JsonReader reader, System.Type t, object? existingValue, Newtonsoft.Json.JsonSerializer serializer)
            => reader.TokenType switch
            {
                Newtonsoft.Json.JsonToken.String =>
                    serializer.Deserialize<string>(reader) switch
                    {
                        "a" => Alias1.A,
                        "b" => Alias1.B,
                        "c" => Alias1.C,
                        _ => throw new System.NotSupportedException("Cannot unmarshal type Alias1")
                    },
                _ => throw new System.NotSupportedException("Cannot unmarshal type Alias1")
            };
        public override void WriteJson(Newtonsoft.Json.JsonWriter writer, object? untypedValue, Newtonsoft.Json.JsonSerializer serializer)
        {
            if (untypedValue is null) { serializer.Serialize(writer, null); return; }
            var value = (Alias1)untypedValue;
            switch (value)
            {
                case Alias1.A: serializer.Serialize(writer, "a"); return;
                case Alias1.B: serializer.Serialize(writer, "b"); return;
                case Alias1.C: serializer.Serialize(writer, "c"); return;
                default: break;
            }
            throw new System.NotSupportedException("Cannot marshal type Alias1");
        }
    }
}

namespace TypedocConverter
{
    [Newtonsoft.Json.JsonConverter(typeof(Alias2Converter))]
    enum Alias2
    {
        ///<summary>
        /// a
        ///</summary>
        A,
        ///<summary>
        /// b
        ///</summary>
        B,
        ///<summary>
        /// c
        ///</summary>
        C
    }
    class Alias2Converter : Newtonsoft.Json.JsonConverter
    {
        public override bool CanConvert(System.Type t) => t == typeof(Alias2) || t == typeof(Alias2?);
        public override object ReadJson(Newtonsoft.Json.JsonReader reader, System.Type t, object? existingValue, Newtonsoft.Json.JsonSerializer serializer)
            => reader.TokenType switch
            {
                Newtonsoft.Json.JsonToken.String =>
                    serializer.Deserialize<string>(reader) switch
                    {
                        "a" => Alias2.A,
                        "b" => Alias2.B,
                        "c" => Alias2.C,
                        _ => throw new System.NotSupportedException("Cannot unmarshal type Alias2")
                    },
                _ => throw new System.NotSupportedException("Cannot unmarshal type Alias2")
            };
        public override void WriteJson(Newtonsoft.Json.JsonWriter writer, object? untypedValue, Newtonsoft.Json.JsonSerializer serializer)
        {
            if (untypedValue is null) { serializer.Serialize(writer, null); return; }
            var value = (Alias2)untypedValue;
            switch (value)
            {
                case Alias2.A: serializer.Serialize(writer, "a"); return;
                case Alias2.B: serializer.Serialize(writer, "b"); return;
                case Alias2.C: serializer.Serialize(writer, "c"); return;
                default: break;
            }
            throw new System.NotSupportedException("Cannot marshal type Alias2");
        }
    }
}"""

    testCode code expect

[<Fact>]
let testSimpleClass () =
    let code = """class Test {
    /**
    * alternative constructor
    */
    Test(s: string, n: number) { }
    /**
    * string property
    */
    prop1?: string;

    /**
    * number property
    */
    prop2?: number;

    /**
    * boolean property
    */
    prop3?: boolean;

    /**
    * array property
    */
    prop4?: number[];

    /**
    * delegate property 1
    */
    prop5?: (n: number, s: string) => boolean;

    /**
    * delegate property 2
    */
    prop6?: (n: number, s: string) => void;

    /**
    * simple method 1
    */
    method1(s: string, n: number, b: boolean): void { }

    /**
    * simple method 2
    */
    method2(s: string, n: number, b: boolean): boolean { return false; }

    /**
    * event
    * @event
    */
    onEvent(listener: (e: number) => void): void { }
}"""


    let expect = """namespace TypedocConverter
{

    class Test
    {
        public Test() => throw new System.NotImplementedException();

        /// <summary>
        /// event
        /// </summary>
        public event System.Action<double> OnEvent;

        /// <summary>
        /// string property
        /// </summary>
        [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        public string? Prop1 { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

        /// <summary>
        /// number property
        /// </summary>
        [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        public double? Prop2 { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

        /// <summary>
        /// boolean property
        /// </summary>
        [Newtonsoft.Json.JsonProperty("prop3", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        public bool? Prop3 { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

        /// <summary>
        /// array property
        /// </summary>
        [Newtonsoft.Json.JsonProperty("prop4", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        public double[]? Prop4 { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

        /// <summary>
        /// delegate property 1
        /// </summary>
        [Newtonsoft.Json.JsonProperty("prop5", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        public System.Func<double, string, bool>? Prop5 { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

        /// <summary>
        /// delegate property 2
        /// </summary>
        [Newtonsoft.Json.JsonProperty("prop6", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        public System.Action<double, string>? Prop6 { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

        /// <summary>
        /// alternative constructor
        /// </summary>
        public void Test(string s, double n) => throw new System.NotImplementedException();
    
        /// <summary>
        /// simple method 1
        /// </summary>
        public void Method1(string s, double n, bool b) => throw new System.NotImplementedException();

        /// <summary>
        /// simple method 2
        /// </summary>
        public bool Method2(string s, double n, bool b) => throw new System.NotImplementedException();
    }
}"""

    testCode code expect


[<Fact>]
let testStaticMember () =
    let code = """class Test {
    static method(s: string): number { return 5; }
}"""


    let expect = """namespace TypedocConverter
{

    class Test
    {
        public Test() => throw new System.NotImplementedException();
        static double Method(string s) => throw new System.NotImplementedException();
    }
}"""

    testCode code expect


[<Fact>]
let testGetterAndSetter () =
    let code = """class Test {
    #x: string = "test";
    #y: string = "test";
    #z: string = "test";

    get x() {
        return this.#x;
    }

    set x(v: string) {
        this.#x = v;
    }

    get y() {
        return this.#y;
    }

    set z(v: string) {
        this.#z = v;
    }
}"""


    let expect = """namespace TypedocConverter
{

    class Test
    {
        public Test() => throw new System.NotImplementedException();

        private string _x { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); } = "test";

        private string _y { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); } = "test";

        private string _z { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); } = "test";

        [Newtonsoft.Json.JsonProperty("x", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        public string X { get => throw new System.NotImplementedException(); }

        [Newtonsoft.Json.JsonProperty("y", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        public string Y { get => throw new System.NotImplementedException(); }

        [Newtonsoft.Json.JsonProperty("z", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        public void Z { set => throw new System.NotImplementedException(); }
    }
}"""

    testCode code expect


[<Fact>]
let testIndexer () =
    let code = """class Test {
    [v: string]: number;
}"""


    let expect = """namespace TypedocConverter
{

    class Test
    {
        public Test() => throw new System.NotImplementedException();
        public double this[string] { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }
    }
}"""

    testCode code expect
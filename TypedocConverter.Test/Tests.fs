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
            [System.Text.Json.Serialization.JsonPropertyName("prop1")]
            [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            string Prop1 { get; set; }
              
            /// <summary>
            /// number property
            /// </summary>
            [System.Text.Json.Serialization.JsonPropertyName("prop2")]
            [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            double Prop2 { get; set; }
              
            /// <summary>
            /// boolean property
            /// </summary>
            [System.Text.Json.Serialization.JsonPropertyName("prop3")]
            [Newtonsoft.Json.JsonProperty("prop3", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            bool Prop3 { get; set; }
              
            /// <summary>
            /// array property
            /// </summary>
            [System.Text.Json.Serialization.JsonPropertyName("prop4")]
            [Newtonsoft.Json.JsonProperty("prop4", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            double[] Prop4 { get; set; }
              
            /// <summary>
            /// delegate property 1
            /// </summary>
            [System.Text.Json.Serialization.JsonPropertyName("prop5")]
            [Newtonsoft.Json.JsonProperty("prop5", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            System.Func<double, string, bool> Prop5 { get; set; }
              
            /// <summary>
            /// delegate property 2
            /// </summary>
            [System.Text.Json.Serialization.JsonPropertyName("prop6")]
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
            [System.Text.Json.Serialization.JsonPropertyName("prop")]
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
    }
    """
    let expect = """namespace TypedocConverter
    {
        interface Test
        {
            [System.Text.Json.Serialization.JsonPropertyName("prop1")]
            [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            double Prop1 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop2")]
            [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            string Prop2 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop3")]
            [Newtonsoft.Json.JsonProperty("prop3", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            bool Prop3 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop4")]
            [Newtonsoft.Json.JsonProperty("prop4", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            object Prop4 { get; set; }
        }
    }
    """
    testCode code expect

[<Fact>]
let testInlineType () = 
    let code = """interface Test {
        prop: { prop1: string, prop2: number, prop3: boolean }
    }
    """
    let expect = """namespace TypedocConverter
    {
        interface Test
        {
            [System.Text.Json.Serialization.JsonPropertyName("prop")]
            [Newtonsoft.Json.JsonProperty("prop", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            TypedocConverter.GeneratedTypes.LiteralStringProp1DoubleProp2BoolProp3 Prop { get; set; }
        }
    }

    namespace TypedocConverter.GeneratedTypes
    {
        using TypedocConverter;
        interface LiteralStringProp1DoubleProp2BoolProp3 
        {
            [System.Text.Json.Serialization.JsonPropertyName("prop1")]
            [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            string Prop1 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop2")]
            [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            double Prop2 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop3")]
            [Newtonsoft.Json.JsonProperty("prop3", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            bool Prop3 { get; set; }
        }
    }
    """

    testCode code expect

[<Fact>]
let testInlineStringLiteralType () = 
    let code = """interface Test {
        prop: "a" | "b" | "c"
    }
    """
    let expect = """namespace TypedocConverter
    {
        interface Test
        {
            [System.Text.Json.Serialization.JsonPropertyName("prop")]
            [Newtonsoft.Json.JsonProperty("prop", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            string Prop { get; set; }
        }
    }
    """

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
    }
    """

    let expect = """namespace TypedocConverter
    {
        interface Test
        {
            [System.Text.Json.Serialization.JsonPropertyName("prop1")]
            [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            ulong[] Prop1 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop10")]
            [Newtonsoft.Json.JsonProperty("prop10", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            System.Collections.Generic.HashSet<double> Prop10 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop11")]
            [Newtonsoft.Json.JsonProperty("prop11", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            System.Collections.Generic.Dictionary<double, string> Prop11 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop12")]
            [Newtonsoft.Json.JsonProperty("prop12", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            System.Threading.Tasks.Task<Test> Prop12 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop13")]
            [Newtonsoft.Json.JsonProperty("prop13", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            System.Threading.Tasks.Task<double> Prop13 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop14")]
            [Newtonsoft.Json.JsonProperty("prop14", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            System.Threading.Tasks.Task<System.Collections.Generic.Dictionary<double, System.Threading.Tasks.Task<string>>> Prop14 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop2")]
            [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            uint[] Prop2 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop3")]
            [Newtonsoft.Json.JsonProperty("prop3", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            ushort[] Prop3 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop4")]
            [Newtonsoft.Json.JsonProperty("prop4", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            byte[] Prop4 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop5")]
            [Newtonsoft.Json.JsonProperty("prop5", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            long[] Prop5 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop6")]
            [Newtonsoft.Json.JsonProperty("prop6", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            int[] Prop6 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop7")]
            [Newtonsoft.Json.JsonProperty("prop7", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            short[] Prop7 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop8")]
            [Newtonsoft.Json.JsonProperty("prop8", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            char[] Prop8 { get; set; }
            [System.Text.Json.Serialization.JsonPropertyName("prop9")]
            [Newtonsoft.Json.JsonProperty("prop9", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            string Prop9 { get; set; }
        }
    }
    """

    testCode code expect

[<Fact>]
let testUnionType () = 
    let code = """interface Test {
        prop1: string | number | boolean,
        prop2: Map<string, boolean> | boolean,
        prop3: Set<string> | boolean,
        prop4: Map<string, number> | Set<string> | boolean,
    }
    """
    let expect = """namespace TypedocConverter
    {
        interface Test
        {
            [System.Text.Json.Serialization.JsonPropertyName("prop1")]
            [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            TypedocConverter.GeneratedTypes.StringDoubleBoolUnion Prop1 { get; set; }
    
            [System.Text.Json.Serialization.JsonPropertyName("prop2")]
            [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            TypedocConverter.GeneratedTypes.DictionaryString_BoolBoolUnion Prop2 { get; set; }
    
            [System.Text.Json.Serialization.JsonPropertyName("prop3")]
            [Newtonsoft.Json.JsonProperty("prop3", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            TypedocConverter.GeneratedTypes.HashSetStringBoolUnion Prop3 { get; set; }
    
            [System.Text.Json.Serialization.JsonPropertyName("prop4")]
            [Newtonsoft.Json.JsonProperty("prop4", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            TypedocConverter.GeneratedTypes.HashSetStringDictionaryString_DoubleBoolUnion Prop4 { get; set; }
    
        }
    }
    
    namespace TypedocConverter.GeneratedTypes
    {
        using TypedocConverter;
        class DictionaryString_BoolBoolUnionSystemJsonConverter : System.Text.Json.Serialization.JsonConverter<DictionaryString_BoolBoolUnion>
        {
            public override DictionaryString_BoolBoolUnion Read(ref System.Text.Json.Utf8JsonReader reader, System.Type type, System.Text.Json.JsonSerializerOptions options)
            {
                try { return new DictionaryString_BoolBoolUnion { SystemCollectionsGenericDictionaryString_BoolValue = System.Text.Json.JsonSerializer.Deserialize<System.Collections.Generic.Dictionary<string, bool>>(ref reader, options) }; } catch (System.Text.Json.JsonException) { }
                try { return new DictionaryString_BoolBoolUnion { BoolValue = System.Text.Json.JsonSerializer.Deserialize<bool>(ref reader, options) }; } catch (System.Text.Json.JsonException) { }
                return default;
            }
            public override void Write(System.Text.Json.Utf8JsonWriter writer, DictionaryString_BoolBoolUnion value, System.Text.Json.JsonSerializerOptions options)
            {
                if (value.Type == typeof(System.Collections.Generic.Dictionary<string, bool>)) { System.Text.Json.JsonSerializer.Serialize(writer, value.SystemCollectionsGenericDictionaryString_BoolValue, options); return; }
                if (value.Type == typeof(bool)) { System.Text.Json.JsonSerializer.Serialize(writer, value.BoolValue, options); return; }
                writer.WriteNullValue();
            }
        }
        class DictionaryString_BoolBoolUnionNewtonsoftJsonConverter : Newtonsoft.Json.JsonConverter<DictionaryString_BoolBoolUnion>
        {
            public override DictionaryString_BoolBoolUnion ReadJson(Newtonsoft.Json.JsonReader reader, System.Type type, DictionaryString_BoolBoolUnion value, bool hasExistingValue, Newtonsoft.Json.JsonSerializer serializer)
            {
                try { return new DictionaryString_BoolBoolUnion { SystemCollectionsGenericDictionaryString_BoolValue = serializer.Deserialize<System.Collections.Generic.Dictionary<string, bool>>(reader) }; } catch (Newtonsoft.Json.JsonException) { }
                try { return new DictionaryString_BoolBoolUnion { BoolValue = serializer.Deserialize<bool>(reader) }; } catch (Newtonsoft.Json.JsonException) { }
                return default;
            }
            public override void WriteJson(Newtonsoft.Json.JsonWriter writer, DictionaryString_BoolBoolUnion value, Newtonsoft.Json.JsonSerializer serializer)
            {
                if (value.Type == typeof(System.Collections.Generic.Dictionary<string, bool>)) { serializer.Serialize(writer, value.SystemCollectionsGenericDictionaryString_BoolValue); return; }
                if (value.Type == typeof(bool)) { serializer.Serialize(writer, value.BoolValue); return; }
                writer.WriteNull();
            }
        }
        [System.Text.Json.Serialization.JsonConverter(typeof(DictionaryString_BoolBoolUnionSystemJsonConverter))]
        [Newtonsoft.Json.JsonConverter(typeof(DictionaryString_BoolBoolUnionNewtonsoftJsonConverter))]
        struct DictionaryString_BoolBoolUnion
        {
            public System.Type? Type { get; set; }
            private System.Collections.Generic.Dictionary<string, bool>? _systemCollectionsGenericDictionaryString_BoolValue;
            public System.Collections.Generic.Dictionary<string, bool>? SystemCollectionsGenericDictionaryString_BoolValue
            {
                get => _systemCollectionsGenericDictionaryString_BoolValue;
                set
                {
                    ClearValue();
                    _systemCollectionsGenericDictionaryString_BoolValue = value;
                    Type = typeof(System.Collections.Generic.Dictionary<string, bool>);
                }
            }
            public static implicit operator DictionaryString_BoolBoolUnion(System.Collections.Generic.Dictionary<string, bool> value) => new DictionaryString_BoolBoolUnion { SystemCollectionsGenericDictionaryString_BoolValue = value };
            public static implicit operator System.Collections.Generic.Dictionary<string, bool>?(DictionaryString_BoolBoolUnion value) => value.SystemCollectionsGenericDictionaryString_BoolValue;
    
            private bool? _boolValue;
            public bool? BoolValue
            {
                get => _boolValue;
                set
                {
                    ClearValue();
                    _boolValue = value;
                    Type = typeof(bool);
                }
            }
            public static implicit operator DictionaryString_BoolBoolUnion(bool value) => new DictionaryString_BoolBoolUnion { BoolValue = value };
            public static implicit operator bool?(DictionaryString_BoolBoolUnion value) => value.BoolValue;
    
            public override string? ToString()
            {
                if (Type == typeof(System.Collections.Generic.Dictionary<string, bool>)) return SystemCollectionsGenericDictionaryString_BoolValue?.ToString();
                if (Type == typeof(bool)) return BoolValue?.ToString();
                return default;
            }
            public override int GetHashCode()
            {
                if (Type == typeof(System.Collections.Generic.Dictionary<string, bool>)) return SystemCollectionsGenericDictionaryString_BoolValue?.GetHashCode() ?? 0;
                if (Type == typeof(bool)) return BoolValue?.GetHashCode() ?? 0;
                return 0;
            }
            private void ClearValue()
            {
                _systemCollectionsGenericDictionaryString_BoolValue = default;
                _boolValue = default;
            }
        }
    }
    namespace TypedocConverter.GeneratedTypes
    {
        using TypedocConverter;
        class HashSetStringDictionaryString_DoubleBoolUnionSystemJsonConverter : System.Text.Json.Serialization.JsonConverter<HashSetStringDictionaryString_DoubleBoolUnion>
        {
            public override HashSetStringDictionaryString_DoubleBoolUnion Read(ref System.Text.Json.Utf8JsonReader reader, System.Type type, System.Text.Json.JsonSerializerOptions options)
            {
                try { return new HashSetStringDictionaryString_DoubleBoolUnion { SystemCollectionsGenericHashSetStringValue = System.Text.Json.JsonSerializer.Deserialize<System.Collections.Generic.HashSet<string>>(ref reader, options) }; } catch (System.Text.Json.JsonException) { }
                try { return new HashSetStringDictionaryString_DoubleBoolUnion { SystemCollectionsGenericDictionaryString_DoubleValue = System.Text.Json.JsonSerializer.Deserialize<System.Collections.Generic.Dictionary<string, double>>(ref reader, options) }; } catch (System.Text.Json.JsonException) { }
                try { return new HashSetStringDictionaryString_DoubleBoolUnion { BoolValue = System.Text.Json.JsonSerializer.Deserialize<bool>(ref reader, options) }; } catch (System.Text.Json.JsonException) { }
                return default;
            }
            public override void Write(System.Text.Json.Utf8JsonWriter writer, HashSetStringDictionaryString_DoubleBoolUnion value, System.Text.Json.JsonSerializerOptions options)
            {
                if (value.Type == typeof(System.Collections.Generic.HashSet<string>)) { System.Text.Json.JsonSerializer.Serialize(writer, value.SystemCollectionsGenericHashSetStringValue, options); return; }
                if (value.Type == typeof(System.Collections.Generic.Dictionary<string, double>)) { System.Text.Json.JsonSerializer.Serialize(writer, value.SystemCollectionsGenericDictionaryString_DoubleValue, options); return; }
                if (value.Type == typeof(bool)) { System.Text.Json.JsonSerializer.Serialize(writer, value.BoolValue, options); return; }
                writer.WriteNullValue();
            }
        }
        class HashSetStringDictionaryString_DoubleBoolUnionNewtonsoftJsonConverter : Newtonsoft.Json.JsonConverter<HashSetStringDictionaryString_DoubleBoolUnion>
        {
            public override HashSetStringDictionaryString_DoubleBoolUnion ReadJson(Newtonsoft.Json.JsonReader reader, System.Type type, HashSetStringDictionaryString_DoubleBoolUnion value, bool hasExistingValue, Newtonsoft.Json.JsonSerializer serializer)
            {
                try { return new HashSetStringDictionaryString_DoubleBoolUnion { SystemCollectionsGenericHashSetStringValue = serializer.Deserialize<System.Collections.Generic.HashSet<string>>(reader) }; } catch (Newtonsoft.Json.JsonException) { }
                try { return new HashSetStringDictionaryString_DoubleBoolUnion { SystemCollectionsGenericDictionaryString_DoubleValue = serializer.Deserialize<System.Collections.Generic.Dictionary<string, double>>(reader) }; } catch (Newtonsoft.Json.JsonException) { }
                try { return new HashSetStringDictionaryString_DoubleBoolUnion { BoolValue = serializer.Deserialize<bool>(reader) }; } catch (Newtonsoft.Json.JsonException) { }
                return default;
            }
            public override void WriteJson(Newtonsoft.Json.JsonWriter writer, HashSetStringDictionaryString_DoubleBoolUnion value, Newtonsoft.Json.JsonSerializer serializer)
            {
                if (value.Type == typeof(System.Collections.Generic.HashSet<string>)) { serializer.Serialize(writer, value.SystemCollectionsGenericHashSetStringValue); return; }
                if (value.Type == typeof(System.Collections.Generic.Dictionary<string, double>)) { serializer.Serialize(writer, value.SystemCollectionsGenericDictionaryString_DoubleValue); return; }
                if (value.Type == typeof(bool)) { serializer.Serialize(writer, value.BoolValue); return; }
                writer.WriteNull();
            }
        }
        [System.Text.Json.Serialization.JsonConverter(typeof(HashSetStringDictionaryString_DoubleBoolUnionSystemJsonConverter))]
        [Newtonsoft.Json.JsonConverter(typeof(HashSetStringDictionaryString_DoubleBoolUnionNewtonsoftJsonConverter))]
        struct HashSetStringDictionaryString_DoubleBoolUnion
        {
            public System.Type? Type { get; set; }
            private System.Collections.Generic.HashSet<string>? _systemCollectionsGenericHashSetStringValue;
            public System.Collections.Generic.HashSet<string>? SystemCollectionsGenericHashSetStringValue
            {
                get => _systemCollectionsGenericHashSetStringValue;
                set
                {
                    ClearValue();
                    _systemCollectionsGenericHashSetStringValue = value;
                    Type = typeof(System.Collections.Generic.HashSet<string>);
                }
            }
            public static implicit operator HashSetStringDictionaryString_DoubleBoolUnion(System.Collections.Generic.HashSet<string> value) => new HashSetStringDictionaryString_DoubleBoolUnion { SystemCollectionsGenericHashSetStringValue = value };
            public static implicit operator System.Collections.Generic.HashSet<string>?(HashSetStringDictionaryString_DoubleBoolUnion value) => value.SystemCollectionsGenericHashSetStringValue;
    
            private System.Collections.Generic.Dictionary<string, double>? _systemCollectionsGenericDictionaryString_DoubleValue;
            public System.Collections.Generic.Dictionary<string, double>? SystemCollectionsGenericDictionaryString_DoubleValue
            {
                get => _systemCollectionsGenericDictionaryString_DoubleValue;
                set
                {
                    ClearValue();
                    _systemCollectionsGenericDictionaryString_DoubleValue = value;
                    Type = typeof(System.Collections.Generic.Dictionary<string, double>);
                }
            }
            public static implicit operator HashSetStringDictionaryString_DoubleBoolUnion(System.Collections.Generic.Dictionary<string, double> value) => new HashSetStringDictionaryString_DoubleBoolUnion { SystemCollectionsGenericDictionaryString_DoubleValue = value };
            public static implicit operator System.Collections.Generic.Dictionary<string, double>?(HashSetStringDictionaryString_DoubleBoolUnion value) => value.SystemCollectionsGenericDictionaryString_DoubleValue;
    
            private bool? _boolValue;
            public bool? BoolValue
            {
                get => _boolValue;
                set
                {
                    ClearValue();
                    _boolValue = value;
                    Type = typeof(bool);
                }
            }
            public static implicit operator HashSetStringDictionaryString_DoubleBoolUnion(bool value) => new HashSetStringDictionaryString_DoubleBoolUnion { BoolValue = value };
            public static implicit operator bool?(HashSetStringDictionaryString_DoubleBoolUnion value) => value.BoolValue;
    
            public override string? ToString()
            {
                if (Type == typeof(System.Collections.Generic.HashSet<string>)) return SystemCollectionsGenericHashSetStringValue?.ToString();
                if (Type == typeof(System.Collections.Generic.Dictionary<string, double>)) return SystemCollectionsGenericDictionaryString_DoubleValue?.ToString();
                if (Type == typeof(bool)) return BoolValue?.ToString();
                return default;
            }
            public override int GetHashCode()
            {
                if (Type == typeof(System.Collections.Generic.HashSet<string>)) return SystemCollectionsGenericHashSetStringValue?.GetHashCode() ?? 0;
                if (Type == typeof(System.Collections.Generic.Dictionary<string, double>)) return SystemCollectionsGenericDictionaryString_DoubleValue?.GetHashCode() ?? 0;
                if (Type == typeof(bool)) return BoolValue?.GetHashCode() ?? 0;
                return 0;
            }
            private void ClearValue()
            {
                _systemCollectionsGenericHashSetStringValue = default;
                _systemCollectionsGenericDictionaryString_DoubleValue = default;
                _boolValue = default;
            }
        }
    }
    namespace TypedocConverter.GeneratedTypes
    {
        using TypedocConverter;
        class HashSetStringBoolUnionSystemJsonConverter : System.Text.Json.Serialization.JsonConverter<HashSetStringBoolUnion>
        {
            public override HashSetStringBoolUnion Read(ref System.Text.Json.Utf8JsonReader reader, System.Type type, System.Text.Json.JsonSerializerOptions options)
            {
                try { return new HashSetStringBoolUnion { SystemCollectionsGenericHashSetStringValue = System.Text.Json.JsonSerializer.Deserialize<System.Collections.Generic.HashSet<string>>(ref reader, options) }; } catch (System.Text.Json.JsonException) { }
                try { return new HashSetStringBoolUnion { BoolValue = System.Text.Json.JsonSerializer.Deserialize<bool>(ref reader, options) }; } catch (System.Text.Json.JsonException) { }
                return default;
            }
            public override void Write(System.Text.Json.Utf8JsonWriter writer, HashSetStringBoolUnion value, System.Text.Json.JsonSerializerOptions options)
            {
                if (value.Type == typeof(System.Collections.Generic.HashSet<string>)) { System.Text.Json.JsonSerializer.Serialize(writer, value.SystemCollectionsGenericHashSetStringValue, options); return; }
                if (value.Type == typeof(bool)) { System.Text.Json.JsonSerializer.Serialize(writer, value.BoolValue, options); return; }
                writer.WriteNullValue();
            }
        }
        class HashSetStringBoolUnionNewtonsoftJsonConverter : Newtonsoft.Json.JsonConverter<HashSetStringBoolUnion>
        {
            public override HashSetStringBoolUnion ReadJson(Newtonsoft.Json.JsonReader reader, System.Type type, HashSetStringBoolUnion value, bool hasExistingValue, Newtonsoft.Json.JsonSerializer serializer)
            {
                try { return new HashSetStringBoolUnion { SystemCollectionsGenericHashSetStringValue = serializer.Deserialize<System.Collections.Generic.HashSet<string>>(reader) }; } catch (Newtonsoft.Json.JsonException) { }
                try { return new HashSetStringBoolUnion { BoolValue = serializer.Deserialize<bool>(reader) }; } catch (Newtonsoft.Json.JsonException) { }
                return default;
            }
            public override void WriteJson(Newtonsoft.Json.JsonWriter writer, HashSetStringBoolUnion value, Newtonsoft.Json.JsonSerializer serializer)
            {
                if (value.Type == typeof(System.Collections.Generic.HashSet<string>)) { serializer.Serialize(writer, value.SystemCollectionsGenericHashSetStringValue); return; }
                if (value.Type == typeof(bool)) { serializer.Serialize(writer, value.BoolValue); return; }
                writer.WriteNull();
            }
        }
        [System.Text.Json.Serialization.JsonConverter(typeof(HashSetStringBoolUnionSystemJsonConverter))]
        [Newtonsoft.Json.JsonConverter(typeof(HashSetStringBoolUnionNewtonsoftJsonConverter))]
        struct HashSetStringBoolUnion
        {
            public System.Type? Type { get; set; }
            private System.Collections.Generic.HashSet<string>? _systemCollectionsGenericHashSetStringValue;
            public System.Collections.Generic.HashSet<string>? SystemCollectionsGenericHashSetStringValue
            {
                get => _systemCollectionsGenericHashSetStringValue;
                set
                {
                    ClearValue();
                    _systemCollectionsGenericHashSetStringValue = value;
                    Type = typeof(System.Collections.Generic.HashSet<string>);
                }
            }
            public static implicit operator HashSetStringBoolUnion(System.Collections.Generic.HashSet<string> value) => new HashSetStringBoolUnion { SystemCollectionsGenericHashSetStringValue = value };
            public static implicit operator System.Collections.Generic.HashSet<string>?(HashSetStringBoolUnion value) => value.SystemCollectionsGenericHashSetStringValue;
    
            private bool? _boolValue;
            public bool? BoolValue
            {
                get => _boolValue;
                set
                {
                    ClearValue();
                    _boolValue = value;
                    Type = typeof(bool);
                }
            }
            public static implicit operator HashSetStringBoolUnion(bool value) => new HashSetStringBoolUnion { BoolValue = value };
            public static implicit operator bool?(HashSetStringBoolUnion value) => value.BoolValue;
    
            public override string? ToString()
            {
                if (Type == typeof(System.Collections.Generic.HashSet<string>)) return SystemCollectionsGenericHashSetStringValue?.ToString();
                if (Type == typeof(bool)) return BoolValue?.ToString();
                return default;
            }
            public override int GetHashCode()
            {
                if (Type == typeof(System.Collections.Generic.HashSet<string>)) return SystemCollectionsGenericHashSetStringValue?.GetHashCode() ?? 0;
                if (Type == typeof(bool)) return BoolValue?.GetHashCode() ?? 0;
                return 0;
            }
            private void ClearValue()
            {
                _systemCollectionsGenericHashSetStringValue = default;
                _boolValue = default;
            }
        }
    }
    namespace TypedocConverter.GeneratedTypes
    {
        using TypedocConverter;
        class StringDoubleBoolUnionSystemJsonConverter : System.Text.Json.Serialization.JsonConverter<StringDoubleBoolUnion>
        {
            public override StringDoubleBoolUnion Read(ref System.Text.Json.Utf8JsonReader reader, System.Type type, System.Text.Json.JsonSerializerOptions options)
            {
                try { return new StringDoubleBoolUnion { StringValue = System.Text.Json.JsonSerializer.Deserialize<string>(ref reader, options) }; } catch (System.Text.Json.JsonException) { }
                try { return new StringDoubleBoolUnion { DoubleValue = System.Text.Json.JsonSerializer.Deserialize<double>(ref reader, options) }; } catch (System.Text.Json.JsonException) { }
                try { return new StringDoubleBoolUnion { BoolValue = System.Text.Json.JsonSerializer.Deserialize<bool>(ref reader, options) }; } catch (System.Text.Json.JsonException) { }
                return default;
            }
            public override void Write(System.Text.Json.Utf8JsonWriter writer, StringDoubleBoolUnion value, System.Text.Json.JsonSerializerOptions options)
            {
                if (value.Type == typeof(string)) { System.Text.Json.JsonSerializer.Serialize(writer, value.StringValue, options); return; }
                if (value.Type == typeof(double)) { System.Text.Json.JsonSerializer.Serialize(writer, value.DoubleValue, options); return; }
                if (value.Type == typeof(bool)) { System.Text.Json.JsonSerializer.Serialize(writer, value.BoolValue, options); return; }
                writer.WriteNullValue();
            }
        }
        class StringDoubleBoolUnionNewtonsoftJsonConverter : Newtonsoft.Json.JsonConverter<StringDoubleBoolUnion>
        {
            public override StringDoubleBoolUnion ReadJson(Newtonsoft.Json.JsonReader reader, System.Type type, StringDoubleBoolUnion value, bool hasExistingValue, Newtonsoft.Json.JsonSerializer serializer)
            {
                try { return new StringDoubleBoolUnion { StringValue = serializer.Deserialize<string>(reader) }; } catch (Newtonsoft.Json.JsonException) { }
                try { return new StringDoubleBoolUnion { DoubleValue = serializer.Deserialize<double>(reader) }; } catch (Newtonsoft.Json.JsonException) { }
                try { return new StringDoubleBoolUnion { BoolValue = serializer.Deserialize<bool>(reader) }; } catch (Newtonsoft.Json.JsonException) { }
                return default;
            }
            public override void WriteJson(Newtonsoft.Json.JsonWriter writer, StringDoubleBoolUnion value, Newtonsoft.Json.JsonSerializer serializer)
            {
                if (value.Type == typeof(string)) { serializer.Serialize(writer, value.StringValue); return; }
                if (value.Type == typeof(double)) { serializer.Serialize(writer, value.DoubleValue); return; }
                if (value.Type == typeof(bool)) { serializer.Serialize(writer, value.BoolValue); return; }
                writer.WriteNull();
            }
        }
        [System.Text.Json.Serialization.JsonConverter(typeof(StringDoubleBoolUnionSystemJsonConverter))]
        [Newtonsoft.Json.JsonConverter(typeof(StringDoubleBoolUnionNewtonsoftJsonConverter))]
        struct StringDoubleBoolUnion
        {
            public System.Type? Type { get; set; }
            private string? _stringValue;
            public string? StringValue
            {
                get => _stringValue;
                set
                {
                    ClearValue();
                    _stringValue = value;
                    Type = typeof(string);
                }
            }
            public static implicit operator StringDoubleBoolUnion(string value) => new StringDoubleBoolUnion { StringValue = value };
            public static implicit operator string?(StringDoubleBoolUnion value) => value.StringValue;
    
            private double? _doubleValue;
            public double? DoubleValue
            {
                get => _doubleValue;
                set
                {
                    ClearValue();
                    _doubleValue = value;
                    Type = typeof(double);
                }
            }
            public static implicit operator StringDoubleBoolUnion(double value) => new StringDoubleBoolUnion { DoubleValue = value };
            public static implicit operator double?(StringDoubleBoolUnion value) => value.DoubleValue;
    
            private bool? _boolValue;
            public bool? BoolValue
            {
                get => _boolValue;
                set
                {
                    ClearValue();
                    _boolValue = value;
                    Type = typeof(bool);
                }
            }
            public static implicit operator StringDoubleBoolUnion(bool value) => new StringDoubleBoolUnion { BoolValue = value };
            public static implicit operator bool?(StringDoubleBoolUnion value) => value.BoolValue;
    
            public override string? ToString()
            {
                if (Type == typeof(string)) return StringValue?.ToString();
                if (Type == typeof(double)) return DoubleValue?.ToString();
                if (Type == typeof(bool)) return BoolValue?.ToString();
                return default;
            }
            public override int GetHashCode()
            {
                if (Type == typeof(string)) return StringValue?.GetHashCode() ?? 0;
                if (Type == typeof(double)) return DoubleValue?.GetHashCode() ?? 0;
                if (Type == typeof(bool)) return BoolValue?.GetHashCode() ?? 0;
                return 0;
            }
            private void ClearValue()
            {
                _stringValue = default;
                _doubleValue = default;
                _boolValue = default;
            }
        }
    }
    """

    testCode code expect
    
[<Fact>]
let testTypeAlias () = 
    let code = """type Alias1 = "a" | "b" | "c";
    type Alias2 = "a" | "b" | "c" | number;
    interface Test {
        prop1: Alias1,
        prop2: Alias2,
    }
    """
    let expect = """namespace TypedocConverter
    {
              
        interface Test
        {
            [System.Text.Json.Serialization.JsonPropertyName("prop1")]
            [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            Alias1 Prop1 { get; set; }
              
            [System.Text.Json.Serialization.JsonPropertyName("prop2")]
            [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            Alias2 Prop2 { get; set; }
              
        }
    }
    
    namespace TypedocConverter
    {
              
        [System.Text.Json.Serialization.JsonConverter(typeof(Alias1SystemJsonConverter))]
        [Newtonsoft.Json.JsonConverter(typeof(Alias1NewtonsoftJsonConverter))]
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
              
        class Alias1SystemJsonConverter : System.Text.Json.Serialization.JsonConverter<Alias1>
        {
            public override Alias1 Read(ref System.Text.Json.Utf8JsonReader reader, System.Type typeToConvert, System.Text.Json.JsonSerializerOptions options)
                => reader.TokenType switch
                {
                    System.Text.Json.JsonTokenType.String =>
                        System.Text.Json.JsonSerializer.Deserialize<string>(ref reader, options) switch
                        {
                            "a" => Alias1.A,
                            "b" => Alias1.B,
                            "c" => Alias1.C,
                            _ => throw new System.NotSupportedException("Cannot unmarshal type Alias1")
                        },
                    _ => throw new System.NotSupportedException("Cannot unmarshal type Alias1")
                };
              
            public override void Write(System.Text.Json.Utf8JsonWriter writer, [System.Diagnostics.CodeAnalysis.DisallowNull] Alias1 value, System.Text.Json.JsonSerializerOptions options)
            {
                switch (value)
                {
                    case Alias1.A: System.Text.Json.JsonSerializer.Serialize<string>(writer, "a", options); return;
                    case Alias1.B: System.Text.Json.JsonSerializer.Serialize<string>(writer, "b", options); return;
                    case Alias1.C: System.Text.Json.JsonSerializer.Serialize<string>(writer, "c", options); return;
                    default: break;
                }
                throw new System.NotSupportedException("Cannot marshal type Alias1");
            }
        }
        class Alias1NewtonsoftJsonConverter : Newtonsoft.Json.JsonConverter
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
              
        [System.Text.Json.Serialization.JsonConverter(typeof(Alias2SystemJsonConverter))]
        [Newtonsoft.Json.JsonConverter(typeof(Alias2NewtonsoftJsonConverter))]
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
              
        class Alias2SystemJsonConverter : System.Text.Json.Serialization.JsonConverter<Alias2>
        {
            public override Alias2 Read(ref System.Text.Json.Utf8JsonReader reader, System.Type typeToConvert, System.Text.Json.JsonSerializerOptions options)
                => reader.TokenType switch
                {
                    System.Text.Json.JsonTokenType.String =>
                        System.Text.Json.JsonSerializer.Deserialize<string>(ref reader, options) switch
                        {
                            "a" => Alias2.A,
                            "b" => Alias2.B,
                            "c" => Alias2.C,
                            _ => throw new System.NotSupportedException("Cannot unmarshal type Alias2")
                        },
                    _ => throw new System.NotSupportedException("Cannot unmarshal type Alias2")
                };
              
            public override void Write(System.Text.Json.Utf8JsonWriter writer, [System.Diagnostics.CodeAnalysis.DisallowNull] Alias2 value, System.Text.Json.JsonSerializerOptions options)
            {
                switch (value)
                {
                    case Alias2.A: System.Text.Json.JsonSerializer.Serialize<string>(writer, "a", options); return;
                    case Alias2.B: System.Text.Json.JsonSerializer.Serialize<string>(writer, "b", options); return;
                    case Alias2.C: System.Text.Json.JsonSerializer.Serialize<string>(writer, "c", options); return;
                    default: break;
                }
                throw new System.NotSupportedException("Cannot marshal type Alias2");
            }
        }
        class Alias2NewtonsoftJsonConverter : Newtonsoft.Json.JsonConverter
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
    }
    """

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
    }
    """


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
            [System.Text.Json.Serialization.JsonPropertyName("prop1")]
            [Newtonsoft.Json.JsonProperty("prop1", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            public string? Prop1 { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

            /// <summary>
            /// number property
            /// </summary>
            [System.Text.Json.Serialization.JsonPropertyName("prop2")]
            [Newtonsoft.Json.JsonProperty("prop2", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            public double? Prop2 { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

            /// <summary>
            /// boolean property
            /// </summary>
            [System.Text.Json.Serialization.JsonPropertyName("prop3")]
            [Newtonsoft.Json.JsonProperty("prop3", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            public bool? Prop3 { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

            /// <summary>
            /// array property
            /// </summary>
            [System.Text.Json.Serialization.JsonPropertyName("prop4")]
            [Newtonsoft.Json.JsonProperty("prop4", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            public double[]? Prop4 { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

            /// <summary>
            /// delegate property 1
            /// </summary>
            [System.Text.Json.Serialization.JsonPropertyName("prop5")]
            [Newtonsoft.Json.JsonProperty("prop5", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            public System.Func<double, string, bool>? Prop5 { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

            /// <summary>
            /// delegate property 2
            /// </summary>
            [System.Text.Json.Serialization.JsonPropertyName("prop6")]
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
    }
    """

    testCode code expect


[<Fact>]
let testStaticMember () =
    let code = """class Test {
        static method(s: string): number { return 5; }
    }
    """


    let expect = """namespace TypedocConverter
    {

        class Test
        {
            public Test() => throw new System.NotImplementedException();
            static double Method(string s) => throw new System.NotImplementedException();
        }
    }
    """

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
    }
    """


    let expect = """namespace TypedocConverter
    {

        class Test
        {
            public Test() => throw new System.NotImplementedException();

            private string _x { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); } = "test";

            private string _y { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); } = "test";

            private string _z { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); } = "test";

            [System.Text.Json.Serialization.JsonPropertyName("x")]
            [Newtonsoft.Json.JsonProperty("x", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            public string X { get => throw new System.NotImplementedException(); }

            [System.Text.Json.Serialization.JsonPropertyName("y")]
            [Newtonsoft.Json.JsonProperty("y", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            public string Y { get => throw new System.NotImplementedException(); }

            [System.Text.Json.Serialization.JsonPropertyName("z")]
            [Newtonsoft.Json.JsonProperty("z", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            public void Z { set => throw new System.NotImplementedException(); }
        }
    }
    """

    testCode code expect


[<Fact>]
let testIndexer () =
    let code = """class Test {
        [v: string]: number;
    }
    """


    let expect = """namespace TypedocConverter
    {

        class Test
        {
            public Test() => throw new System.NotImplementedException();
            public double this[string v] { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }
        }
    }
    """

    testCode code expect

[<Fact>]
let testStringLiteralType () =
    let code = """export type BuiltinTheme = 'vs' | 'vs-dark' | 'hc-black';"""

    let expect = """namespace TypedocConverter
    {
              
        [System.Text.Json.Serialization.JsonConverter(typeof(BuiltinThemeSystemJsonConverter))]
        [Newtonsoft.Json.JsonConverter(typeof(BuiltinThemeNewtonsoftJsonConverter))]
        enum BuiltinTheme
        {
            ///<summary>
            /// vs
            ///</summary>
            Vs,
            ///<summary>
            /// vs-dark
            ///</summary>
            VsDark,
            ///<summary>
            /// hc-black
            ///</summary>
            HcBlack
        }
              
        class BuiltinThemeSystemJsonConverter : System.Text.Json.Serialization.JsonConverter<BuiltinTheme>
        {
            public override BuiltinTheme Read(ref System.Text.Json.Utf8JsonReader reader, System.Type typeToConvert, System.Text.Json.JsonSerializerOptions options)
                => reader.TokenType switch
                {
                    System.Text.Json.JsonTokenType.String =>
                        System.Text.Json.JsonSerializer.Deserialize<string>(ref reader, options) switch
                        {
                            "vs" => BuiltinTheme.Vs,
                            "vs-dark" => BuiltinTheme.VsDark,
                            "hc-black" => BuiltinTheme.HcBlack,
                            _ => throw new System.NotSupportedException("Cannot unmarshal type BuiltinTheme")
                        },
                    _ => throw new System.NotSupportedException("Cannot unmarshal type BuiltinTheme")
                };
              
            public override void Write(System.Text.Json.Utf8JsonWriter writer, [System.Diagnostics.CodeAnalysis.DisallowNull] BuiltinTheme value, System.Text.Json.JsonSerializerOptions options)
            {
                switch (value)
                {
                    case BuiltinTheme.Vs: System.Text.Json.JsonSerializer.Serialize<string>(writer, "vs", options); return;
                    case BuiltinTheme.VsDark: System.Text.Json.JsonSerializer.Serialize<string>(writer, "vs-dark", options); return;
                    case BuiltinTheme.HcBlack: System.Text.Json.JsonSerializer.Serialize<string>(writer, "hc-black", options); return;
                    default: break;
                }
                throw new System.NotSupportedException("Cannot marshal type BuiltinTheme");
            }
        }
        class BuiltinThemeNewtonsoftJsonConverter : Newtonsoft.Json.JsonConverter
        {
            public override bool CanConvert(System.Type t) => t == typeof(BuiltinTheme) || t == typeof(BuiltinTheme?);
              
            public override object ReadJson(Newtonsoft.Json.JsonReader reader, System.Type t, object? existingValue, Newtonsoft.Json.JsonSerializer serializer)
                => reader.TokenType switch
                {
                    Newtonsoft.Json.JsonToken.String =>
                        serializer.Deserialize<string>(reader) switch
                        {
                            "vs" => BuiltinTheme.Vs,
                            "vs-dark" => BuiltinTheme.VsDark,
                            "hc-black" => BuiltinTheme.HcBlack,
                            _ => throw new System.NotSupportedException("Cannot unmarshal type BuiltinTheme")
                        },
                    _ => throw new System.NotSupportedException("Cannot unmarshal type BuiltinTheme")
                };
              
            public override void WriteJson(Newtonsoft.Json.JsonWriter writer, object? untypedValue, Newtonsoft.Json.JsonSerializer serializer)
            {
                if (untypedValue is null) { serializer.Serialize(writer, null); return; }
                var value = (BuiltinTheme)untypedValue;
                switch (value)
                {
                    case BuiltinTheme.Vs: serializer.Serialize(writer, "vs"); return;
                    case BuiltinTheme.VsDark: serializer.Serialize(writer, "vs-dark"); return;
                    case BuiltinTheme.HcBlack: serializer.Serialize(writer, "hc-black"); return;
                    default: break;
                }
                throw new System.NotSupportedException("Cannot marshal type BuiltinTheme");
            }
        }
    }
    """
    
    testCode code expect

[<Fact>]
let testMemberNameSantization () =
    let code = """export interface Foo {
        $ref?: string;
    }"""

    let expect = """namespace TypedocConverter
    {
        interface Foo
        {
            [System.Text.Json.Serialization.JsonPropertyName("$ref")]
            [Newtonsoft.Json.JsonProperty("$ref", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
            string? Ref { get; set; }
        }
    }
    """

    testCode code expect

[<Fact>]
let testParameterNameSantization () =
    let code = """export interface Foo {
        method($ref: number): number
    }"""

    let expect = """namespace TypedocConverter
    {
        interface Foo
        {
            double Method(double _ref);  
        }
    }
    """

    testCode code expect
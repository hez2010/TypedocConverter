# Typedoc Converter
This is a [typedoc](https://github.com/TypeStrong/typedoc) json to C# bindings converter. Can be used as a TypeScript to C# bindings converter. 

Build status: ![.NET](https://github.com/hez2010/TypedocConverter/workflows/.NET/badge.svg)

## Compatibility Status
- typedoc: 0.20
- TypeScript: 3.9, 4.0, 4.1, 4.2

## Languages support
- [x] Enums
  - [x] Direct value
  - [x] Referenced value
- [x] Interfaces
  - [x] Inherits
  - [x] Generics
  - [ ] Generics Constaints
  - [x] Properties
  - [x] Methods
  - [x] Events
- [x] Classes
  - [x] Constructors
  - [x] Inherits
  - [x] Generics
  - [ ] Generics Constaints
  - [x] Properties
  - [x] Methods
  - [x] Events
  - [x] Indexer
- [ ] Types
  - [x] String literals
  - [x] Type literals
  - [x] Type alias*
  - [x] Union types
  - [ ] Intersection types
- [x] Split entities to different files
- [x] Auto rename conflict parameter names
- [x] WinRT/CLR async types support (`IAsyncAction`/`IAsyncOperation<T>` or `Task`/`Task<T>`)
- [x] Newtonsoft.Json and System.Text.Json for JSON serialization
- [x] Nullable Reference Types
- [ ] Type validation

`*: Partial support`

## Quick start
TypeScript code `input.ts`:
```typescript
declare namespace test {
  /**
   * The declaration of an enum
   */
  export enum MyEnum {
    A = 0,
    B = 1,
    C = 2,
    D = C
  }

  /**
   * The declaration of an interface
   */
  export interface MyInterface1 {
    /**
     * A method
     */
    testMethod(arg: string, callback: () => void): string;
    /**
     * An event
     * @event
     */
    onTest(listener: (e: MyInterface1) => void): void;
    /**
     * An property
     */
    readonly testProp: string;
  }

  /**
   * Another declaration of an interface
   */
  export interface MyInterface2<T> {
    /**
     * A method
     */
    testMethod(arg: T, callback: () => void): T;
    /**
     * An event
     * @event
     */
    onTest(listener: (e: MyInterface2<T>) => void): void;
    /**
     * An property
     */
    readonly testProp: T;
  }

  /**
   * The declaration of a class
   */
  export class MyClass1<T> implements MyInterface1 {
    /**
     * A method
     */
    testMethod(arg: string, callback: () => void): string;
    /**
     * An event
     * @event
     */
    onTest(listener: (e: MyInterface1) => void): void;
    /**
     * An property
     */
    readonly testProp: string;
    static staticMethod(value: string, isOption?: boolean): UnionStr;
  }

  /**
   * Another declaration of a class
   */
  export class MyClass2<T> implements MyInterface2<T> {
    /**
     * A method
     */
    testMethod(arg: T, callback: () => void): T;
    /**
     * An event
     * @event
     */
    onTest(listener: (e: MyInterface2<T>) => void): void;
    /**
     * An property
     */
    readonly testProp: T;
    static staticMethod(value: string, isOption?: boolean): UnionStr;
  }

  /**
   * The declaration of a type alias
   */
  export type UnionStr = "A" | "B" | "C" | "other";
}
```

With below commands (need a `tsconfig.json`): 
```bash
typedoc input.ts --json output.json
TypedocConverter --inputfile output.json --outputfile output.cs
```

Voila! C# type bindings generated in output.cs:
```csharp
namespace Test
{

    /// <summary>
    /// The declaration of an enum
    /// </summary>
    enum MyEnum
    {
        A = 0,
        B = 1,
        C = 2,
        D = 2
    }
}

namespace Test
{

    /// <summary>
    /// The declaration of a class
    /// </summary>
    class MyClass1<T> : MyInterface1
    {
        /// <summary>
        /// An event
        /// </summary>
        public event System.Action<MyInterface1> OnTest;

        /// <summary>
        /// An property
        /// </summary>
        [Newtonsoft.Json.JsonProperty("testProp", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        public string TestProp { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

        /// <summary>
        /// A method
        /// </summary>
        public string TestMethod(string arg, System.Action callback) => throw new System.NotImplementedException();

        static UnionStr StaticMethod(string value, bool isOption) => throw new System.NotImplementedException();

    }
}

namespace Test
{

    /// <summary>
    /// Another declaration of a class
    /// </summary>
    class MyClass2<T> : MyInterface2<T>
    {
        /// <summary>
        /// An event
        /// </summary>
        public event System.Action<MyInterface2<T>> OnTest;

        /// <summary>
        /// An property
        /// </summary>
        [Newtonsoft.Json.JsonProperty("testProp", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        public T TestProp { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

        /// <summary>
        /// A method
        /// </summary>
        public T TestMethod(T arg, System.Action callback) => throw new System.NotImplementedException();

        static UnionStr StaticMethod(string value, bool isOption) => throw new System.NotImplementedException();

    }
}

namespace Test
{

    /// <summary>
    /// The declaration of an interface
    /// </summary>
    interface MyInterface1
    {
        /// <summary>
        /// An event
        /// </summary>
        event System.Action<MyInterface1> OnTest;

        /// <summary>
        /// An property
        /// </summary>
        [Newtonsoft.Json.JsonProperty("testProp", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        string TestProp { get; set; }

        /// <summary>
        /// A method
        /// </summary>
        string TestMethod(string arg, System.Action callback);

    }
}

namespace Test
{

    /// <summary>
    /// Another declaration of an interface
    /// </summary>
    interface MyInterface2<T>
    {
        /// <summary>
        /// An event
        /// </summary>
        event System.Action<MyInterface2<T>> OnTest;

        /// <summary>
        /// An property
        /// </summary>
        [Newtonsoft.Json.JsonProperty("testProp", NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        T TestProp { get; set; }

        /// <summary>
        /// A method
        /// </summary>
        T TestMethod(T arg, System.Action callback);

    }
}

namespace Test
{

    /// <summary>
    /// The declaration of a type alias
    /// </summary>
    [Newtonsoft.Json.JsonConverter(typeof(UnionStrConverter))]
    enum UnionStr
    {
        ///<summary>
        /// A
        ///</summary>
        A,
        ///<summary>
        /// B
        ///</summary>
        B,
        ///<summary>
        /// C
        ///</summary>
        C,
        ///<summary>
        /// other
        ///</summary>
        Other
    }

    class UnionStrConverter : Newtonsoft.Json.JsonConverter
    {
        public override bool CanConvert(System.Type t) => t == typeof(UnionStr) || t == typeof(UnionStr?);

        public override object ReadJson(Newtonsoft.Json.JsonReader reader, System.Type t, object? existingValue, Newtonsoft.Json.JsonSerializer serializer)
            => reader.TokenType switch
            {
                Newtonsoft.Json.JsonToken.String =>
                    serializer.Deserialize<string>(reader) switch
                    {
                        "A" => UnionStr.A,
                        "B" => UnionStr.B,
                        "C" => UnionStr.C,
                        "other" => UnionStr.Other,
                        _ => throw new System.NotSupportedException("Cannot unmarshal type UnionStr")
                    },
                _ => throw new System.NotSupportedException("Cannot unmarshal type UnionStr")
            };

        public override void WriteJson(Newtonsoft.Json.JsonWriter writer, object? untypedValue, Newtonsoft.Json.JsonSerializer serializer)
        {
            if (untypedValue is null) { serializer.Serialize(writer, null); return; }
            var value = (UnionStr)untypedValue;
            switch (value)
            {
                case UnionStr.A: serializer.Serialize(writer, "A"); return;
                case UnionStr.B: serializer.Serialize(writer, "B"); return;
                case UnionStr.C: serializer.Serialize(writer, "C"); return;
                case UnionStr.Other: serializer.Serialize(writer, "other"); return;
                default: break;
            }
            throw new System.NotSupportedException("Cannot marshal type UnionStr");
        }
    }
}
```


## Build
```bash
cd TypedocConverter/TypedocConverter
dotnet publish -c Release -r win-x64 --no-self-contained /p:PublishSingleFile=true /p:PublishReadyToRun=true
```
You can replace `win-x64` with other platform identifiers such as `win-arm64`, `linux-x64`, `linux-arm64`, `osx-x64` and etc.  
Then built dists will be placed in `bin/Release/net6.0/[platform identifier]/publish`

## Native Build
```bash
cd TypedocConverter/TypedocConverter
dotnet publish -c Release -r win-x64 /p:NativeBuild=true
```
You can replace `win-x64` with other platform identifiers such as `win-arm64`, `linux-x64`, `linux-arm64`, `osx-x64` and etc.  
Then built dists will be placed in `bin/Release/net6.0/[platform identifier]/publish`

## Run & Usage
```bash
TypedocConverter --help
```
Sample:
```bash
TypedocConverter --inputfile 1.json --splitfiles true --outputdir .
```
Arguments:
```
--inputfile [file]: input file
--namespace [namespace]: specify namespace for generated code
--splitfiles [true|false]: whether to split code to different files
--outputdir [path]: used for place code files when splitfiles is true
--outputfile [path]: used for place code file when splitfiles is false
--number-type [int/decimal/double...]: config for number type mapping
--promise-type [CLR/WinRT]: config for promise type mapping, CLR for Task and WinRT for IAsyncAction/IAsyncOperation
--any-type [object/dynamic...]: config for any type mapping
--array-type [Array/IEnumerable/List...]: config for array type mapping
--nrt-disabled [true|false]: whether to disable Nullable Reference Types
--json-mode [system|newtonsoft|both]: whether to use System.Text.Json or Newtonsoft.Json or both
```

## Prebuilt binaries
We have prepared some prebuilt binaries for Windows, Linux and macOS.  
You can download them directly from [Releases](https://github.com/hez2010/TypedocConverter/releases)

Prerequisites for non-native binaries: [.NET Runtime 6.0](https://dotnet.microsoft.com/download/dotnet-core/6.0)

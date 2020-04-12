# Typedoc Converter
This is a [typedoc](https://github.com/TypeStrong/typedoc) json to C# bindings converter.  

Build status: ![.NET Core](https://github.com/hez2010/TypedocConverter/workflows/.NET%20Core/badge.svg)

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
- [ ] Types
  - [x] String literals
  - [ ] Type alias
  - [ ] Union types
  - [ ] Intersection types
  - [ ] Anonymous interfaces
- [x] Split entities to different files
- [x] Auto rename conflict parameter names
- [ ] ...

## Build
```
cd TypedocConverter/TypedocConverter
dotnet publish -c Release -r win-x64 --no-self-contained /p:PublishSingleFile=true /p:PublishReadyToRun=true
```
You can replace `win-x64` with other platform identifiers such as `linux-x64`, `linux-arm`, `osx-x64` and etc.  
Then built dists will be placed in `bin/Release/netcoreapp3.1/[platform identifier]/publish`

## Native Build:
```
cd TypedocConverter/TypedocConverter
dotnet publish -c Release -r win-x64 /p:NativeBuild=true
```

## Run
```
TypedocConverter --help
```
Sample:
```
TypedocConverter --inputfile 1.json --splitfiles true --outputdir . --namespace ""
```
## Prebuilt binaries
We have prepared some prebuilt binaries for Windows, Linux and macOS.  
You can download them directly from [Releases](https://github.com/hez2010/TypedocConverter/releases)

Prerequisites: [.NET Core Runtime 3.1.x](https://dotnet.microsoft.com/download/dotnet-core/3.1)

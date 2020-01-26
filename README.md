# Typedoc Converter
This is a [typedoc](https://github.com/TypeStrong/typedoc) json to language type bindings converter.  
Currently it support TypeScript doc to C# binding.

## Languages support
- [x] C#
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
    - [x] Inherits
    - [x] Generics
    - [ ] Generics Constaints
    - [x] Properties
    - [x] Methods
    - [x] Events
  - [x] Split entities to different files
  - [x] Auto rename conflict parameter names
  - [ ] ...
- [ ] ...

## Build
```
cd TypedocConverter/TypedocConverter
dotnet publish -c Release -r win-x64 --no-self-contained /p:PublishSingleFile=true /p:PublishReadyToRun=true
```
You can replace `win-x64` with other platform identifiers such as `linux-x64`, `linux-arm`, `osx-x64` and etc.  
Then built dists will be placed in `bin/Release/netcoreapp3.1/[platform identifier]/publish`
## Run
```
TypedocConverter --help
```
Sample:
```
TypedocConverter --inputfile 1.json --splitfiles true --outputdir . --namespace ""
```
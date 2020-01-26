# Typedoc Generator
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
  - [ ] Split entities to different files
  - [x] Auto rename conflict parameter names
  - [ ] ...
- [ ] ...

## Build
```
dotnet publish -c Release -r win-x64 --no-self-contained /p:PublishSingleFile=true /p:PublishReadyToRun=true
```
You can replace `win-x64` with other platform identifiers such as `linux-x64`, `linux-arm`, `osx-x64` and etc.

## Run
```
cd bin\Release\netcoreapp3.1\[platform identifier]\publish
.\TypeDocGenerator --help
```
Sample:
```
cd bin\Release\netcoreapp3.1\win-x64\publish
.\TypeDocGenerator --help
```
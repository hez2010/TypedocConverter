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
dotnet build [-c Release|Debug]
```

## Run
```
dotnet run [-c Release|Debug] inputfile [> outputfile]
```
Sample:
```
dotnet run -c Release input.json > 1.cs
```

## Test
```
dotnet test
```
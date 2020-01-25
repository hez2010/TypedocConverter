# TypeDocGenerator
This is a [typedoc](https://github.com/TypeStrong/typedoc) json to language type bindings converter.

## Languages support
- [ ] C#
  - [x] Enums
    - [x] Direct value
    - [x] Referenced value
  - [ ] Interfaces
    - [ ] Inherits
    - [ ] Generics
    - [ ] Properties
    - [ ] Methods
  - [ ] Classes
    - [ ] Inherits
    - [ ] Generics
    - [ ] Properties
    - [ ] Methods
    - [ ] Fields
  - [ ] Split entities to different files
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
module Entity

type EntityBodyType = {
    Type: string
    Name: string option
    InnerTypes: EntityBodyType list
}

type EntityMethod = {
    Comment: string
    Modifier: string list
    Type: EntityBodyType
    Name: string
    TypeParameter: string list
    Parameter: EntityBodyType list
}

type EntityProperty = {
    Comment: string
    Modifier: string list
    Name: string
    Type: EntityBodyType
    WithGet: bool
    WithSet: bool
    IsOptional: bool
    InitialValue: string option
}

type EntityEvent = {
    Comment: string
    Modifier: string list
    DelegateType: EntityBodyType
    Name: string
    IsOptional: bool
}

type EntityEnum = {
    Comment: string
    Name: string
    Value: int64 option
}

type EntityType = 
| Interface
| Class
| Enum
| StringEnum

type Entity = {
    Namespace: string
    Name: string
    Comment: string
    Methods: EntityMethod list
    Properties: EntityProperty list
    Events: EntityEvent list
    Enums: EntityEnum list
    InheritedFrom: EntityBodyType list
    Type: EntityType
    TypeParameter: string list
    Modifier: string list
}
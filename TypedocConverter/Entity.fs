module Entity

type Entity =
| TypeEntity of
    Name: string *
    InnerTypes: Entity list
| TypeParameterEntity of
    Name: string // TODO: Add contraints support
| ParameterEntity of
    Name: string *
    Type: Entity
| PropertyEntity of
    Name: string *
    Comment: string *
    Modifier: string list *
    Type: Entity *
    WithGet: bool *
    WithSet: bool *
    IsOptional: bool *
    InitialValue: string option
| EventEntity of
    Name: string *
    Comment: string *
    Modifier: string list *
    IsOptional: bool *
    Type: Entity
| EnumMemberEntity of
    Name: string *
    Comment: string *
    Value: string option
| MethodEntity of
    Name: string *
    Comment: string *
    Modifier: string list *
    TypeParameter: Entity list *
    Parameter: Entity list *
    ReturnType: Entity
| EnumEntity of 
    Namespace: string *
    Name: string *
    Comment: string *
    Modifier: string list *
    Members: Entity list
| ClassInterfaceEntity of
    Namespace: string *
    Name: string *
    Comment: string *
    Modifier: string list *
    Methods: Entity list *
    Properties: Entity list *
    Events: Entity list *
    InheritedFrom: Entity list *
    TypeParameter: Entity list *
    IsInterface: bool
| StringUnionEntity of
    Namespace: string *
    Name: string *
    Comment: string *
    Modifier: string list *
    Members: Entity list
| TypealiasEntity of // not used
    AliasedType: Entity
| UnionTypeEntity of // not used
    Types: Entity list
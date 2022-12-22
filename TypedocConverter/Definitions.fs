module Definitions

type JsonMode = System = 1 | Newtonsoft = 2 | Both = 3

type Config = {
    Namespace: string
    SplitFiles: bool
    OutputDir: string
    OutputFile: string
    InputFile: string
    Help: bool
    NumberType: string
    UseWinRTPromise: bool
    AnyType: string
    ArrayType: string
    NrtDisabled: bool
    JsonMode: JsonMode
}

type ReflectionKind = 
| Global = 0
| ExternalModule = 1
| Module = 2
| Enum = 4
| EnumMember = 16
| Variable = 32
| Function = 64
| Class = 128
| Interface = 256
| Constructor = 512
| Property = 1024
| Method = 2048
| CallSignature = 4096
| IndexSignature = 8192
| ConstructorSignature = 16384
| Parameter = 32768
| TypeLiteral = 65536
| TypeParameter = 131072
| Accessor = 262144
| GetSignature = 524288
| SetSignature = 1048576
| ObjectLiteral = 2097152
| TypeAlias = 4194304
| Event = 8388608
| Reference = 16777216

type ReflectionFlags = {
    IsPrivate: bool option
    IsProtected: bool option
    IsPublic: bool option
    IsStatic: bool option
    IsExported: bool option
    IsExternal: bool option
    IsOptional: bool option
    IsReset: bool option
    HasExportAssignment: bool option
    IsConstructorProperty: bool option
    IsAbstract: bool option
    IsConst: bool option
    IsLet: bool option
}

type Reflection = {
    Id: int
    Name: string
    OriginalName: string
    Kind: ReflectionKind
    KindString: string option
    Flags: ReflectionFlags
    Parent: Reflection option
    Comment: Comment option
    Sources: SourceReference list option
    Decorators: Decorator option
    Decorates: Type list option
    Url: string option
    Anchor: string option
    HasOwnDocument: bool option
    CssClasses: string option
    DefaultValue: string option
    Type: Type option
    TypeParameter: Reflection list option
    Signatures: Reflection list option
    IndexSignature: Reflection option
    GetSignature: Reflection list option
    SetSignature: Reflection list option
    Overwrites: Type option
    InheritedFrom: Type option
    ImplementationOf: Type option
    ExtendedTypes: Type list option
    ExtendedBy: Type list option
    ImplementedTypes: Type list option
    ImplementedBy: Type list option
    TypeHierarchy: DeclarationHierarchy option
    Children: Reflection list option
    Groups: ReflectionGroup list option
    Categories: ReflectionCategory list option
    Reflections: Map<int, Reflection> option
    Directory: SourceDirectory option
    Files: SourceFile list option
    Readme: string option
    PackageInfo: obj option
    Parameters: Reflection list option
}
and DeclarationHierarchy = {
    Type: Type list
    Next: DeclarationHierarchy option
    IsTarget: bool option
}
and Type = {
    Type: string
    Id: int
    Name: string option
    ElementType: Type option
    Value: obj option
    Types: Type list option
    TypeArguments: Type list option
    Constraint: Type option
    Declaration: Reflection option
}
and Decorator = {
    Name: string
    Type: Type option
    Arguments: obj option
}
and ReflectionGroup = {
    Title: string
    Kind: ReflectionKind
    Children: int list
    CssClasses: string option
    AllChildrenHaveOwnDocument: bool option
    AllChildrenAreInherited: bool option
    AllChildrenArePrivate: bool option
    AllChildrenAreProtectedOrPrivate: bool option
    AllChildrenAreExternal: bool option
    SomeChildrenAreExported: bool option
    Categories: ReflectionCategory list option
}
and ReflectionCategory = {
    Title: string
    Children: int list
    AllChildrenHaveOwnDocument: bool option
}
and SourceDirectory = {
    Parent: SourceDirectory option
    Directories: Map<string, SourceDirectory>
    Groups: ReflectionGroup list option
    Files: SourceFile list
    Name: string option
    DirName: string option
    Url: string option
}
and SourceFile = {
    FullFileName: string
    FileName: string
    Name: string
    Url: string option
    Parent: SourceDirectory option
    Reflections: Reflection list option
    Groups: ReflectionGroup list option
}
and SourceReference = {
    File: SourceFile option
    FileName: string
    Line: int
    Character: int
    Url: string option
}
and Comment = {
    ShortText: string
    Text: string option
    Returns: string option
    Tags: CommentTag list option
}
and CommentTag = {
    TagName: string
    ParentName: string
    Text: string
}
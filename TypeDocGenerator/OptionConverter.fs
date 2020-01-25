module Converters

open Newtonsoft.Json
open System
open Microsoft.FSharp.Reflection

type OptionConverter() =
    inherit JsonConverter()
    override __.CanConvert(objectType: Type) : bool = 
        match objectType.IsGenericType with
        | false -> false
        | true -> typedefof<_ option> = objectType.GetGenericTypeDefinition()
    override __.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) : unit = 
        serializer.Serialize(writer, 
            if isNull value then null
            else let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
                 fields.[0]
        )
    override __.ReadJson(reader: JsonReader, objectType: Type, _existingValue: obj, serializer: JsonSerializer) : obj = 
        let innerType = objectType.GetGenericArguments().[0]
        let value = serializer.Deserialize(reader, if innerType.IsValueType 
                                                   then (typedefof<_ Nullable>).MakeGenericType([|innerType|])
                                                   else innerType
        )
        let cases = FSharpType.GetUnionCases objectType
        if isNull value then FSharpValue.MakeUnion(cases.[0], [||])
        else FSharpValue.MakeUnion(cases.[1], [|value|])

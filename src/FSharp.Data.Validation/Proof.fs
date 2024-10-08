namespace FSharp.Data.Validation

open System
open System.Text.Json
open System.Text.Json.Serialization

[<JsonConverter(typeof<ValidationFailuresConverterFactory>)>]
type ValidationFailures<'F> =
    { Failures: 'F list
      Fields: FailureMap<'F> }
and ValidationFailuresConverter<'F>() =
    inherit JsonConverter<ValidationFailures<'F>>()
        member private this.mkName(ns:Name list) =
            let rec mk (ns:Name list) (acc:string) =
                match ns with
                | [] -> acc
                | n::ns' -> mk ns' (sprintf "%s.%s" acc (this.toCamelCase n.Value))
            match ns with
            | []        -> String.Empty
            | n::ns'    -> mk ns' (this.toCamelCase n.Value)
        member private this.toCamelCase (str:string) =
            match str.Length with
            | 0 -> str
            | 1 -> str.ToLower()
            | _ -> sprintf "%c%s" (Char.ToLowerInvariant(str[0])) (str.Substring(1))
        override this.Read(reader: byref<Utf8JsonReader>, typ, opts) =
            JsonSerializer.Deserialize<ValidationFailures<'F>>(&reader, opts)
        override this.Write(writer, fs, opts) =
            writer.WriteStartObject()

            writer.WriteStartArray("failures")
            for f in fs.Failures do
                writer.WriteStringValue(f.ToString())
            writer.WriteEndArray()

            writer.WriteStartObject("fields")
            for (ns,fs) in Map.toSeq(fs.Fields) do
                writer.WriteStartArray(this.mkName ns)
                for f in fs do
                    writer.WriteStringValue(f.ToString())
                writer.WriteEndArray()
            writer.WriteEndObject()

            writer.WriteEndObject()
            writer.Flush()
and ValidationFailuresConverterFactory() =
    inherit JsonConverterFactory()
        override this.CanConvert(typ) =
            typ.GetGenericTypeDefinition() = typedefof<ValidationFailures<_>>
        override this.CreateConverter(typ, opts) =
            let tArgs = typ.GetGenericArguments()
            let t = typedefof<ValidationFailuresConverter<_>>.MakeGenericType(tArgs)
            Activator.CreateInstance(t) :?> JsonConverter

[<JsonConverter(typeof<ProofConverterFactory>)>]
type Proof<'F, 'A> =
    | Valid of 'A
    | Invalid of 'F list * FailureMap<'F>
and ProofConverter<'F, 'A>() =
    inherit JsonConverter<Proof<'F, 'A>>()
        member private this.mkName(ns:Name list) =
            let rec mk (ns:Name list) (acc:string) =
                match ns with
                | [] -> acc
                | n::ns' -> mk ns' (sprintf "%s.%s" acc (this.toCamelCase n.Value))
            match ns with
            | []        -> String.Empty
            | n::ns'    -> mk ns' (this.toCamelCase n.Value)
        member private this.toCamelCase (str:string) =
            match str.Length with
            | 0 -> str
            | 1 -> str.ToLower()
            | _ -> sprintf "%c%s" (Char.ToLowerInvariant(str[0])) (str.Substring(1))
        override this.Read(reader: byref<Utf8JsonReader>, typ, opts) =
            JsonSerializer.Deserialize<Proof<'F, 'A>>(&reader, opts)
        override this.Write(writer, proof, opts) =
            match proof with
            | Valid a           -> JsonSerializer.Serialize(writer, a, opts)
            | Invalid (gfs,lfs) -> JsonSerializer.Serialize(writer, { Failures = gfs; Fields = lfs }, opts)
and ProofConverterFactory() =
    inherit JsonConverterFactory()
        override this.CanConvert(typ) =
            typ.GetGenericTypeDefinition() = typedefof<Proof<_,_>>
        override this.CreateConverter(typ, opts) =
            let tArgs = typ.GetGenericArguments()
            let t = typedefof<ProofConverter<_,_>>.MakeGenericType(tArgs)
            Activator.CreateInstance(t) :?> JsonConverter

module Proof =
    /// Applies function to the proof value
    let map fn p =
        match p with
        | Invalid (gfs, lfs)    -> Invalid (gfs, lfs)
        | Valid a               -> Valid (fn a)

    /// Applies function to failure type
    let mapInvalid fn p =
        match p with
        | Invalid (gfs, lfs) -> Invalid (List.map fn gfs, Map.map (fun _ s -> List.map fn s) lfs)
        | Valid a -> Valid a

    /// Combines two proofs using the provided function
    /// If given `Invalid` proofs, aggregates failures
    let combine fn p1 p2 =
        match p1 with
        | Valid a1 ->
            match p2 with
            | Valid a2              -> Valid (fn a1 a2)
            | Invalid (gfs', lfs')  -> Invalid (gfs', lfs')
        | Invalid (gfs, lfs) ->
            match p2 with
            | Valid _               -> Invalid (gfs, lfs)
            | Invalid (gfs', lfs')  -> Invalid (gfs @ gfs', Utilities.mergeFailures lfs lfs')

    let toValidationFailures p =
        match p with
        | Valid a           -> None
        | Invalid (gfs,lfs) -> Some { Failures = gfs; Fields= lfs; }

    let toResult p =
        match p with
        | Valid a           -> Ok a
        | Invalid (gfs,lfs) -> Error { Failures = gfs; Fields= lfs; }

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

    member private this.mkName(ns: Name list) =
        let rec mk (ns: Name list) (acc: string) =
            match ns with
            | [] -> acc
            | n :: ns' -> mk ns' (sprintf "%s.%s" acc (this.toCamelCase n.Value))

        match ns with
        | [] -> String.Empty
        | n :: ns' -> mk ns' (this.toCamelCase n.Value)

    member private this.toCamelCase(str: string) =
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

        for (ns, fs) in Map.toSeq (fs.Fields) do
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
    /// Represents a successful validation with the valid value.
    | Valid of 'A
    /// Represents a failed validation with global and field-specific failures.
    | Invalid of 'F list * FailureMap<'F>

and ProofConverter<'F, 'A>() =
    inherit JsonConverter<Proof<'F, 'A>>()

    member private this.mkName(ns: Name list) =
        let rec mk (ns: Name list) (acc: string) =
            match ns with
            | [] -> acc
            | n :: ns' -> mk ns' (sprintf "%s.%s" acc (this.toCamelCase n.Value))

        match ns with
        | [] -> String.Empty
        | n :: ns' -> mk ns' (this.toCamelCase n.Value)

    member private this.toCamelCase(str: string) =
        match str.Length with
        | 0 -> str
        | 1 -> str.ToLower()
        | _ -> sprintf "%c%s" (Char.ToLowerInvariant(str[0])) (str.Substring(1))

    override this.Read(reader: byref<Utf8JsonReader>, typ, opts) =
        JsonSerializer.Deserialize<Proof<'F, 'A>>(&reader, opts)

    override this.Write(writer, proof, opts) =
        match proof with
        | Valid a -> JsonSerializer.Serialize(writer, a, opts)
        | Invalid(gfs, lfs) -> JsonSerializer.Serialize(writer, { Failures = gfs; Fields = lfs }, opts)

and ProofConverterFactory() =
    inherit JsonConverterFactory()

    override this.CanConvert(typ) =
        typ.GetGenericTypeDefinition() = typedefof<Proof<_, _>>

    override this.CreateConverter(typ, opts) =
        let tArgs = typ.GetGenericArguments()
        let t = typedefof<ProofConverter<_, _>>.MakeGenericType(tArgs)
        Activator.CreateInstance(t) :?> JsonConverter

module Proof =
    /// Applies function to the proof value
    let map fn p =
        match p with
        | Invalid(gfs, lfs) -> Invalid(gfs, lfs)
        | Valid a -> Valid(fn a)

    /// Applies function to failure type
    let mapInvalid fn p =
        match p with
        | Invalid(gfs, lfs) -> Invalid(List.map fn gfs, Map.map (fun _ s -> List.map fn s) lfs)
        | Valid a -> Valid a

    /// Combines two proofs using the provided function
    /// If given `Invalid` proofs, aggregates failures
    let combine fn p1 p2 =
        match p1 with
        | Valid a1 ->
            match p2 with
            | Valid a2 -> Valid(fn a1 a2)
            | Invalid(gfs', lfs') -> Invalid(gfs', lfs')
        | Invalid(gfs, lfs) ->
            match p2 with
            | Valid _ -> Invalid(gfs, lfs)
            | Invalid(gfs', lfs') -> Invalid(gfs @ gfs', Utilities.mergeFailures lfs lfs')

    /// Sequences a list of proofs into a proof of a list.
    /// If all proofs are Valid, returns Valid with list of values.
    /// If any are Invalid, aggregates all failures.
    let sequence (proofs: Proof<'F, 'A> list) : Proof<'F, 'A list> =
        let folder state proof =
            combine (fun xs x -> xs @ [ x ]) state proof

        List.fold folder (Valid []) proofs

    /// Applies a function returning a proof to each element in a list and sequences the results.
    /// If all applications are Valid, returns Valid with list of transformed values.
    /// If any are Invalid, aggregates all failures.
    let traverse (fn: 'A -> Proof<'F, 'B>) (items: 'A list) : Proof<'F, 'B list> = items |> List.map fn |> sequence

    /// Monadic bind for Proof. Chains proof-returning computations.
    /// If the first proof is Invalid, returns it unchanged.
    /// If the first proof is Valid, applies the function to the value.
    let bind (fn: 'A -> Proof<'F, 'B>) (proof: Proof<'F, 'A>) : Proof<'F, 'B> =
        match proof with
        | Valid a -> fn a
        | Invalid(gfs, lfs) -> Invalid(gfs, lfs)

    /// Applicative apply for Proof. Applies a proof of a function to a proof of a value.
    /// If both are Valid, returns Valid with the function applied to the value.
    /// If either is Invalid, aggregates failures.
    let apply (fnProof: Proof<'F, ('A -> 'B)>) (valueProof: Proof<'F, 'A>) : Proof<'F, 'B> =
        match fnProof with
        | Valid fn ->
            match valueProof with
            | Valid a -> Valid(fn a)
            | Invalid(gfs, lfs) -> Invalid(gfs, lfs)
        | Invalid(gfs, lfs) ->
            match valueProof with
            | Valid _ -> Invalid(gfs, lfs)
            | Invalid(gfs', lfs') -> Invalid(gfs @ gfs', Utilities.mergeFailures lfs lfs')

    /// Chooses the first Valid proof from two proofs.
    /// If the first is Valid, returns it.
    /// If the first is Invalid and second is Valid, returns the second.
    /// If both are Invalid, returns the first.
    let choose (first: Proof<'F, 'A>) (second: Proof<'F, 'A>) : Proof<'F, 'A> =
        match first with
        | Valid _ -> first
        | Invalid _ ->
            match second with
            | Valid _ -> second
            | Invalid _ -> first

    /// Extracts the validation failures from a Proof, if any.
    let toValidationFailures p =
        match p with
        | Valid a -> None
        | Invalid(gfs, lfs) -> Some { Failures = gfs; Fields = lfs }

    /// Converts a Proof to a Result, where Valid becomes Ok and Invalid becomes Error with ValidationFailures.
    let toResult p =
        match p with
        | Valid a -> Ok a
        | Invalid(gfs, lfs) -> Error { Failures = gfs; Fields = lfs }

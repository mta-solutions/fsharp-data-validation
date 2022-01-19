namespace Data.Validation

open System
open System.Text.Json
open System.Text.Json.Serialization

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
        override this.Read(reader: byref<Utf8JsonReader>, typ, opts) = base.Read(&reader, typ, opts)
        override this.Write(writer, proof, opts) = 
            match proof with
            | Valid a               -> JsonSerializer.Serialize(writer, a, opts)
            | Invalid (gfs, lfs)    ->
                writer.WriteStartObject()

                writer.WriteStartArray("failures")
                for f in gfs do
                    writer.WriteStringValue(f.ToString())
                writer.WriteEndArray()

                writer.WriteStartObject("fields")
                for (ns,fs) in Map.toSeq(lfs) do
                    writer.WriteStartArray(this.mkName ns)
                    for f in fs do
                        writer.WriteStringValue(f.ToString())
                    writer.WriteEndArray()
                writer.WriteEndObject()

                writer.WriteEndObject()
                writer.Flush()
and ProofConverterFactory() =
    inherit JsonConverterFactory()
        override this.CanConvert(typ) = 
            typ.GetGenericTypeDefinition() = typedefof<Proof<_,_>>
        override this.CreateConverter(typ, opts) = 
            let tArgs = typ.GetGenericArguments()
            let t = typedefof<ProofConverter<_,_>>.MakeGenericType(tArgs)
            Activator.CreateInstance(t) :?> JsonConverter

module Proof =
    // Applies function to the proof value
    let map fn p = 
        match p with
        | Invalid (gfs, lfs)    -> Invalid (gfs, lfs)
        | Valid a               -> Valid (fn a)

    // Applies function to failure type
    let mapInvalid fn p =
        match p with
        | Invalid (gfs, lfs) -> Invalid (List.map fn gfs, Map.map (fun _ s -> List.map fn s) lfs)
        | Valid a -> Valid a
    
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

[<AutoOpen>]
module FSharp.Data.Validation.Core

/// Converts a `VCtx` to a `Proof`, where `ValidCtx` becomes `Valid` and both `DisputedCtx` and `RefutedCtx` become
/// `Invalid` with their respective failures.
let fromVCtx<'F, 'A> (ctx: FSharp.Data.Validation.VCtx<'F, 'A>) : FSharp.Data.Validation.Proof<'F, 'A> =
    match ctx with
    | FSharp.Data.Validation.ValidCtx a -> FSharp.Data.Validation.Valid a
    | FSharp.Data.Validation.DisputedCtx(gfs, lfs, _) -> FSharp.Data.Validation.Invalid(gfs, lfs)
    | FSharp.Data.Validation.RefutedCtx(gfs, lfs) -> FSharp.Data.Validation.Invalid(gfs, lfs)

/// Checks that a `Result` value is a `Error`.
/// If not, it adds the given failure to the result and validation end.
let isError e = Result.isError e

/// Checks that a `Result` value is a `Ok`.
/// If not, it adds the given failure to the result and validation end.
let isOk e = Result.isOk e

/// tests if a 'Proof' is valid.
let isValid p =
    match p with
    | FSharp.Data.Validation.Valid _ -> true
    | FSharp.Data.Validation.Invalid _ -> false

/// tests if a 'Proof' is invalid.
let isInvalid p = isValid p |> not

/// Flatten a list of proofs into a proof of the list.
let flattenProofs ps =
    let ps' = ps |> List.map (FSharp.Data.Validation.Proof.map (fun a -> [ a ]))

    (FSharp.Data.Validation.Valid [], ps')
    ||> List.fold (FSharp.Data.Validation.Proof.combine (@))

/// Raises an `InvalidProofException` if the the given proof is `Invalid`.
let raiseIfInvalid msg p =
    match p with
    | FSharp.Data.Validation.Invalid(gfs, lfs) ->
        raise (FSharp.Data.Validation.Types.InvalidProofException<_>(msg, gfs, lfs))
    | FSharp.Data.Validation.Valid a -> a

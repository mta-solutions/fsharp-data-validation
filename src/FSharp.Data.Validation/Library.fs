[<AutoOpen>]
module FSharp.Data.Validation.Default

open System.Collections.Generic
open System.Linq
open System.Text.RegularExpressions

let fromVCtx<'F, 'A> (ctx:VCtx<'F, 'A>): Proof<'F, 'A> =
    match ctx with
    | ValidCtx a                -> Valid a
    | DisputedCtx (gfs, lfs, _) -> Invalid (gfs, lfs)
    | RefutedCtx (gfs, lfs)     -> Invalid (gfs, lfs)

/// Checks that an `Option` value is a `Some`.
/// If not, it adds the given failure to the result and validation end.
let isRequired (f:'F) (ma:'A option): Result<'A, 'F> =
    match ma with
    | None -> Error f
    | Some a -> Ok a

/// Checks that a `Option` value is a `Some` when some condition is true.
/// If the condition is met and the value is `None`,
/// it adds the given failure to the result and validation continues.
let isRequiredWhen f b (ma:'A option): 'F option =
    match b with
    | false -> None
    | true ->
        match ma with
        | None   -> Some f
        | Some _ -> None

/// Checks that a `Option` value is a `Some` when some condition is false.
/// If the condition is not met and the value is `Some`,
/// it adds the given failure to the result and validation continues.
let isRequiredUnless f b v = isRequiredWhen f (not b) v

/// Checks that a `Result` value is a `Error`.
/// If not, it adds the given failure to the result and validation end.
let isError e =
    match e with
    | Error _   -> true
    | Ok _      -> false

/// Checks that a `Result` value is a `Ok`.
/// If not, it adds the given failure to the result and validation end.
let isOk e =
    match e with
    | Error _   -> false
    | Ok _      -> true

/// Checks that the `IEnumerable` is empty.
/// If not, it adds the given failure to the result and validation continues.
let isNull (a:#seq<_>) = not (a.Any())

/// Checks that the `IEnumerable` is not empty.
/// If empty, it adds the given failure to the result and validation continues.
let isNotNull (a:#seq<_>) = a.Any()

/// Checks that a `IEnumerable` has a length equal to or greater than the given value.
/// If not, it adds the given failure to the result and validation continues.
let minLength l (a:#seq<_>) = a.Count() >= l

/// Checks that a `IEnumerable` has a length equal to or less than the given value.
/// If not, it adds the given failure to the result and validation continues.
let maxLength l (a:#seq<_>) = a.Count() <= l

/// Checks that a `IEnumerable` has a length equal to the given value.
/// If not, it adds the given failure to the result and validation continues.
let isLength l (a:#seq<_>) = a.Count() = l

/// Checks that a value is equal to another.
/// If not, it adds the given failure to the result and validation continues.
let isEqual = (=)

/// Checks that a value is not equal to another.
/// If equal, it adds the given failure to the result and validation continues.
let isNotEqual a b = a = b |> not

/// Checks that b is less than a, as b is our validation input.
/// If not, it adds the given failure to the result and validation continues.
let isLessThan = (>)

/// Checks that b is greater than a, as b is our validation input.
/// If not, it adds the given failure to the result and validation continues.
let isGreaterThan = (<)

/// Checks that b is less than or equal to a, as b is our validation input.
/// If not, it adds the given failure to the result and validation continues.
let isLessThanOrEqual = (>=)

/// Checks that b is greater than or equal to a, as b is our validation input.
/// If not, it adds the given failure to the result and validation continues.
let isGreaterThanOrEqual = (<=)

/// Checks that a `IEnumerable` has a given element.
/// If not, it adds the given failure to the result and validation continues.
let hasElem e (a:#seq<_>) = a.Contains(e)

/// Checks that a `IEnumerable` does not have a given element.
/// If it has element, it adds the given failure to the result and validation continues.
let doesNotHaveElem e (a:#seq<_>) = a.Contains(e) |> not

/// tests if a 'Proof' is valid.
let isValid p =
    match p with
    | Valid _   -> true
    | Invalid _ -> false

/// tests if a 'Proof' is invalid.
let isInvalid p = isValid p |> not

/// Flatten a list of proofs into a proof of the list
let flattenProofs ps =
    let ps' = ps |> List.map (Proof.map (fun a -> [a]))
    (Valid [], ps') ||> List.fold (Proof.combine (@))

/// Raises an `InvalidProofException` if the the given proof is `Invalid`.
let raiseIfInvalid msg p =
    match p with
    | Invalid (gfs,lfs) -> raise (InvalidProofException(msg, gfs, lfs))
    | Valid a -> a

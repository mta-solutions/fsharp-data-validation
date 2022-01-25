[<AutoOpen>]
module FSharp.Data.Validation.Default

open System.Collections.Generic
open System.Linq

let fromVCtx<'F, 'A> (ctx:VCtx<'F, 'A>): Proof<'F, 'A> =
    match ctx with
    | ValidCtx a                -> Valid a
    | DisputedCtx (gfs, lfs, _) -> Invalid (gfs, lfs)
    | RefutedCtx (gfs, lfs)     -> Invalid (gfs, lfs)

/// An interfaces that represents a value that can be validated.
type IValidatable<'F, 'B> =
    abstract Validation: unit -> VCtx<'F, 'B>

/// Runs the validations for a given value and returns the proof.
let validate<'F, 'B> (a:IValidatable<'F, 'B>): Proof<'F, 'B> =
    let ctx = a.Validation()
    fromVCtx ctx

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

/// If any element is valid, the entire value is valid.
let ifAny (fn:'U -> 'F option) (v:ValueCtx<'T> when 'T :> IEnumerable<'U>) =
    let xs = ValueCtx.getValue v
    let fs = List.ofSeq (Seq.map fn xs |> Utilities.catOptions)
    if fs.Count() = xs.Count()
        then validation { disputeMany v fs }
        else validation.Return(v)

/// Every element must be valid.
let ifAll (fn:'U -> 'F option) (v:ValueCtx<'T> when 'T :> IEnumerable<'U>) =
    let xs = ValueCtx.getValue v
    let fs = List.ofSeq (Seq.map fn xs |> Utilities.catOptions)
    match fs with
    | [] -> validation.Return(v)
    | _  -> validation { disputeMany v fs }

/// Validate each element with a given function.
let ifEach fn v =
    let xs = ValueCtx.getValue v
    let es = Seq.map fn xs
    match List.ofSeq(Utilities.errors es) with
    | []  -> validation.Return(Utilities.oks es |> ValueCtx.setValue v)
    | fs -> validation { refuteMany v fs }

/// Validate each element with a given function.
let ifEachProven fn v =
    let vs = ValueCtx.getValue v |> Seq.map (fun a -> fn a |> Proof.map Seq.singleton)
    let p = (Proof.Valid(Seq.empty), vs) ||> Seq.fold (fun acc p' ->
        match p' with
        | Valid b              ->
            match acc with
            | Valid b'              -> Valid (Seq.append b' b)
            | Invalid (gfs',lfs')   -> Invalid (gfs', lfs')
        | Invalid (gfs,lfs)    ->
            match acc with
            | Valid _               -> Invalid (gfs, lfs)
            | Invalid (gfs',lfs')   -> Invalid (List.append gfs' gfs, Utilities.mergeFailures lfs lfs')
    )
    match p with
        | Valid es -> validation.Return(ValueCtx.setValue v es)
        | Invalid (gfs, lfs) ->
            match v with
            | Global _      -> RefutedCtx (gfs, lfs)
            | Field (n,_)   -> RefutedCtx ([], Utilities.mergeFailures lfs (Map.ofList [ ([n], gfs) ]))

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

let raiseIfInvalid msg p =
    match p with
    | Invalid (gfs,lfs) -> raise (InvalidProofException(msg, gfs, lfs))
    | Valid a -> a

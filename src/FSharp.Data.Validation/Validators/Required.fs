[<AutoOpen>]
module FSharp.Data.Validation.Required

/// Checks that an `Option` value is a `Some`.
let isRequired (f: 'F) (ma: 'A option) : Result<'A, 'F> =
    match ma with
    | None -> Error f
    | Some a -> Ok a

/// Checks that a `Option` value is a `Some` when some condition is true.
let isRequiredWhen f b (ma: 'A option) : 'F option =
    match b with
    | false -> None
    | true ->
        match ma with
        | None -> Some f
        | Some _ -> None

/// Checks that a `Option` value is a `Some` when some condition is false.
let isRequiredUnless f b v = isRequiredWhen f (not b) v

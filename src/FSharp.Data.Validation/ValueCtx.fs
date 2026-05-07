namespace FSharp.Data.Validation

/// Represents the context of a value being validated, including its location (element index, field name, or global) and
/// the value itself.
type ValueCtx<'a> =
    /// Represents a value that is part of a collection, with its index and the value.
    | Element of int * 'a
    /// Represents a value that is part of a record or object, with its field name and the value.
    | Field of Name * 'a
    /// Represents a global value that is not associated with a specific field or element.
    | Global of 'a

module ValueCtx =
    /// Extracts the value from a ValueCtx, regardless of its context.
    let getValue<'A> (v: ValueCtx<'A>) : 'A =
        match v with
        | Element(_, a) -> a
        | Field(_, a) -> a
        | Global a -> a

    /// Sets a new value in the ValueCtx while preserving its context (element index, field name, or global).
    let setValue<'A, 'B> (v: ValueCtx<'A>) (b: 'B) : ValueCtx<'B> =
        match v with
        | Element(i, _a) -> Element(i, b)
        | Field(n, _a) -> Field(n, b)
        | Global _a -> Global b

    /// Maps a function over the value contained in the ValueCtx, preserving its context.
    let map (fn: 'A -> 'B) (v: ValueCtx<'A>) : ValueCtx<'B> = getValue v |> fn |> setValue v

    /// Binds a function that returns a ValueCtx to the value contained in the original ValueCtx, allowing for chaining
    /// of operations while preserving context.
    let bind (fn: 'A -> ValueCtx<'B>) (v: ValueCtx<'A>) : ValueCtx<'B> = getValue v |> fn

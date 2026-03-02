[<AutoOpen>]
module FSharp.Data.Validation.Collection

open System.Linq

/// Checks that a `IEnumerable` is empty.
/// If not, it adds the given failure to the result and validation continues.
let isNull (a: #seq<_>) = not (a.Any())

/// Checks that a `IEnumerable` is not empty.
/// If empty, it adds the given failure to the result and validation continues.
let isNotNull (a: #seq<_>) = a.Any()

/// Checks that a `IEnumerable` has a length equal to or greater than the given value.
/// If not, it adds the given failure to the result and validation continues.
let minLength l (a: #seq<_>) = a.Count() >= l

/// Checks that a `IEnumerable` has a length equal to or less than the given value.
/// If not, it adds the given failure to the result and validation continues.
let maxLength l (a: #seq<_>) = a.Count() <= l

/// Checks that a `IEnumerable` has a length equal to the given value.
/// If not, it adds the given failure to the result and validation continues.
let isLength l (a: #seq<_>) = a.Count() = l

/// Checks that a `IEnumerable` has a given element.
/// If not, it adds the given failure to the result and validation continues.
let hasElem e (a: #seq<_>) = a.Contains(e)

/// Checks that a `IEnumerable` does not have a given element.
/// If it has element, it adds the given failure to the result and validation continues.
let doesNotHaveElem e (a: #seq<_>) = a.Contains(e) |> not

/// Checks that all elements in a sequence are distinct (no duplicates).
/// If not, it adds the given failure to the result and validation continues.
let isDistinct (a: #seq<'a>) = a.Distinct().Count() = a.Count()

/// Checks that a sequence contains all of the given elements.
/// If not, it adds the given failure to the result and validation continues.
let containsAllElems (elements: 'a seq) (a: #seq<'a>) =
    elements |> Seq.forall (fun e -> a.Contains(e))

/// Checks that a sequence contains any of the given elements.
/// If not, it adds the given failure to the result and validation continues.
let containsAnyElem (elements: 'a seq) (a: #seq<'a>) =
    elements |> Seq.exists (fun e -> a.Contains(e))

/// Checks that all elements in a sequence match the given predicate.
/// If not, it adds the given failure to the result and validation continues.
let allMatch (predicate: 'a -> bool) (a: #seq<'a>) = a |> Seq.forall predicate

/// Checks that at least one element in a sequence matches the given predicate.
/// If not, it adds the given failure to the result and validation continues.
let anyMatch (predicate: 'a -> bool) (a: #seq<'a>) = a |> Seq.exists predicate

/// Checks that no elements in a sequence match the given predicate.
/// If any match, it adds the given failure to the result and validation continues.
let noneMatch (predicate: 'a -> bool) (a: #seq<'a>) = a |> Seq.exists predicate |> not

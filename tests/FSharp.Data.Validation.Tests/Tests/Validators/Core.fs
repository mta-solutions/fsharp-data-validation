module FSharp.Data.Validation.Tests.Core

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open FSharp.Data.Validation

[<Property>]
let ``fromVCTx: Transforms a ValidCtx to a Valid Proof`` (a: int) =
    let input = ValidCtx a
    let result = fromVCtx input
    Assert.Equal(Valid a, result)

[<Property>]
let ``fromVCTx: Transforms a DisputedCtx to an Invalid Proof`` (a: int, NonWhiteSpaceString n1, lf1: int, gf1: int) =
    let field1 = mkName n1 |> Option.get
    // Todo: make failures of arbitrary length
    let gfs = [ gf1 ]
    let lfs = Map.ofList [ ([ field1 ], [ lf1 ]) ]

    let input = DisputedCtx(gfs, lfs, a)
    let result = fromVCtx input
    Assert.Equal(Invalid(gfs, lfs), result)

[<Property>]
let ``fromVCTx: Transforms a RefutedCtx to an Invalid Proof`` (NonWhiteSpaceString n1, lf1: int, gf1: int) =
    let field1 = mkName n1 |> Option.get
    // Todo: make failures of arbitrary length
    let gfs = [ gf1 ]
    let lfs = Map.ofList [ ([ field1 ], [ lf1 ]) ]

    let input = RefutedCtx(gfs, lfs)
    let result = fromVCtx input
    Assert.Equal(Invalid(gfs, lfs), result)

[<Property>]
let ``isError: Returns true when Result is Error`` (NonWhiteSpaceString a) = Assert.True(isError (Error a))

[<Property>]
let ``isError: Returns false when Result is Ok`` (NonWhiteSpaceString a) = Assert.False(isError (Ok a))

[<Property>]
let ``isOk: Returns true when Result is OK`` (NonWhiteSpaceString a) = Assert.True(isOk (Ok a))

[<Property>]
let ``isOk: Returns false when Result is Error`` (NonWhiteSpaceString a) = Assert.False(isOk (Error a))

[<Property>]
let ``isEqual: true when equal`` (a: int) = Assert.True(isEqual a a)

[<Property>]
let ``isEqual: false when not equal, less than`` (a: int) = Assert.False(isEqual a (a - 1))

[<Property>]
let ``isEqual: false when not equal, greater`` (a: int) = Assert.False(isEqual a (a + 1))

[<Property>]
let ``isNotEqual: false when equal`` (a: int) = Assert.False(isNotEqual a a)

[<Property>]
let ``isNotEqual: true when not equal, less than`` (a: int) = Assert.True(isNotEqual a (a - 1))

[<Property>]
let ``isNotEqual: true when not equal, greater`` (a: int) = Assert.True(isNotEqual a (a + 1))

// For comparative operators, our validation input is b, so these may seem logically reversed
[<Property>]
let ``isLessThan: true when b is less than a`` (NegativeInt b, NonNegativeInt a) = Assert.True(isLessThan a b)

[<Property>]
let ``isLessThan: false when equal`` (a: int) = Assert.False(isLessThan a a)

[<Property>]
let ``isLessThan: false when b is greater than a`` (NonNegativeInt b, NegativeInt a) = Assert.False(isLessThan a b)

[<Property>]
let ``isGreaterThan: false when b is less than a`` (NegativeInt b, NonNegativeInt a) = Assert.False(isGreaterThan a b)

[<Property>]
let ``isGreaterThan: false when equal`` (a: int) = Assert.False(isGreaterThan a a)

[<Property>]
let ``isGreaterThan: true when b is greater than a`` (NonNegativeInt b, NegativeInt a) = Assert.True(isGreaterThan a b)

[<Property>]
let ``isLessThanOrEqual: true when b is less than a`` (NegativeInt b, NonNegativeInt a) =
    Assert.True(isLessThanOrEqual a b)

[<Property>]
let ``isLessThanOrEqual: true when equal`` (a: int) = Assert.True(isLessThanOrEqual a a)

[<Property>]
let ``isLessThanOrEqual: false when b is greater than a`` (NonNegativeInt b, NegativeInt a) =
    Assert.False(isLessThanOrEqual a b)

[<Property>]
let ``isGreaterThanOrEqual: false when b is less than a`` (NegativeInt b, NonNegativeInt a) =
    Assert.False(isGreaterThanOrEqual a b)

[<Property>]
let ``isGreaterThanOrEqual: false when equal`` (a: int) = Assert.True(isGreaterThanOrEqual a a)

[<Property>]
let ``isGreaterThanOrEqual: true when b is greater than a`` (NonNegativeInt b, NegativeInt a) =
    Assert.True(isGreaterThanOrEqual a b)

type Five = Five
type NotFiveError = NotFiveError

let mk5 i =
    validation {
        withValue i
        disputeWithFact NotFiveError (isEqual 5)
        qed (fun _ -> Five)
    }
    |> fromVCtx

let is5 i =
    if i = 5 then Ok 5 else Error NotFiveError

[<Property>]
let ``isValid: Returns true when Result is Valid`` (NonWhiteSpaceString a) = Assert.True(isValid (Valid a))

[<Fact>]
let ``isValid: Returns false when Result is Invalid`` () =
    isValid (Invalid([], Map.empty)) |> should be False

[<Property>]
let ``isInvalid: Returns false when Result is Valid`` (NonWhiteSpaceString a) = Assert.False(isInvalid (Valid a))

[<Fact>]
let ``isInvalid: Returns true when Result is Invalid`` () =
    isInvalid (Invalid([], Map.empty)) |> should be True

[<Fact>]
let ``flattenProofs: Returns valid list when all proofs are valid`` () =
    let input = [ Valid 1; Valid 2; Valid 3 ]
    let expected = Valid [ 1; 2; 3 ]
    Assert.Equal(expected, flattenProofs input)

[<Fact>]
let ``flattenProofs: Returns invalid proof when some proofs are invalid`` () =
    let input = [ Valid 1; Invalid([ "Failure" ], Map.empty); Valid 3 ]
    let expected = Invalid([ "Failure" ], Map.empty)
    Assert.Equal(expected, flattenProofs input)

[<Fact>]
let ``flattenProofs: Returns invalid proof when all proofs are invalid`` () =
    let field1 = mkName "Field1" |> Option.get

    let input =
        [ Invalid([ "GFailure1" ], Map.ofList [ ([ field1 ], [ "Failure1" ]) ])
          Invalid([ "GFailure2" ], Map.empty)
          Invalid([], Map.ofList [ ([ field1 ], [ "Failure2" ]) ]) ]

    let expected =
        Invalid([ "GFailure1"; "GFailure2" ], Map.ofList [ ([ field1 ], [ "Failure1"; "Failure2" ]) ])

    Assert.Equal(expected, flattenProofs input)

[<Property>]
let ``raiseIfInvalid: Returns value when result is Valid`` (a: int) =
    Assert.Equal(a, raiseIfInvalid "test" (Valid a))

[<Fact>]
let ``raiseIfInvalid: Raises InvalidProofException if Invalid`` () =
    (fun () -> raiseIfInvalid "test" (Invalid([ "test" ], Map.empty)) |> ignore)
    |> should (throwWithMessage "test") typeof<InvalidProofException<string>>

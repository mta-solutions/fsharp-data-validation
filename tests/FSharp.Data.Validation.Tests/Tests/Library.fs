module FSharp.Data.Validation.Tests.Library

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open FSharp.Data.Validation

[<Property>]
let ``fromVCTx: Transforms a ValidCtx to a Valid Proof``
    (a : int)
    =
    let input = ValidCtx a
    let result = fromVCtx input
    Assert.Equal(Valid a, result)

[<Property>]
let ``fromVCTx: Transforms a DisputedCtx to an Invalid Proof``
    (a : int, NonWhiteSpaceString n1, lf1 : int, gf1: int)
    =
    let field1 = mkName n1 |> Option.get
    // Todo: make failures of arbitrary length
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]

    let input = DisputedCtx (gfs, lfs, a)
    let result = fromVCtx input
    Assert.Equal(Invalid (gfs, lfs), result)

[<Property>]
let ``fromVCTx: Transforms a RefutedCtx to an Invalid Proof``
    (NonWhiteSpaceString n1, lf1 : int, gf1: int)
    =
    let field1 = mkName n1 |> Option.get
    // Todo: make failures of arbitrary length
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]

    let input = RefutedCtx (gfs, lfs)
    let result = fromVCtx input
    Assert.Equal(Invalid (gfs, lfs), result)

[<Property>]
let ``isRequired: Adds a failure to the context if the value is None``
    (NonWhiteSpaceString f1)
    =
    let input : int option = None
    let result = isRequired f1 input
    Assert.Equal(Error f1, result)

[<Property>]
let ``isRequired: Returns the value if Some``
    (a : int, NonWhiteSpaceString f1)
    =
    let input : int option = Some a
    let result = isRequired f1 input
    Assert.Equal(Ok a, result)


[<Property>]
let ``isRequiredWhen: Returns None when the value is Some``
    (a : int, NonWhiteSpaceString f1, b : bool)
    =
    let input : int option = Some a
    let result = isRequiredWhen f1 b input
    Assert.Equal(None, result)

[<Property>]
let ``isRequiredWhen: Returns None when the value is None and condition is false``
    (NonWhiteSpaceString f1)
    =
    let input : int option = None
    let result = isRequiredWhen f1 false input
    Assert.Equal(None, result)

[<Property>]
let ``isRequiredWhen: Returns Some error when the value is None and condition is true``
    (NonWhiteSpaceString f1)
    =
    let input : int option = None
    let result = isRequiredWhen f1 true input
    Assert.Equal(Some f1, result)

[<Property>]
let ``isRequiredUnless: Returns None when the value is Some``
    (a : int, NonWhiteSpaceString f1, b : bool)
    =
    let input : int option = Some a
    let result = isRequiredUnless f1 b input
    Assert.Equal( None, result)

[<Property>]
let ``isRequiredUnless: Returns Some Error when the value is None and condition is false``
    (NonWhiteSpaceString f1)
    =
    let input : int option = None
    let result = isRequiredUnless f1 false input
    Assert.Equal(Some f1, result)

[<Property>]
let ``isRequiredUnless: Returns None when the value is None and condition is true``
    (NonWhiteSpaceString f1)
    =
    let input : int option = None
    let result = isRequiredUnless f1 true input
    Assert.Equal(None, result)

[<Property>]
let ``isError: Returns true when Result is Error`` (NonWhiteSpaceString a) =
    Assert.True(isError (Error a))

[<Property>]
let ``isError: Returns false when Result is Ok`` (NonWhiteSpaceString a) =
    Assert.False(isError (Ok a))

[<Property>]
let ``isOk: Returns true when Result is OK`` (NonWhiteSpaceString a) =
    Assert.True(isOk (Ok a))

[<Property>]
let ``isOk: Returns false when Result is Error`` (NonWhiteSpaceString a) =
    Assert.False(isOk (Error a))

[<Property>]
let ``isNull: Returns true when empty`` () =
    Assert.True(isNull "")

[<Property>]
let ``isNull: Returns false when not empty`` (NonWhiteSpaceString a) =
    Assert.False(isNull a)

[<Property>]
let ``isNotNull: Returns false when empty`` () =
    Assert.False(isNotNull "")

[<Property>]
let ``isNotNull: Returns true when not empty`` (NonWhiteSpaceString a) =
    Assert.True(isNotNull a)

[<Property>]
let ``minLength: false when too short`` (NonWhiteSpaceString a, PositiveInt b) =
    Assert.False(minLength (a.Length + b) a)

[<Property>]
let ``minLength: true when correct length`` (NonWhiteSpaceString a) =
    Assert.True(minLength a.Length a)

[<Property>]
let ``minLength: true when greater than required length`` (NonWhiteSpaceString a) =
    Assert.True(minLength (a.Length - 1) a)

[<Property>]
let ``maxLength: false when too long`` (NonWhiteSpaceString a) =
    Assert.False(maxLength (a.Length - 1) a)

[<Property>]
let ``maxLength: true when correct length`` (NonWhiteSpaceString a) =
   Assert.True(maxLength a.Length a)

[<Property>]
let ``maxLength: true when less than required length`` (NonWhiteSpaceString a, PositiveInt b) =
    Assert.True(maxLength (a.Length + b) a)

[<Property>]
let ``isLength: false when too long`` (NonWhiteSpaceString a) =
    Assert.False(isLength (a.Length - 1) a)

[<Property>]
let ``isLength: true when correct length`` (NonWhiteSpaceString a) =
    Assert.True(isLength a.Length a)

[<Property>]
let ``isLength: false when less than required length`` (NonWhiteSpaceString a, PositiveInt b) =
    Assert.False(isLength (a.Length + b) a)

[<Property>]
let ``isEqual: true when equal`` (a : int) =
    Assert.True(isEqual a a )

[<Property>]
let ``isEqual: false when not equal, less than`` (a : int) =
    Assert.False(isEqual a (a - 1))

[<Property>]
let ``isEqual: false when not equal, greater`` (a : int) =
    Assert.False(isEqual a (a + 1))

[<Property>]
let ``isNotEqual: false when equal`` (a : int) =
    Assert.False(isNotEqual a a )

[<Property>]
let ``isNotEqual: true when not equal, less than`` (a : int) =
    Assert.True(isNotEqual a (a - 1))

[<Property>]
let ``isNotEqual: true when not equal, greater`` (a : int) =
    Assert.True(isNotEqual a (a + 1))

// For comparative operators, our validation input is b, so these may seem logically reversed
[<Property>]
let ``isLessThan: true when b is less than a`` (NegativeInt b, NonNegativeInt a) =
    Assert.True(isLessThan a b)

[<Property>]
let ``isLessThan: false when equal`` (a : int) =
    Assert.False(isLessThan a a)

[<Property>]
let ``isLessThan: false when b is greater than a`` (NonNegativeInt b, NegativeInt a) =
    Assert.False(isLessThan a b)

[<Property>]
let ``isGreaterThan: false when b is less than a`` (NegativeInt b, NonNegativeInt a) =
    Assert.False(isGreaterThan a b)

[<Property>]
let ``isGreaterThan: false when equal`` (a : int) =
    Assert.False(isGreaterThan a a)

[<Property>]
let ``isGreaterThan: true when b is greater than a`` (NonNegativeInt b, NegativeInt a) =
    Assert.True(isGreaterThan a b)

[<Property>]
let ``isLessThanOrEqual: true when b is less than a`` (NegativeInt b, NonNegativeInt a) =
    Assert.True(isLessThanOrEqual a b)

[<Property>]
let ``isLessThanOrEqual: true when equal`` (a : int) =
    Assert.True(isLessThanOrEqual a a)

[<Property>]
let ``isLessThanOrEqual: false when b is greater than a`` (NonNegativeInt b, NegativeInt a) =
    Assert.False(isLessThanOrEqual a b)

[<Property>]
let ``isGreaterThanOrEqual: false when b is less than a`` (NegativeInt b, NonNegativeInt a) =
    Assert.False(isGreaterThanOrEqual a b)

[<Property>]
let ``isGreaterThanOrEqual: false when equal`` (a : int) =
    Assert.True(isGreaterThanOrEqual a a)

[<Property>]
let ``isGreaterThanOrEqual: true when b is greater than a`` (NonNegativeInt b, NegativeInt a) =
    Assert.True(isGreaterThanOrEqual a b)

[<Fact>]
let ``hasElem: true when collection includes element`` () =
    let input = [1;2;5;7]
    Assert.True(hasElem 5 input)

[<Fact>]
let ``hasElem: false when collection is missing element`` () =
    let input = [1;2;5;7]
    Assert.False(hasElem 3 input)

[<Fact>]
let ``doesNotHaveElem: true when collection is missing element`` () =
    let input = [1;2;5;7]
    Assert.False(hasElem 3 input)

[<Fact>]
let ``doesNotHaveElem: false when collection includes element`` () =
    let input = [1;2;5;7]
    Assert.True(hasElem 5 input)

[<Fact>]
let ``ifAny: When no elements are valid, returns invalid`` () =
    let input = Global ([1;2;3;4] |> Seq.ofList)
    let fn a = if a = 5 then None else Some "Failure"
    let expected = DisputedCtx (["Failure"; "Failure"; "Failure"; "Failure"], Map.empty, input)
    Assert.Equal(expected, ifAny fn input)

[<Fact>]
let ``ifAny: When one element is valid, returns valid`` () =
    let input = Global ([1;2;3;5] |> Seq.ofList)
    let fn a = if a = 5 then None else Some "Failure"
    let expected = ValidCtx input
    Assert.Equal(expected, ifAny fn input)

[<Fact>]
let ``ifAny: When all elements are valid, returns valid`` () =
    let input = Global ([5;5] |> Seq.ofList)
    let fn a = if a = 5 then None else Some "Failure"
    let expected = ValidCtx input
    Assert.Equal(expected, ifAny fn input)

[<Fact>]
let ``ifAll: When no elements are valid, returns invalid`` () =
    let input = Global ([1;2;3;4] |> Seq.ofList)
    let fn a = if a = 5 then None else Some "Failure"
    let expected = DisputedCtx (["Failure"; "Failure"; "Failure"; "Failure"], Map.empty, input)
    Assert.Equal(expected, ifAll fn input)

[<Fact>]
let ``ifAll: When one element is valid, returns invalid`` () =
    let input = Global ([1;2;3;5] |> Seq.ofList)
    let fn a = if a = 5 then None else Some "Failure"
    let expected = DisputedCtx (["Failure"; "Failure"; "Failure"], Map.empty, input)
    Assert.Equal(expected, ifAll fn input)

[<Fact>]
let ``ifAll: When all elements are valid, returns valid`` () =
    let input = Global ([5;5] |> Seq.ofList)
    let fn a = if a = 5 then None else Some "Failure"
    let expected = ValidCtx input
    Assert.Equal(expected, ifAll fn input)

type Five = Five
type NotFiveError = NotFiveError

let mk5 i =
    validation {
        withValue i
        disputeWithFact NotFiveError (isEqual 5)
        whenProven (fun _ -> Five)
    } |> fromVCtx

let is5 i =
    if i = 5 then Ok 5 else Error NotFiveError

[<Fact>]
let ``ifEach: When no elements are valid, returns refuted with all errors`` () =
    let input = Global ([1;2;3;4] |> Seq.ofList)
    let expected = RefutedCtx ([NotFiveError; NotFiveError; NotFiveError; NotFiveError], Map.empty)
    Assert.Equal(expected, ifEach is5 input)

[<Fact>]
let ``ifEach: When some elements are valid, returns refuted with all errors`` () =
    let input = Global ([1;5;3;5] |> Seq.ofList)
    let expected = RefutedCtx ([NotFiveError; NotFiveError], Map.empty)
    Assert.Equal(expected, ifEach is5 input)

[<Fact>]
let ``ifEach: When all elements are valid, returns valid`` () =
    let input = Global ([5;5;5;5] |> Seq.ofList)
    let expected = ValidCtx input
    // Seqs cannot be directly compared for equality
    Assert.Equal(expected |> VCtx.map (ValueCtx.map Seq.toList),
                 ifEach is5 input |> VCtx.map (ValueCtx.map Seq.toList))

[<Fact>]
let ``ifEachProven: When no elements are proven, returns refuted with all errors`` () =
    let input = Global ([1;2;3;4] |> Seq.ofList)
    let expected = RefutedCtx ([NotFiveError; NotFiveError; NotFiveError; NotFiveError], Map.empty)
    Assert.Equal(expected, ifEachProven mk5 input)

[<Fact>]
let ``ifEachProven: Field Context, When no elements are proven, returns refuted with all errors`` () =
    let field1 = mkName "Field1" |> Option.get
    let input = Field (field1, ([1;2;3;4] |> Seq.ofList))
    let expected = RefutedCtx ([], Map.ofList [([field1], [NotFiveError; NotFiveError; NotFiveError; NotFiveError])])
    Assert.Equal(expected, ifEachProven mk5 input)

[<Fact>]
let ``ifEachProven: When some elements are proven, returns refuted with all errors`` () =
    let input = Global ([1;5;3;5] |> Seq.ofList)
    let expected = RefutedCtx ([NotFiveError; NotFiveError], Map.empty)
    Assert.Equal(expected, ifEachProven mk5 input)

[<Fact>]
let ``ifEachProven: Field Context, When some elements are proven, returns refuted with all errors`` () =
    let field1 = mkName "Field1" |> Option.get
    let input = Field (field1, ([1;5;3;5] |> Seq.ofList))
    let expected = RefutedCtx ([], Map.ofList [([field1], [NotFiveError; NotFiveError])])
    Assert.Equal(expected, ifEachProven mk5 input)

[<Fact>]
let ``ifEachProven: When all elements are proven, returns valid`` () =
    let input = Global ([5;5;5;5] |> Seq.ofList)
    let expected = ValidCtx (Global ([Five; Five; Five; Five] |> Seq.ofList))
    Assert.Equal(expected |> VCtx.map (ValueCtx.map Seq.toList),
                 ifEachProven mk5 input |> VCtx.map (ValueCtx.map Seq.toList))

[<Fact>]
let ``ifEachProven: Field Context, When all elements are proven, returns valid`` () =
    let field1 = mkName "Field1" |> Option.get
    let input = Field (field1, ([5;5;5;5] |> Seq.ofList))
    let expected = ValidCtx (Field (field1, ([Five; Five; Five; Five] |> Seq.ofList)))
    Assert.Equal(expected |> VCtx.map (ValueCtx.map Seq.toList),
                 ifEachProven mk5 input |> VCtx.map (ValueCtx.map Seq.toList))

[<Property>]
let ``isValid: Returns true when Result is Valid`` (NonWhiteSpaceString a) =
    Assert.True(isValid (Valid a))

[<Fact>]
let ``isValid: Returns false when Result is Invalid`` () =
    Assert.False(isValid (Invalid ([], Map.empty)))

[<Property>]
let ``isInvalid: Returns false when Result is Valid`` (NonWhiteSpaceString a) =
    Assert.False(isInvalid (Valid a))

[<Fact>]
let ``isInvalid: Returns true when Result is Invalid`` () =
    Assert.True(isInvalid (Invalid ([], Map.empty)))

[<Fact>]
let ``flattenProofs: Returns valid list when all proofs are valid`` () =
    let input = [Valid 1; Valid 2; Valid 3]
    let expected = Valid [1; 2; 3]
    Assert.Equal(expected, flattenProofs input)

[<Fact>]
let ``flattenProofs: Returns invalid proof when some proofs are invalid`` () =
    let input = [Valid 1; Invalid (["Failure"], Map.empty); Valid 3]
    let expected = Invalid (["Failure"], Map.empty)
    Assert.Equal(expected, flattenProofs input)

[<Fact>]
let ``flattenProofs: Returns invalid proof when all proofs are invalid`` () =
    let field1 = mkName "Field1" |> Option.get
    let input = [
        Invalid (["GFailure1"], Map.ofList [([field1], ["Failure1"])])
        Invalid (["GFailure2"], Map.empty)
        Invalid ([], Map.ofList [([field1], ["Failure2"])])
    ]
    let expected = Invalid (["GFailure1"; "GFailure2"], Map.ofList [([field1], ["Failure1"; "Failure2"])])
    Assert.Equal(expected, flattenProofs input)

[<Property>]
let ``raiseIfInvalid: Returns value when result is Valid`` (a : int) =
    raiseIfInvalid "test" (Valid a) |> should equal a

[<Fact>]
let ``raiseIfInvalid: Raises InvalidProofException if Invalid`` () =
    (fun () -> raiseIfInvalid "test" (Invalid (["test"], Map.empty)) |> ignore)
    |> should (throwWithMessage "test") typeof<InvalidProofException<string>>

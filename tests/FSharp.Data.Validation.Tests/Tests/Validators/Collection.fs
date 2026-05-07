module FSharp.Data.Validation.Tests.Collection

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open FSharp.Data.Validation

[<Property>]
let ``isNull: Returns true when empty`` () = Assert.True(isNull "")

[<Property>]
let ``isNull: Returns false when not empty`` (NonWhiteSpaceString a) = Assert.False(isNull a)

[<Property>]
let ``isNotNull: Returns false when empty`` () = Assert.False(isNotNull "")

[<Property>]
let ``isNotNull: Returns true when not empty`` (NonWhiteSpaceString a) = Assert.True(isNotNull a)

[<Property>]
let ``minLength: false when too short`` (NonWhiteSpaceString a, PositiveInt b) =
    Assert.False(minLength (a.Length + b) a)

[<Property>]
let ``minLength: true when correct length`` (NonWhiteSpaceString a) = Assert.True(minLength a.Length a)

[<Property>]
let ``minLength: true when greater than required length`` (NonWhiteSpaceString a) =
    Assert.True(minLength (a.Length - 1) a)

[<Property>]
let ``maxLength: false when too long`` (NonWhiteSpaceString a) =
    Assert.False(maxLength (a.Length - 1) a)

[<Property>]
let ``maxLength: true when correct length`` (NonWhiteSpaceString a) = Assert.True(maxLength a.Length a)

[<Property>]
let ``maxLength: true when less than required length`` (NonWhiteSpaceString a, PositiveInt b) =
    Assert.True(maxLength (a.Length + b) a)

[<Property>]
let ``isLength: false when too long`` (NonWhiteSpaceString a) = Assert.False(isLength (a.Length - 1) a)

[<Property>]
let ``isLength: true when correct length`` (NonWhiteSpaceString a) = Assert.True(isLength a.Length a)

[<Property>]
let ``isLength: false when less than required length`` (NonWhiteSpaceString a, PositiveInt b) =
    Assert.False(isLength (a.Length + b) a)

[<Fact>]
let ``hasElem: true when collection includes element`` () =
    let input = [ 1; 2; 5; 7 ]
    hasElem 5 input |> should be True

[<Fact>]
let ``hasElem: false when collection is missing element`` () =
    let input = [ 1; 2; 5; 7 ]
    hasElem 3 input |> should be False

[<Fact>]
let ``doesNotHaveElem: true when collection is missing element`` () =
    let input = [ 1; 2; 5; 7 ]
    doesNotHaveElem 3 input |> should be True

[<Fact>]
let ``doesNotHaveElem: false when collection includes element`` () =
    let input = [ 1; 2; 5; 7 ]
    doesNotHaveElem 5 input |> should be False

[<Fact>]
let ``isDistinct: Returns true when all elements are unique`` () =
    isDistinct [ 1; 2; 3; 4; 5 ] |> should be True

[<Fact>]
let ``isDistinct: Returns false when elements are duplicated`` () =
    isDistinct [ 1; 2; 3; 2; 5 ] |> should be False

[<Fact>]
let ``containsAllElems: Returns true when sequence contains all elements`` () =
    containsAllElems [ 1; 2; 3 ] [ 1; 2; 3; 4; 5 ] |> should be True

[<Fact>]
let ``containsAllElems: Returns false when sequence does not contain all elements`` () =
    containsAllElems [ 1; 2; 6 ] [ 1; 2; 3; 4; 5 ] |> should be False

[<Fact>]
let ``containsAnyElem: Returns true when sequence contains at least one element`` () =
    containsAnyElem [ 1; 6; 7 ] [ 1; 2; 3; 4; 5 ] |> should be True

[<Fact>]
let ``containsAnyElem: Returns false when sequence contains none of the elements`` () =
    containsAnyElem [ 6; 7; 8 ] [ 1; 2; 3; 4; 5 ] |> should be False

[<Fact>]
let ``allMatch: Returns true when all elements match predicate`` () =
    allMatch (fun x -> x > 0) [ 1; 2; 3; 4; 5 ] |> should be True

[<Fact>]
let ``allMatch: Returns false when not all elements match predicate`` () =
    allMatch (fun x -> x > 3) [ 1; 2; 3; 4; 5 ] |> should be False

[<Fact>]
let ``anyMatch: Returns true when at least one element matches predicate`` () =
    anyMatch (fun x -> x > 3) [ 1; 2; 3; 4; 5 ] |> should be True

[<Fact>]
let ``anyMatch: Returns false when no elements match predicate`` () =
    anyMatch (fun x -> x > 10) [ 1; 2; 3; 4; 5 ] |> should be False

[<Fact>]
let ``noneMatch: Returns true when no elements match predicate`` () =
    noneMatch (fun x -> x > 10) [ 1; 2; 3; 4; 5 ] |> should be True

[<Fact>]
let ``noneMatch: Returns false when at least one element matches predicate`` () =
    noneMatch (fun x -> x > 3) [ 1; 2; 3; 4; 5 ] |> should be False

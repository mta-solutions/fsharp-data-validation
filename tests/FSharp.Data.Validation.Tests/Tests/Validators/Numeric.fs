module FSharp.Data.Validation.Tests.Numeric

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open FSharp.Data.Validation

[<Property>]
let ``inRange: Returns true when value is within range`` (a: int) =
    let min = a - 10
    let max = a + 10
    Assert.True(inRange min max a)

[<Property>]
let ``inRange: Returns false when value is outside range`` (a: int) =
    let min = a + 10
    let max = a + 20
    Assert.False(inRange min max a)

[<Property>]
let ``inRangeExclusive: Returns true when value is within exclusive range`` (a: int) =
    let min = a - 10
    let max = a + 10
    Assert.True(inRangeExclusive min max a)

[<Property>]
let ``inRangeExclusive: Returns false when value equals boundary`` (a: int) = Assert.False(inRangeExclusive a 100 a)

[<Property>]
let ``isPositive: Returns true for positive numbers`` (PositiveInt a) = Assert.True(isPositive a)

[<Fact>]
let ``isPositive: Returns false for zero`` () = isPositive 0 |> should be False

[<Property>]
let ``isNegative: Returns true for negative numbers`` (NegativeInt a) = Assert.True(isNegative a)

[<Fact>]
let ``isNegative: Returns false for zero`` () = isNegative 0 |> should be False

[<Property>]
let ``isNonZero: Returns true for non-zero numbers`` (NonZeroInt a) = Assert.True(isNonZero a)

[<Fact>]
let ``isNonZero: Returns false for zero`` () = isNonZero 0 |> should be False

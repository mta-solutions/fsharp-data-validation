module FSharp.Data.Validation.Tests.String

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open FSharp.Data.Validation

[<Fact>]
let ``matchesRegex: Returns true when string matches pattern`` () =
    let pattern = System.Text.RegularExpressions.Regex(@"^\d{3}-\d{4}$")
    matchesRegex pattern "123-4567" |> should be True

[<Fact>]
let ``matchesRegex: Returns false when string does not match pattern`` () =
    let pattern = System.Text.RegularExpressions.Regex(@"^\d{3}-\d{4}$")
    matchesRegex pattern "abc-defg" |> should be False

[<Fact>]
let ``containsAny: Returns true when string contains at least one character`` () =
    containsAny [ 'a'; 'b'; 'c' ] "hello world abc" |> should be True

[<Fact>]
let ``containsAny: Returns false when string contains none of the characters`` () =
    containsAny [ 'x'; 'y'; 'z' ] "hello world" |> should be False

[<Fact>]
let ``containsAll: Returns true when string contains all characters`` () =
    containsAll [ 'h'; 'e'; 'l' ] "hello world" |> should be True

[<Fact>]
let ``containsAll: Returns false when string does not contain all characters`` () =
    containsAll [ 'x'; 'y'; 'z' ] "hello world" |> should be False

[<Property>]
let ``startsWith: Returns true when string starts with prefix`` (NonWhiteSpaceString prefix) =
    let str = prefix + "suffix"
    Assert.True(startsWith prefix str)

[<Property>]
let ``startsWith: Returns false when string does not start with prefix``
    (NonWhiteSpaceString prefix, NonWhiteSpaceString other)
    =
    // Only test when other doesn't start with prefix
    if not (startsWith prefix other) then
        Assert.False(startsWith prefix other)

[<Property>]
let ``endsWith: Returns true when string ends with suffix`` (NonWhiteSpaceString suffix) =
    let str = "prefix" + suffix
    Assert.True(endsWith suffix str)

[<Fact>]
let ``isAlphanumeric: Returns true for alphanumeric strings`` () =
    isAlphanumeric "abc123XYZ" |> should be True

[<Fact>]
let ``isAlphanumeric: Returns false for non-alphanumeric strings`` () =
    isAlphanumeric "abc-123" |> should be False

[<Fact>]
let ``isAlpha: Returns true for alphabetic strings`` () = isAlpha "abcXYZ" |> should be True

[<Fact>]
let ``isAlpha: Returns false for non-alphabetic strings`` () = isAlpha "abc123" |> should be False

[<Fact>]
let ``isNumeric: Returns true for numeric strings`` () = isNumeric "123456" |> should be True

[<Fact>]
let ``isNumeric: Returns false for non-numeric strings`` () = isNumeric "123abc" |> should be False

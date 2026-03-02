module FSharp.Data.Validation.Tests.Required

open Xunit
open FsCheck
open FsCheck.Xunit

open FSharp.Data.Validation

[<Property>]
let ``isRequired: Adds a failure to the context if the value is None`` (NonWhiteSpaceString f1) =
    let input: int option = None
    let result = isRequired f1 input
    Assert.Equal(Error f1, result)

[<Property>]
let ``isRequired: Returns the value if Some`` (a: int, NonWhiteSpaceString f1) =
    let input: int option = Some a
    let result = isRequired f1 input
    Assert.Equal(Ok a, result)

[<Property>]
let ``isRequiredWhen: Returns None when the value is Some`` (a: int, NonWhiteSpaceString f1, b: bool) =
    let input: int option = Some a
    let result = isRequiredWhen f1 b input
    Assert.Equal(None, result)

[<Property>]
let ``isRequiredWhen: Returns None when the value is None and condition is false`` (NonWhiteSpaceString f1) =
    let input: int option = None
    let result = isRequiredWhen f1 false input
    Assert.Equal(None, result)

[<Property>]
let ``isRequiredWhen: Returns Some error when the value is None and condition is true`` (NonWhiteSpaceString f1) =
    let input: int option = None
    let result = isRequiredWhen f1 true input
    Assert.Equal(Some f1, result)

[<Property>]
let ``isRequiredUnless: Returns None when the value is Some`` (a: int, NonWhiteSpaceString f1, b: bool) =
    let input: int option = Some a
    let result = isRequiredUnless f1 b input
    Assert.Equal(None, result)

[<Property>]
let ``isRequiredUnless: Returns Some Error when the value is None and condition is false`` (NonWhiteSpaceString f1) =
    let input: int option = None
    let result = isRequiredUnless f1 false input
    Assert.Equal(Some f1, result)

[<Property>]
let ``isRequiredUnless: Returns None when the value is None and condition is true`` (NonWhiteSpaceString f1) =
    let input: int option = None
    let result = isRequiredUnless f1 true input
    Assert.Equal(None, result)

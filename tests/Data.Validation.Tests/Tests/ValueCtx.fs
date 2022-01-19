module Data.Validation.Tests.ValueCtx

open Xunit
open FsCheck
open FsCheck.Xunit

open Data.Validation

[<Property>]
let ``getValue: Retrieves the value from a global context``
    (gf1 : string)
    =
    let input = Global gf1
    let result = ValueCtx.getValue input
    Assert.Equal(result, gf1)

[<Property>]
let ``getValue: Retrieves the value from a field context``
    (NonWhiteSpaceString n1, lf1 : string)
    =
    let field1 = mkName n1 |> Option.get
    let input = Field (field1, lf1)
    let result = ValueCtx.getValue input
    Assert.Equal(result, lf1)

[<Property>]
let ``setValue: Sets the value of a global context``
    (gf1 : string, gf2 : string)
    =
    let input = Global gf1
    let result = ValueCtx.setValue input gf2
    Assert.Equal(result, Global gf2)

[<Property>]
let ``setValue: Sets the value of a field context``
    (NonWhiteSpaceString n1, lf1 : string, lf2 : string)
    =
    let field1 = mkName n1 |> Option.get
    let input = Field (field1, lf1)
    let result = ValueCtx.setValue input lf2
    Assert.Equal(result, Field (field1, lf2))

[<Property>]
let ``map: Transforms a global context``
    (gf1 : int)
    =
    let input = Global gf1
    let result = ValueCtx.map (fun a -> a.ToString()) input
    Assert.Equal(result, Global (gf1.ToString()))

[<Property>]
let ``map: Transforms a field context while preserving the field name``
    (NonWhiteSpaceString n1, lf1 : int)
    =
    let field1 = mkName n1 |> Option.get
    let input = Field (field1, lf1)
    let result = ValueCtx.map (fun a -> a.ToString()) input
    Assert.Equal(result, Field (field1, lf1.ToString()))

[<Property>]
let ``bind: Transforms a global context``
    (NonWhiteSpaceString n1, gf1 : int)
    =
    let field1 = mkName n1 |> Option.get
    let input = Global gf1
    let result = ValueCtx.bind (fun a -> Field (field1, a)) input
    Assert.Equal(result, Field (field1, gf1))

[<Property>]
let ``bind: Transforms a field context``
    (NonWhiteSpaceString n1, lf1 : int)
    =
    let field1 = mkName n1 |> Option.get
    let input = Field (field1, lf1)
    let result = ValueCtx.bind Global input
    Assert.Equal(result, Global lf1)
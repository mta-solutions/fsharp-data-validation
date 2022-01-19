module Data.Validation.Tests.Library

open Xunit
open FsCheck
open FsCheck.Xunit

open Data.Validation

[<Property>]
let ``fromVCTx: Transforms a ValidCtx to a Valid Proof``
    (a : int)
    =
    let input = ValidCtx a
    let result = fromVCtx input
    Assert.Equal(result, Valid a)
    
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
    Assert.Equal(result, Invalid (gfs, lfs))
   
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
    Assert.Equal(result, Invalid (gfs, lfs))
    
[<Property>]
let ``isRequired: Adds a failure to the context if the value is None``
    (NonWhiteSpaceString f1)
    =
    let input : int option = None
    let result = isRequired f1 input
    Assert.Equal(result, Error f1)
    
[<Property>]
let ``isRequired: Returns the value if Some``
    (a : int, NonWhiteSpaceString f1)
    =
    let input : int option = Some a
    let result = isRequired f1 input
    Assert.Equal(result, Ok a)
    

[<Property>]
let ``isRequiredWhen: Returns None when the value is Some``
    (a : int, NonWhiteSpaceString f1, b : bool)
    =
    let input : int option = Some a
    let result = isRequiredWhen f1 b input
    Assert.Equal(result, None)
    
[<Property>]
let ``isRequiredWhen: Returns None when the value is None and condition is false``
    (NonWhiteSpaceString f1)
    =
    let input : int option = None
    let result = isRequiredWhen f1 false input
    Assert.Equal(result, None)
    
[<Property>]
let ``isRequiredWhen: Returns Some error when the value is None and condition is true``
    (NonWhiteSpaceString f1)
    =
    let input : int option = None
    let result = isRequiredWhen f1 true input
    Assert.Equal(result, Some f1)
    
[<Property>]
let ``isRequiredUnless: Returns None when the value is Some``
    (a : int, NonWhiteSpaceString f1, b : bool)
    =
    let input : int option = Some a
    let result = isRequiredUnless f1 b input
    Assert.Equal(result, None)
    
[<Property>]
let ``isRequiredUnless: Returns Some Error when the value is None and condition is false``
    (NonWhiteSpaceString f1)
    =
    let input : int option = None
    let result = isRequiredUnless f1 false input
    Assert.Equal(result, Some f1)
    
[<Property>]
let ``isRequiredUnless: Returns None when the value is None and condition is true``
    (NonWhiteSpaceString f1)
    =
    let input : int option = None
    let result = isRequiredUnless f1 true input
    Assert.Equal(result, None)
    
[<Property>]
let ``isError: Returns true when Result is Error`` (NonWhiteSpaceString a) =
    Assert.Equal(isError (Error a), true)
    
[<Property>]
let ``isError: Returns false when Result is Ok`` (NonWhiteSpaceString a) =
    Assert.Equal(isError (Ok a), false)
    
[<Property>]
let ``isOk: Returns true when Result is OK`` (NonWhiteSpaceString a) =
    Assert.Equal(isOk (Ok a), true)
    
[<Property>]
let ``isOk: Returns false when Result is Error`` (NonWhiteSpaceString a) =
    Assert.Equal(isOk (Error a), false)
    
[<Property>]
let ``isNull: Returns true when empty`` () =
    Assert.Equal(isNull "", true)
    
[<Property>]
let ``isNull: Returns false when not empty`` (NonWhiteSpaceString a) =
    Assert.Equal(isNull a, false)
    
[<Property>]
let ``isNotNull: Returns false when empty`` () =
    Assert.Equal(isNotNull "", false)
    
[<Property>]
let ``isNotNull: Returns true when not empty`` (NonWhiteSpaceString a) =
    Assert.Equal(isNotNull a, true)
    
[<Property>]
let ``minLength: Returns false when too short`` (NonWhiteSpaceString a, PositiveInt b) =
    Assert.Equal(minLength (a.Length + b) a, false)
    
[<Property>]
let ``minLength: Returns true when correct length`` (NonWhiteSpaceString a) =
    Assert.Equal(minLength a.Length a, true)
    
[<Property>]
let ``minLength: Returns true when greater than required length`` (NonWhiteSpaceString a) =
    Assert.Equal(minLength (a.Length - 1) a, true)
    
// TODO: Finish unit testing library functions
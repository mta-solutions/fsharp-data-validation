module Data.Validation.Tests.Proof

open Xunit
open FsCheck
open FsCheck.Xunit
open System.Text.Json

open Data.Validation

[<Property>]
let ``map: Does not change the contents of an invalid proof``
    (gf1, NonWhiteSpaceString n1, lf1)
    =
    let field1 = mkName n1 |> Option.get
    let input : Proof<string, int> = Invalid ([gf1], Map.ofList [([field1], [lf1])])
    let result = Proof.map (fun a -> a + 1) input
    Assert.Equal(result, input)

[<Property>]
let ``map: Converts a Proof<f, a> to a Proof<f, b>``
    (n : int)
    =
    let input = Valid n
    let result = Proof.map (fun a -> a.ToString()) input
    Assert.Equal(result, Valid (n.ToString()))

[<Property(Verbose = false)>]
let ``combine: two valid proof results in valid proof`` (a : int, b : int) =
    let input1 = Valid a
    let input2 = Valid b
    let result = Proof.combine (+) input1 input2
    Assert.Equal(result, Valid (a + b))

[<Property(Verbose = false)>]
let ``combine: one valid and one invalid proof results in invalid proof``
    (a : int, b : string, NonWhiteSpaceString c, d : string)
    =
    let field1 = mkName c |> Option.get
    let input1 = Valid a
    let input2 =
        Invalid ([b], Map.ofList [([field1], [d])])
    let result = Proof.combine (+) input1 input2
    Assert.Equal(result, input2)
    
[<Property>]
let ``combine: one invalid and one valid proof results in invalid proof``
    (a : string, NonWhiteSpaceString b, c : string, d: int)
    =
    let field1 = mkName b |> Option.get
    let input1 =
        Invalid ([a], Map.ofList [([field1], [c])])
    let input2 = Valid d
    let result = Proof.combine (+) input1 input2
    Assert.Equal(result, input1)
    
[<Property>]
let ``combine: two invalid proofs results in concatenated errors``
    (gf1, gf2, NonWhiteSpaceString n1, NonWhiteSpaceString n2, lf1 : string, lf2 : string, lf3 : string)
    =
    let field1 = mkName n1 |> Option.get
    let field2 = mkName n2 |> Option.get
    let input1 =
        Invalid ([gf1], Map.ofList [([field1], [lf1])])
    let input2 =
        Invalid ([gf2], Map.ofList [([field1], [lf2]); ([field2], [lf3])])
    let result = Proof.combine (+) input1 input2
    let expected =
        Invalid ([gf1; gf2], Map.ofList [([field1], [lf1; lf2]); ([field2], [lf3])])
    Assert.Equal(result, expected)

type MyRecord = { MyName: string; MyInt: int; }
    
[<Fact>]
let ``serialize: valid proof of type T should result in JSON representing T`` () =
    // Arrange
    let sot = Valid ({ MyName = "John Smith"; MyInt = 42 })

    // Act
    let json = JsonSerializer.Serialize(sot)

    //Assert
    Assert.Equal("{\"MyName\":\"John Smith\",\"MyInt\":42}", json)

type MyFailures = 
    | EmptyName
    | IntToSmall
    | NameAndNumberDoNotMatch of string * int
    override this.ToString() = 
        match this with
        | EmptyName                     -> "MyName cannot be empty."
        | IntToSmall                    -> "MyInt cannot be less than 42."
        | NameAndNumberDoNotMatch (n,i) -> sprintf "%s's number can only be 42, not %i." n i
    
[<Fact>]
let ``serialize: invalid proof of type T should result in JSON representing the failures`` () =
    // Arrange
    let myName = (mkName "MyName").Value
    let myObj = (mkName "MyObj").Value
    let myInt = (mkName "MyInt").Value
    let gfs = [NameAndNumberDoNotMatch ("John Smith", 41)]
    let lfs = Map.ofList [([myName], [EmptyName]); ([myObj; myInt], [IntToSmall])]
    let sot = Invalid (gfs, lfs)

    // Act
    let json = JsonSerializer.Serialize(sot)

    //Assert
    Assert.Equal("{\"failures\":[\"John Smith\\u0027s number can only be 42, not 41.\"],\"fields\":{\"myName\":[\"MyName cannot be empty.\"],\"myObj.myInt\":[\"MyInt cannot be less than 42.\"]}}", json)

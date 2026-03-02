module FSharp.Data.Validation.Tests.Proof

open Xunit
open FsCheck
open FsCheck.Xunit
open System.Text.Json

open FSharp.Data.Validation

[<Property>]
let ``map: Does not change the contents of an invalid proof``
    (gf1, NonWhiteSpaceString n1, lf1)
    =
    let field1 = mkName n1 |> Option.get
    let input : Proof<string, int> = Invalid ([gf1], Map.ofList [([field1], [lf1])])
    let result = Proof.map (fun a -> a + 1) input
    Assert.Equal(input, result)

[<Property>]
let ``map: Converts a Proof<f, a> to a Proof<f, b>``
    (n : int)
    =
    let input = Valid n
    let result = Proof.map (fun a -> a.ToString()) input
    Assert.Equal(Valid (n.ToString()), result)

[<Property>]
let ``mapInvalid: Does not change the contents of a valid proof``
    (n : int)
    =
    let input = Valid n
    let result = Proof.mapInvalid (fun a -> a + 1) input
    Assert.Equal(input, result)

[<Property>]
let ``mapInvalid: Converts a Proof<f, a> to a Proof<g, a>``
    (gf1 : int, lf1 : int, NonWhiteSpaceString n1)
    =
    let field1 = mkName n1 |> Option.get
    let input : Proof<int, int> = Invalid ([gf1], Map.ofList [([field1], [lf1])])
    let result = Proof.mapInvalid (fun a -> a.ToString()) input
    let expected : Proof<string, int> = Invalid ([gf1.ToString()], Map.ofList [([field1], [lf1.ToString()])])
    Assert.Equal(expected, result)


[<Property(Verbose = false)>]
let ``combine: two valid proof results in valid proof`` (a : int, b : int) =
    let input1 = Valid a
    let input2 = Valid b
    let result = Proof.combine (+) input1 input2
    Assert.Equal(Valid (a + b), result)

[<Property(Verbose = false)>]
let ``combine: one valid and one invalid proof results in invalid proof``
    (a : int, b : string, NonWhiteSpaceString c, d : string)
    =
    let field1 = mkName c |> Option.get
    let input1 = Valid a
    let input2 =
        Invalid ([b], Map.ofList [([field1], [d])])
    let result = Proof.combine (+) input1 input2
    Assert.Equal(input2, result)

[<Property>]
let ``combine: one invalid and one valid proof results in invalid proof``
    (a : string, NonWhiteSpaceString b, c : string, d: int)
    =
    let field1 = mkName b |> Option.get
    let input1 =
        Invalid ([a], Map.ofList [([field1], [c])])
    let input2 = Valid d
    let result = Proof.combine (+) input1 input2
    Assert.Equal(input1, result)

[<Property>]
let ``combine: two invalid proofs results in concatenated errors``
    (gf1, gf2, lf1 : string, lf2 : string, lf3 : string)
    =
    let field1 = mkName "Field1" |> Option.get
    let field2 = mkName "Field2" |> Option.get
    let input1 =
        Invalid ([gf1], Map.ofList [([field1], [lf1])])
    let input2 =
        Invalid ([gf2], Map.ofList [([field1], [lf2]); ([field2], [lf3])])
    let result = Proof.combine (+) input1 input2
    let expected =
        Invalid ([gf1; gf2], Map.ofList [([field1], [lf1; lf2]); ([field2], [lf3])])
    Assert.Equal(expected, result)

type MyRecord = { MyName: string; MyInt: int; }

[<Fact>]
let ``serialize: valid proof of type T should result in JSON representing T`` () =
    // Arrange
    let sot = Valid { MyName = "John Smith"; MyInt = 42 }

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

[<Property>]
let ``toResult: Valid proof becomes Ok`` (a: int) =
    let input = Valid a
    let result = Proof.toResult input
    Assert.Equal(Ok a, result)

[<Property>]
let ``toResult: Invalid proof becomes Error with both global and field failures``
    (gf1: string, gf2: string, lf1: string, NonWhiteSpaceString field1Name)
    =
    let field1 = mkName field1Name |> Option.get

    let input: Proof<string, int> =
        Invalid([ gf1; gf2 ], Map.ofList [ ([ field1 ], [ lf1 ]) ])

    let result = Proof.toResult input

    match result with
    | Error vf ->
        Assert.Equal<string seq>([ gf1; gf2 ], vf.Failures)
        Assert.True(Map.containsKey [ field1 ] vf.Fields)
    | _ -> failwith "Expected Error result"

[<Property>]
let ``toValidationFailures: Valid proof returns None`` (a: int) =
    let input = Valid a
    let result = Proof.toValidationFailures input
    Assert.Null(result)

[<Property>]
let ``toValidationFailures: Invalid proof with only global failures`` (gf1: string, gf2: string) =
    let input: Proof<string, int> = Invalid([ gf1; gf2 ], Map.empty)
    let result = Proof.toValidationFailures input

    match result with
    | Some vf -> Assert.Equal<string seq>([ gf1; gf2 ], vf.Failures)
    | None -> failwith "Expected Some result"

[<Property>]
let ``toValidationFailures: Invalid proof with field and global failures``
    (gf1: string, lf1: string, NonWhiteSpaceString field1Name)
    =
    let field1 = mkName field1Name |> Option.get

    let input: Proof<string, int> =
        Invalid([ gf1 ], Map.ofList [ ([ field1 ], [ lf1 ]) ])

    let result = Proof.toValidationFailures input
    // Should include both global and field failures
    match result with
    | Some vf ->
        Assert.Equal<string seq>([ gf1 ], vf.Failures)
        Assert.True(Map.containsKey [ field1 ] vf.Fields)
    | None -> failwith "Expected Some result"

// New Proof combinator tests

[<Fact>]
let ``sequence: Returns Valid list when all proofs are Valid`` () =
    let proofs = [ Valid 1; Valid 2; Valid 3 ]
    let result = Proof.sequence proofs
    result |> should equal (Valid [ 1; 2; 3 ])

[<Fact>]
let ``sequence: Returns Invalid when any proof is Invalid`` () =
    let field1 = mkName "Field1" |> Option.get

    let proofs =
        [ Valid 1
          Invalid([ "Error" ], Map.ofList [ ([ field1 ], [ "FieldError" ]) ])
          Valid 3 ]

    match Proof.sequence proofs with
    | Invalid(gfs, lfs) ->
        gfs |> should equal [ "Error" ]
        Map.containsKey [ field1 ] lfs |> should be True
    | _ -> failwith "Expected Invalid result"

[<Fact>]
let ``sequence: Aggregates failures from multiple Invalid proofs`` () =
    let field1 = mkName "Field1" |> Option.get
    let field2 = mkName "Field2" |> Option.get

    let proofs =
        [ Invalid([ "Error1" ], Map.ofList [ ([ field1 ], [ "FieldError1" ]) ])
          Invalid([ "Error2" ], Map.ofList [ ([ field2 ], [ "FieldError2" ]) ]) ]

    match Proof.sequence proofs with
    | Invalid(gfs, lfs) ->
        gfs |> should equal [ "Error1"; "Error2" ]
        Map.count lfs |> should equal 2
    | _ -> failwith "Expected Invalid result"

[<Fact>]
let ``traverse: Maps and sequences valid proofs`` () =
    let items = [ 1; 2; 3 ]
    let fn x = Valid(x * 2)
    let result = Proof.traverse fn items
    result |> should equal (Valid [ 2; 4; 6 ])

[<Fact>]
let ``traverse: Returns Invalid when any mapping fails`` () =
    let items = [ 1; 2; 3 ]

    let fn x =
        if x = 2 then
            Invalid([ "Error" ], Map.empty)
        else
            Valid(x * 2)

    match Proof.traverse fn items with
    | Invalid(gfs, _) -> gfs |> should equal [ "Error" ]
    | _ -> failwith "Expected Invalid result"

[<Property>]
let ``bind: Chains Valid proofs`` (a: int) =
    let proof = Valid a
    let fn x = Valid(x + 1)
    let result = Proof.bind fn proof
    Assert.Equal(Valid(a + 1), result)

[<Property>]
let ``bind: Returns Invalid when first proof is Invalid`` (gf: string) =
    let proof: Proof<string, int> = Invalid([ gf ], Map.empty)
    let fn x = Valid(x + 1)
    let result = Proof.bind fn proof
    Assert.Equal(proof, result)

[<Property>]
let ``bind: Returns Invalid when function returns Invalid`` (a: int, gf: string) =
    let proof = Valid a
    let fn _ = Invalid([ gf ], Map.empty)

    match Proof.bind fn proof with
    | Invalid(gfs, _) -> Assert.Equal<string seq>([ gf ], gfs)
    | _ -> failwith "Expected Invalid result"

[<Property>]
let ``apply: Applies function to value when both are Valid`` (a: int) =
    let fnProof = Valid(fun x -> x + 1)
    let valueProof = Valid a
    let result = Proof.apply fnProof valueProof
    Assert.Equal(Valid(a + 1), result)

[<Property>]
let ``apply: Returns Invalid when function proof is Invalid`` (a: int, gf: string) =
    let fnProof: Proof<string, int -> int> = Invalid([ gf ], Map.empty)
    let valueProof = Valid a

    match Proof.apply fnProof valueProof with
    | Invalid(gfs, _) -> Assert.Equal<string seq>([ gf ], gfs)
    | _ -> failwith "Expected Invalid result"

[<Property>]
let ``apply: Returns Invalid when value proof is Invalid`` (gf: string) =
    let fnProof = Valid(fun x -> x + 1)
    let valueProof: Proof<string, int> = Invalid([ gf ], Map.empty)

    match Proof.apply fnProof valueProof with
    | Invalid(gfs, _) -> Assert.Equal<string seq>([ gf ], gfs)
    | _ -> failwith "Expected Invalid result"

[<Property>]
let ``apply: Aggregates failures when both are Invalid`` (gf1: string, gf2: string) =
    let fnProof: Proof<string, int -> int> = Invalid([ gf1 ], Map.empty)
    let valueProof: Proof<string, int> = Invalid([ gf2 ], Map.empty)

    match Proof.apply fnProof valueProof with
    | Invalid(gfs, _) -> Assert.Equal<string seq>([ gf1; gf2 ], gfs)
    | _ -> failwith "Expected Invalid result"

[<Property>]
let ``choose: Returns first proof when Valid`` (a: int, b: int) =
    let first = Valid a
    let second = Valid b
    let result = Proof.choose first second
    Assert.Equal(first, result)

[<Property>]
let ``choose: Returns second proof when first is Invalid and second is Valid`` (a: int, gf: string) =
    let first: Proof<string, int> = Invalid([ gf ], Map.empty)
    let second = Valid a
    let result = Proof.choose first second
    Assert.Equal(second, result)

[<Property>]
let ``choose: Returns first proof when both are Invalid`` (gf1: string, gf2: string) =
    let first: Proof<string, int> = Invalid([ gf1 ], Map.empty)
    let second: Proof<string, int> = Invalid([ gf2 ], Map.empty)
    let result = Proof.choose first second
    Assert.Equal(first, result)

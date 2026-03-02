module FSharp.Data.Validation.Tests.Utilities

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open FSharp.Data.Validation
open FSharp.Data.Validation.Utilities

[<Fact>]
let ``catOptions: empty list results in empty list`` () =
    catOptions [] |> Seq.toList |> should equal []

[<Fact>]
let ``catOptions: all Some list results in all elements`` () =
    catOptions [ Some 1; Some 2 ] |> Seq.toList |> should equal [ 1; 2 ]

[<Fact>]
let ``catOptions: all None list results in empty list`` () =
    catOptions [ None; None ] |> Seq.toList |> should equal []

[<Fact>]
let ``catOptions: mixed list results in only Somes`` () =
    catOptions [ Some 1; None; Some 2; None ] |> Seq.toList |> should equal [ 1; 2 ]

[<Fact>]
let ``oks: empty list results in empty list`` () = oks [] |> Seq.toList |> should equal []

[<Fact>]
let ``oks: all Ok list results in all elements`` () =
    oks [ Ok 1; Ok 2 ] |> Seq.toList |> should equal [ 1; 2 ]

[<Fact>]
let ``oks: all Error list results in empty list`` () =
    oks [ Error "String 1"; Error "String 2" ] |> Seq.toList |> should equal []

[<Fact>]
let ``oks: mixed list results in only Oks`` () =
    oks [ Ok 1; Error "String 1"; Ok 2; Error "String 2" ]
    |> Seq.toList
    |> should equal [ 1; 2 ]

[<Fact>]
let ``errors: empty list results in empty list`` () =
    errors [] |> Seq.toList |> should equal []

[<Fact>]
let ``errors: all Ok list results in empty list`` () =
    errors [ Ok 1; Ok 2 ] |> Seq.toList |> should equal []

[<Fact>]
let ``errors: all Error list results in all elements`` () =
    errors [ Error "String 1"; Error "String 2" ]
    |> Seq.toList
    |> should equal [ "String 1"; "String 2" ]

[<Fact>]
let ``errors: mixed list results in only Errors`` () =
    errors [ Ok 1; Error "String 1"; Ok 2; Error "String 2" ]
    |> Seq.toList
    |> should equal [ "String 1"; "String 2" ]

[<Fact>]
let ``mergeFailures: two empty FailureMap results in empty FailureMap`` () =
    let input1 = Map.ofList []
    let input2 = Map.ofList []
    let result = mergeFailures input1 input2
    let expected = Map.ofList []
    result = expected

[<Property>]
let ``mergeFailures: an empty FailureMap and populated FailureMap result in the populated``
    (NonWhiteSpaceString n1, e1: string)
    =
    let name1 = mkName n1 |> Option.get
    let input1 = Map.ofList []
    let input2 = Map.ofList [ ([ name1 ], [ e1 ]) ]
    let result = mergeFailures input1 input2
    result = input2

[<Property>]
let ``mergeFailures: a populated FailureMap and an empty FailureMap result in the populated``
    (NonWhiteSpaceString n1, e1: string)
    =
    let name1 = mkName n1 |> Option.get
    let input1 = Map.ofList [ ([ name1 ], [ e1 ]) ]
    let input2 = Map.ofList []
    let result = mergeFailures input1 input2
    result = input1

[<Property>]
let ``mergeFailures: two single element FailureMaps of different names merge properly`` (e1: string, e2: string) =
    // Need to ensure names are distinct
    let name1 = mkName "Name1" |> Option.get
    let name2 = mkName "Name2" |> Option.get
    let input1 = Map.ofList [ ([ name1 ], [ e1 ]) ]
    let input2 = Map.ofList [ ([ name2 ], [ e2 ]) ]
    let result = mergeFailures input1 input2
    let expected = Map.ofList [ ([ name1 ], [ e1 ]); ([ name2 ], [ e2 ]) ]
    result = expected

[<Property>]
let ``mergeFailures: two single element FailureMaps of the same name merge properly``
    (NonWhiteSpaceString n1, e1: string, e2: string)
    =
    let name1 = mkName n1 |> Option.get
    let input1 = Map.ofList [ ([ name1 ], [ e1 ]) ]
    let input2 = Map.ofList [ ([ name1 ], [ e2 ]) ]
    let result = mergeFailures input1 input2
    let expected = Map.ofList [ ([ name1 ], [ e1; e2 ]) ]
    result = expected

[<Property>]
let ``mergeFailures: two multi element FailureMaps merge properly`` (e1: string, e2: string, e3: string, e4: string) =
    // Need to ensure names are distinct
    let name1 = mkName "Name1" |> Option.get
    let name2 = mkName "Name2" |> Option.get
    let name3 = mkName "Name3" |> Option.get
    let input1 = Map.ofList [ ([ name1 ], [ e1 ]); ([ name2 ], [ e2 ]) ]
    let input2 = Map.ofList [ ([ name2 ], [ e3 ]); ([ name3 ], [ e4 ]) ]
    let result = mergeFailures input1 input2

    let expected =
        Map.ofList [ ([ name1 ], [ e1 ]); ([ name2 ], [ e2; e3 ]); ([ name3 ], [ e4 ]) ]

    result = expected

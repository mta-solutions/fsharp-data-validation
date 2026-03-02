module FSharp.Data.Validation.Tests.VCtx

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open FSharp.Data.Validation

[<Property>]
let ``map: Transforms a ValidCtx``
    (a : int)
    =
    let input = ValidCtx a
    let result = VCtx.map (fun b -> b.ToString()) input
    Assert.Equal(ValidCtx (a.ToString()), result)

[<Property>]
let ``map: Transforms a DisputedCtx while preserving failures``
    (a : int, NonWhiteSpaceString n1, lf1 : int, gf1: int)
    =
    let field1 = mkName n1 |> Option.get
    // Todo: make failures of arbitrary length
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]

    let input = DisputedCtx (gfs, lfs, a)
    let result = VCtx.map (fun b -> b.ToString()) input
    Assert.Equal(DisputedCtx (gfs, lfs, a.ToString()), result)

[<Property>]
let ``map: Makes no changes to a RefutedCtx``
    (NonWhiteSpaceString n1, lf1 : int, gf1: int)
    =
    let field1 = mkName n1 |> Option.get
    // Todo: make failures of arbitrary length
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]

    let input = RefutedCtx (gfs, lfs)
    let result = VCtx.map (fun b -> b.ToString()) input
    Assert.Equal(input, result)

[<Property>]
let ``bind: Transforms a ValidCtx``
    (a : int)
    =
    let input = ValidCtx a
    let result = VCtx.bind (fun b -> ValidCtx(b.ToString())) input
    Assert.Equal(ValidCtx (a.ToString()), result)

[<Property>]
let ``bind: Makes no changes to a RefutedCtx``
    (NonWhiteSpaceString n1, lf1 : int, gf1: int)
    =
    let field1 = mkName n1 |> Option.get
    // Todo: make failures of arbitrary length
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]

    let input = RefutedCtx (gfs, lfs)
    let result = VCtx.bind (fun a -> ValidCtx (a + 1)) input
    Assert.Equal(input, result)

[<Property>]
let ``bind: Bind a DisputedCtx with a ValidCtx properly, results in DisputedCtx with same failures``
    (a : int, NonWhiteSpaceString n1, lf1 : int, gf1: int)
    =
    let field1 = mkName n1 |> Option.get
    // Todo: make failures of arbitrary length
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]

    let input =  DisputedCtx (gfs, lfs, a)
    let result = VCtx.bind (fun a -> ValidCtx (a + 1)) input
    Assert.Equal(DisputedCtx (gfs, lfs, a + 1), result)

[<Property>]
let ``bind: Bind a DisputedCtx with a DisputedCtx properly, results in DisputedCtx with merged failures``
    (a : int, NonWhiteSpaceString n1, lf1 : int, lf2 : int, gf1 : int, gf2 : int)
    =
    let field1 = mkName n1 |> Option.get
    // Todo: make failures of arbitrary length
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]
    let gfs2 = [gf2]
    let lfs2 = Map.ofList [([field1], [lf2])]
    let lfsResult = Utilities.mergeFailures lfs lfs2

    let input =  DisputedCtx (gfs, lfs, a)
    let result = VCtx.bind (fun a -> DisputedCtx (gfs2, lfs2, a + 1)) input
    Assert.Equal(DisputedCtx ([gf1; gf2], lfsResult, a + 1), result)

[<Property>]
let ``bind: Bind a DisputedCtx with a RefutedCtx properly, results in RefutedCtx with merged failures``
    (a : int, NonWhiteSpaceString n1, lf1 : int, lf2 : int, gf1 : int, gf2 : int)
    =
    let field1 = mkName n1 |> Option.get
    // Todo: make failures of arbitrary length
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]
    let gfs2 = [gf2]
    let lfs2 = Map.ofList [([field1], [lf2])]
    let lfsResult = Utilities.mergeFailures lfs lfs2

    let input =  DisputedCtx (gfs, lfs, a)
    let result = VCtx.bind (fun _ -> RefutedCtx (gfs2, lfs2)) input
    Assert.Equal(RefutedCtx ([gf1; gf2], lfsResult), result)

[<Property>]
let ``mergeSources with two ValidCtx should return ValidCtx with tuple``
    (a: int, b: int)
    =
    let input1 = ValidCtx a
    let input2 = ValidCtx b
    let expected = ValidCtx (a, b)
    Assert.Equal(expected, VCtx.mergeSources input1 input2)

[<Property>]
let ``mergeSources: Merging one ValidCtx and one DisputedCtx results in DisputedCtx``
    (a: int, b: int, NonWhiteSpaceString n1, lf1: int, gf1: int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]
    let input1 = ValidCtx a
    let input2 = DisputedCtx (gfs, lfs, b)
    let expected = DisputedCtx (gfs, lfs, (a, b))
    Assert.Equal(expected, VCtx.mergeSources input1 input2)

[<Property>]
let ``mergeSources: Merging one ValidCtx and one RefutedCtx results in RefutedCtx``
    (a: int, NonWhiteSpaceString n1, lf1: int, gf1: int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]
    let input1 = ValidCtx a
    let input2 = RefutedCtx (gfs, lfs)
    let expected = RefutedCtx (gfs, lfs)
    Assert.Equal(expected, VCtx.mergeSources input1 input2)

[<Property>]
let ``mergeSources: Merging two DisputedCtx results in DisputedCtx``
    (a: int, b: int, NonWhiteSpaceString n1, lf1: int, lf2: int, gf1: int, gf2: int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs1 = [gf1]
    let lfs1 = Map.ofList [([field1], [lf1])]
    let gfs2 = [gf2]
    let lfs2 = Map.ofList [([field1], [lf2])]
    let lfsResult = Utilities.mergeFailures lfs1 lfs2
    let input1 = DisputedCtx (gfs1, lfs1, a)
    let input2 = DisputedCtx (gfs2, lfs2, b)
    let expected = DisputedCtx (gfs1 @ gfs2, lfsResult, (a, b))
    Assert.Equal(expected, VCtx.mergeSources input1 input2)

[<Property>]
let ``mergeSources: Merging one DisputedCtx and one RefutedCtx results in RefutedCtx``
    (a: int, NonWhiteSpaceString n1, lf1: int, lf2: int, gf1: int, gf2: int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs1 = [gf1]
    let lfs1 = Map.ofList [([field1], [lf1])]
    let gfs2 = [gf2]
    let lfs2 = Map.ofList [([field1], [lf2])]
    let lfsResult = Utilities.mergeFailures lfs1 lfs2
    let input1 = DisputedCtx (gfs1, lfs1, a)
    let input2 = RefutedCtx (gfs2, lfs2)
    let expected = RefutedCtx (gfs1 @ gfs2, lfsResult)
    Assert.Equal(expected, VCtx.mergeSources input1 input2)

[<Property>]
let ``mergeSources: Merging two RefutedCtx results in RefutedCtx``
    (NonWhiteSpaceString n1, lf1: int, lf2: int, gf1: int, gf2: int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs1 = [gf1]
    let lfs1 = Map.ofList [([field1], [lf1])]
    let gfs2 = [gf2]
    let lfs2 = Map.ofList [([field1], [lf2])]
    let lfsResult = Utilities.mergeFailures lfs1 lfs2
    let input1 = RefutedCtx (gfs1, lfs1)
    let input2 = RefutedCtx (gfs2, lfs2)
    let expected = RefutedCtx (gfs1 @ gfs2, lfsResult)
    Assert.Equal(expected, VCtx.mergeSources input1 input2)

[<Fact>]
let ``VCtxBuilder.Zero: Returns ValidCtx unit`` () =
    VCtxBuilder().Zero() |> should equal (ValidCtx ())

[<Property>]
let ``VCtxBuilder.Bind: Transforms a ValidCtx``
    (a : int)
    =
    let input = ValidCtx a
    VCtxBuilder().Bind(input, fun b -> ValidCtx(b.ToString()))
    |> should equal (ValidCtx (a.ToString()))

[<Property>]
let ``VCtxBuilder.Bind: Makes no changes to a RefutedCtx``
    (NonWhiteSpaceString n1, lf1 : int, gf1: int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]
    let input = RefutedCtx (gfs, lfs)

    // TODO: FsUnit should equal fails to match maps it seems
    Assert.Equal(input, VCtxBuilder().Bind(input, fun a -> ValidCtx (a + 1)))

[<Property>]
let ``VCtxBuilder.Bind: Bind a DisputedCtx with a ValidCtx properly, results in DisputedCtx with same failures``
    (a : int, NonWhiteSpaceString n1, lf1 : int, gf1: int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]

    let input =  DisputedCtx (gfs, lfs, a)
    let result = VCtxBuilder().Bind(input, fun a -> ValidCtx (a + 1))
    Assert.Equal(DisputedCtx (gfs, lfs, a + 1), result)

[<Property>]
let ``VCtxBuilder.Bind: Bind a DisputedCtx with a DisputedCtx properly, results in DisputedCtx with merged failures``
    (a : int, NonWhiteSpaceString n1, lf1 : int, lf2 : int, gf1 : int, gf2 : int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]
    let gfs2 = [gf2]
    let lfs2 = Map.ofList [([field1], [lf2])]
    let lfsResult = Utilities.mergeFailures lfs lfs2

    let input =  DisputedCtx (gfs, lfs, a)
    let result = VCtxBuilder().Bind(input, fun a -> DisputedCtx (gfs2, lfs2, a + 1))
    Assert.Equal(DisputedCtx ([gf1; gf2], lfsResult, a + 1), result)

[<Property>]
let ``VCtxBuilder.Bind: Bind a DisputedCtx with a RefutedCtx properly, results in RefutedCtx with merged failures``
    (a : int, NonWhiteSpaceString n1, lf1 : int, lf2 : int, gf1 : int, gf2 : int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]
    let gfs2 = [gf2]
    let lfs2 = Map.ofList [([field1], [lf2])]
    let lfsResult = Utilities.mergeFailures lfs lfs2

    let input =  DisputedCtx (gfs, lfs, a)
    let result =VCtxBuilder().Bind(input, fun _ -> RefutedCtx (gfs2, lfs2))
    Assert.Equal(RefutedCtx ([gf1; gf2], lfsResult), result)

[<Property>]
let ``VCtxBuilder.MergeSources: Merges two ValidCtx into a tuple``
    (a : int, b : int)
    =
    let input = ValidCtx a, ValidCtx b
    VCtxBuilder().MergeSources(input)
    |> should equal (ValidCtx (a, b))

[<Property>]
let ``VCtxBuilder.MergeSources: Merging one Valid and one DisputedCtx results in DisputedCtx``
    (a : int, b : int, NonWhiteSpaceString n1, lf1 : int, gf1: int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]

    let input1 = ValidCtx a, DisputedCtx (gfs, lfs, b)
    let input2 = DisputedCtx (gfs, lfs, b), ValidCtx a
    let expected1 = DisputedCtx(gfs, lfs, (a, b))
    let expected2 = DisputedCtx(gfs, lfs, (b, a))

    Assert.Equal(expected1, VCtxBuilder().MergeSources(input1))
    Assert.Equal(expected2, VCtxBuilder().MergeSources(input2))

[<Property>]
let ``VCtxBuilder.MergeSources: Merging one Valid and one RefutedCtx results in RefutedCtx``
    (a : int, NonWhiteSpaceString n1, lf1 : int, gf1 : int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]

    let input1 = ValidCtx a, RefutedCtx (gfs, lfs)
    let input2 = RefutedCtx (gfs, lfs), ValidCtx a
    let expected = RefutedCtx(gfs, lfs)

    Assert.Equal(expected, VCtxBuilder().MergeSources(input1))
    Assert.Equal(expected, VCtxBuilder().MergeSources(input2))

[<Property>]
let ``VCtxBuilder.MergeSources: Merging two DisputedCtx results in DisputedCtx``
    (a : int, b : int, NonWhiteSpaceString n1, lf1 : int, lf2 : int, gf1 : int, gf2 : int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs1 = [gf1]
    let lfs1 = Map.ofList [([field1], [lf1])]
    let gfs2 = [gf2]
    let lfs2 = Map.ofList [([field1], [lf2])]

    let input1 = DisputedCtx (gfs1, lfs1, a), DisputedCtx (gfs2, lfs2, b)
    let input2 = DisputedCtx (gfs2, lfs2, b), DisputedCtx (gfs1, lfs1, a)
    let expected1 = DisputedCtx(gfs1 @ gfs2, Map.ofList [([field1], [lf1; lf2])], (a, b))
    let expected2 = DisputedCtx(gfs2 @ gfs1, Map.ofList [([field1], [lf2; lf1])], (b, a))

    Assert.Equal(expected1, VCtxBuilder().MergeSources(input1))
    Assert.Equal(expected2, VCtxBuilder().MergeSources(input2))

[<Property>]
let ``VCtxBuilder.MergeSources: Merging one RefutedCTX and one DisputedCtx results in RefutedCtx``
    (a : int, NonWhiteSpaceString n1, lf1 : int, lf2 : int, gf1 : int, gf2 : int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs1 = [gf1]
    let lfs1 = Map.ofList [([field1], [lf1])]
    let gfs2 = [gf2]
    let lfs2 = Map.ofList [([field1], [lf2])]

    let input1 = DisputedCtx (gfs1, lfs1, a), RefutedCtx (gfs2, lfs2)
    let input2 = RefutedCtx (gfs2, lfs2), DisputedCtx (gfs1, lfs1, a)
    let expected1 = RefutedCtx(gfs1 @ gfs2, Map.ofList [([field1], [lf1; lf2])])
    let expected2 = RefutedCtx(gfs2 @ gfs1, Map.ofList [([field1], [lf2; lf1])])

    Assert.Equal(expected1, VCtxBuilder().MergeSources(input1))
    Assert.Equal(expected2, VCtxBuilder().MergeSources(input2))

[<Property>]
let ``VCtxBuilder.MergeSources: Merging two RefutedCtx results in RefutedCtx``
    (NonWhiteSpaceString n1, lf1 : int, lf2 : int, gf1 : int, gf2 : int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs1 = [gf1]
    let lfs1 = Map.ofList [([field1], [lf1])]
    let gfs2 = [gf2]
    let lfs2 = Map.ofList [([field1], [lf2])]

    let input1 = RefutedCtx (gfs1, lfs1), RefutedCtx (gfs2, lfs2)
    let input2 = RefutedCtx (gfs2, lfs2), RefutedCtx (gfs1, lfs1)
    let expected1 = RefutedCtx(gfs1 @ gfs2, Map.ofList [([field1], [lf1; lf2])])
    let expected2 = RefutedCtx(gfs2 @ gfs1, Map.ofList [([field1], [lf2; lf1])])

    Assert.Equal(expected1, VCtxBuilder().MergeSources(input1))
    Assert.Equal(expected2, VCtxBuilder().MergeSources(input2))

type Five = Five

let mk5 i =
    validation {
        withValue i
        disputeWithFact -5 (isEqual 5)
    }



let mk5r i =
    validation {
        withValue i
        refuteWith (fun a -> if a = 5 then Ok 5 else Error -5)
    }

[<Property>]
let ``VCtxBuilder.Optional: Optional of a RefutedCtx returns RefutedCtx``
    (NonWhiteSpaceString n1, lf1 : int, gf1 : int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs1 = [gf1]
    let lfs1 = Map.ofList [([field1], [lf1])]

    let input = RefutedCtx (gfs1, lfs1)

    Assert.Equal(input, VCtxBuilder().Optional(input, mk5))

[<Property>]
let ``VCtxBuilder.Optional: Optional of a ValidCtx with None returns ValidCtx``
    (NonWhiteSpaceString n1)
    =
    let field1 = mkName n1 |> Option.get
    let input = ValidCtx (Field (field1, None))
    Assert.Equal(input, VCtxBuilder().Optional(input, mk5))

[<Property>]
let ``VCtxBuilder.Optional: Optional of a ValidCtx with Some valid returns ValidCtx``
    (NonWhiteSpaceString n1)
    =
    let field1 = mkName n1 |> Option.get
    let input = ValidCtx (Field (field1, Some 5))
    let expected = ValidCtx (Global (Some 5))
    Assert.Equal(expected, VCtxBuilder().Optional(input, mk5))

[<Property>]
let ``VCtxBuilder.Optional: Optional of a ValidCtx with Some disputed returns DisputedCtx``
    (NonWhiteSpaceString n1)
    =
    let field1 = mkName n1 |> Option.get
    let input = ValidCtx (Field (field1, Some 1))
    let expected = DisputedCtx ([], Map.ofList [([field1], [-5])], Global (Some 1))
    Assert.Equal(expected, VCtxBuilder().Optional(input, mk5))

[<Property>]
let ``VCtxBuilder.Optional: Optional of a ValidCtx with Some refuted returns RefutedCtx``
    (NonWhiteSpaceString n1)
    =
    let field1 = mkName n1 |> Option.get
    let input = ValidCtx (Field (field1, Some 1))
    let expected = RefutedCtx ([], Map.ofList [([field1], [-5])])
    Assert.Equal(expected, VCtxBuilder().Optional(input, mk5r))

[<Property>]
let ``VCtxBuilder.Optional: Optional of a DisputedCtx with None returns DisputedCtx``
    (lf1 : int, gf1 : int)
    =
    let field1 = mkName "Field1" |> Option.get
    let field2 = mkName "Field2" |> Option.get
    let input = DisputedCtx ([gf1], Map.ofList [([field1], [lf1])], Field (field2, None))
    Assert.Equal(input, VCtxBuilder().Optional(input, mk5))

[<Property>]
let ``VCtxBuilder.Optional: Optional of a DisputedCtx with Some valid returns DisputedCtx``
    (lf1 : int, gf1 : int)
    =
    let field1 = mkName "Field1" |> Option.get
    let field2 = mkName "Field2" |> Option.get
    let input = DisputedCtx ([gf1], Map.ofList [([field1], [lf1])], Field (field2, Some 5))
    let expected = DisputedCtx ([gf1], Map.ofList [([field1], [lf1])], Global (Some 5))
    Assert.Equal(expected, VCtxBuilder().Optional(input, mk5))

[<Property>]
let ``VCtxBuilder.Optional: Optional of a DisputedCtx with Some disputed returns DisputedCtx``
    (lf1 : int, gf1 : int)
    =
    let field1 = mkName "Field1" |> Option.get
    let field2 = mkName "Field2" |> Option.get
    let input = DisputedCtx ([gf1], Map.ofList [([field1], [lf1])], Field (field2, Some 1))
    let expected = DisputedCtx ([gf1], Map.ofList [([field1], [lf1]); ([field2], [-5])], Global (Some 1))
    Assert.Equal(expected, VCtxBuilder().Optional(input, mk5))

[<Property>]
let ``VCtxBuilder.Optional: Optional of a DisputedCtx with Some refuted returns RefutedCtx``
    (lf1 : int, gf1 : int)
    =
    let field1 = mkName "Field1" |> Option.get
    let field2 = mkName "Field2" |> Option.get
    let input = DisputedCtx ([gf1], Map.ofList [([field1], [lf1])], Field (field2, Some 1))
    let expected = RefutedCtx ([gf1], Map.ofList [([field1], [lf1]); ([field2], [-5])])
    Assert.Equal(expected, VCtxBuilder().Optional(input, mk5r))

[<Property>]
let ``VCtxBuilder.DisputeWith: When validation fails, the valid context becomes a disputed context`` (NegativeInt i) =
    let a = Global i
    let ctx = ValidCtx a
    let failure = "failure"
    let func x = if x > 0 then None else Some failure
    let result = VCtxBuilder().DisputeWith(ctx, func)
    let expected = DisputedCtx([failure], Map.ofList [], a)
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.DisputeWith: When validation fails, the failure is added to the disputed context`` (NegativeInt i) =
    let a = Global i
    let failure1 = "failure1"
    let ctx = DisputedCtx([failure1], Map.ofList [], a)
    let failure2 = "failure2"
    let func x = if x > 0 then None else Some failure2
    let result = VCtxBuilder().DisputeWith(ctx, func)
    let expected = DisputedCtx([failure1; failure2], Map.ofList [], a)
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.DisputeWith: When validation succeeds, the valid context remains the same`` (PositiveInt i) =
    let a = Global i
    let ctx = ValidCtx a
    let failure = "failure"
    let func x = if x > 0 then None else Some failure
    let result = VCtxBuilder().DisputeWith(ctx, func)
    let expected = ctx
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.DisputeWith: When validation succeeds, the disputed context remains the same`` (PositiveInt i) =
    let a = Global i
    let failure1 = "failure1"
    let ctx = DisputedCtx([failure1], Map.ofList [], a)
    let failure2 = "failure2"
    let func x = if x > 0 then None else Some failure2
    let result = VCtxBuilder().DisputeWith(ctx, func)
    let expected = ctx
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.DisputeWithFact: When validation fails, the valid context becomes a disputed context`` (NegativeInt i) =
    let a = Global i
    let ctx = ValidCtx a
    let func x = x > 0
    let failure = "failure"
    let result = VCtxBuilder().DisputeWithFact(ctx, failure, func)
    let expected = DisputedCtx([failure], Map.ofList [], a)
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.DisputeWithFact: When validation fails, the failure is added to the disputed context`` (NegativeInt i) =
    let a = Global i
    let failure1 = "failure1"
    let ctx = DisputedCtx([failure1], Map.ofList [], a)
    let func x = x > 0
    let failure2 = "failure2"
    let result = VCtxBuilder().DisputeWithFact(ctx, failure2, func)
    let expected = DisputedCtx([failure1; failure2], Map.ofList [], a)
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.DisputeWithFact: When validation succeeds, the valid context remains the same`` (PositiveInt i) =
    let a = Global i
    let ctx = ValidCtx a
    let func x = x > 0
    let failure = "failure"
    let result = VCtxBuilder().DisputeWithFact(ctx, failure, func)
    let expected = ctx
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.DisputeWithFact: When validation succeeds, the disputed context remains the same`` (PositiveInt i) =
    let a = Global i
    let failure1 = "failure1"
    let ctx = DisputedCtx([failure1], Map.ofList [], a)
    let func x = x > 0
    let failure2 = "failure2"
    let result = VCtxBuilder().DisputeWithFact(ctx, failure2, func)
    let expected = ctx
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWith: When validation fails, the valid context becomes a refuted context`` (NegativeInt i) =
    let a = Global i
    let ctx = ValidCtx a
    let success = "success"
    let failure = "failure"
    let func x = if x > 0 then Ok success else Error failure
    let result = VCtxBuilder().RefuteWith(ctx, func)
    let expected = RefutedCtx([failure], Map.ofList [])
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWith: When validation fails, the disputed context becomes a refuted context`` (NegativeInt i) =
    let a = Global i
    let failure1 = "failure1"
    let ctx = DisputedCtx([failure1], Map.ofList [], a)
    let success = "success"
    let failure2 = "failure2"
    let func x = if x > 0 then Ok success else Error failure2
    let result = VCtxBuilder().RefuteWith(ctx, func)
    let expected = RefutedCtx([failure1; failure2], Map.ofList [])
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWith: When validation succeeds, the valid context remains the same`` (PositiveInt i) =
    let a = Global i
    let ctx = ValidCtx a
    let success = "success"
    let failure = "failure"
    let func x = if x > 0 then Ok success else Error failure
    let result = VCtxBuilder().RefuteWith(ctx, func)
    let b = Global success
    let expected = ValidCtx b
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWith: When validation succeeds, the disputed context remains the same`` (PositiveInt i) =
    let a = Global i
    let failure1 = "failure1"
    let ctx = DisputedCtx([failure1], Map.ofList [], a)
    let success = "success"
    let failure2 = "failure2"
    let func x = if x > 0 then Ok success else Error failure2
    let result = VCtxBuilder().RefuteWith(ctx, func)
    let b = Global success
    let expected = DisputedCtx([failure1], Map.ofList [], b)
    Assert.Equal(expected, result)

// Tests for refute operator
[<Property>]
let ``VCtxBuilder.Refute: Refutes a ValidCtx`` (a: int, f: int) =
    let input = ValidCtx(Global a)
    let result = VCtxBuilder().Refute(input, f)
    Assert.Equal(RefutedCtx([ f ], Map.empty), result)

[<Property>]
let ``VCtxBuilder.Refute: Refutes a DisputedCtx`` (a: int, f1: int, f2: int) =
    let input = DisputedCtx([ f1 ], Map.empty, Global a)
    let result = VCtxBuilder().Refute(input, f2)
    Assert.Equal(RefutedCtx([ f1; f2 ], Map.empty), result)

// Tests for dispute operator
[<Property>]
let ``VCtxBuilder.Dispute: Adds failure to ValidCtx`` (a: int, f: int) =
    let input = ValidCtx(Global a)
    let result = VCtxBuilder().Dispute(input, f)
    Assert.Equal(DisputedCtx([ f ], Map.empty, Global a), result)

[<Property>]
let ``VCtxBuilder.Dispute: Adds failure to DisputedCtx`` (a: int, f1: int, f2: int) =
    let input = DisputedCtx([ f1 ], Map.empty, Global a)
    let result = VCtxBuilder().Dispute(input, f2)
    Assert.Equal(DisputedCtx([ f1; f2 ], Map.empty, Global a), result)

// Tests for refuteEachWith operator
[<Property>]
let ``VCtxBuilder.RefuteEachWith: Refutes on first failure`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])

    let fn i a =
        if a < 2 then Error "too small" else Ok(a * 2)

    let result = VCtxBuilder().RefuteEachWith(input, fn)

    match result with
    | RefutedCtx(_, lfs) -> Assert.True(Map.containsKey [ mkName "[0]" |> Option.get ] lfs)
    | _ -> failwith "Expected RefutedCtx"

[<Property>]
let ``VCtxBuilder.RefuteEachWith: Succeeds when all elements pass`` () =
    let input = ValidCtx(Global [ 2; 3; 4 ])

    let fn i a =
        if a < 2 then Error "too small" else Ok(a * 2)

    let result = VCtxBuilder().RefuteEachWith(input, fn)

    match result with
    | ValidCtx(Global xs) -> Assert.Equal([ 4; 6; 8 ], xs)
    | _ -> failwith "Expected ValidCtx with transformed values"

// Tests for refuteEachWithProof operator
[<Property>]
let ``VCtxBuilder.RefuteEachWithProof: Refutes on first Invalid`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])

    let fn a =
        if a < 2 then
            Invalid([ "too small" ], Map.empty)
        else
            Valid(a * 2)

    let result = VCtxBuilder().RefuteEachWithProof(input, fn)

    match result with
    | RefutedCtx(_, lfs) -> Assert.True(Map.containsKey [ mkName "[0]" |> Option.get ] lfs)
    | _ -> failwith "Expected RefutedCtx"

[<Property>]
let ``VCtxBuilder.RefuteEachWithProof: Succeeds when all elements Valid`` () =
    let input = ValidCtx(Global [ 2; 3; 4 ])

    let fn a =
        if a < 2 then
            Invalid([ "too small" ], Map.empty)
        else
            Valid(a * 2)

    let result = VCtxBuilder().RefuteEachWithProof(input, fn)

    match result with
    | ValidCtx(Global xs) -> Assert.Equal([ 4; 6; 8 ], xs)
    | _ -> failwith "Expected ValidCtx with transformed values"

// Tests for validateEach operator
[<Property>]
let ``VCtxBuilder.ValidateEach: Validates each element`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])

    let fn i a =
        if a < 2 then
            DisputedCtx([ "too small" ], Map.empty, Global a)
        else
            ValidCtx(Global(a * 2))

    let result = VCtxBuilder().ValidateEach(input, fn)

    match result with
    | DisputedCtx(_, lfs, Global xs) ->
        Assert.True(Map.containsKey [ mkName "[0]" |> Option.get ] lfs)
        Assert.Equal<int list>([ 1; 4; 6 ], Seq.toList xs) // Element 0 keeps original value 1
    | _ -> failwith "Expected DisputedCtx with partial values"

[<Property>]
let ``VCtxBuilder.ValidateEach: Accumulates all failures`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])

    let fn i a =
        if a < 3 then
            DisputedCtx([ "too small" ], Map.empty, Global a)
        else
            ValidCtx(Global(a * 2))

    let result = VCtxBuilder().ValidateEach(input, fn)

    match result with
    | DisputedCtx(_, lfs, Global xs) ->
        Assert.Equal(2, Map.count lfs) // Two elements failed
        Assert.Equal<int list>([ 1; 2; 6 ], Seq.toList xs) // Elements 0,1 keep originals, element 2 transformed
    | _ -> failwith "Expected DisputedCtx with multiple failures"

// Tests for disputeAnyWith operator
[<Property>]
let ``VCtxBuilder.DisputeAnyWith: Disputes if any element fails`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])
    let fn i a = if a = 2 then Some "found 2" else None
    let result = VCtxBuilder().DisputeAnyWith(input, fn)

    match result with
    | DisputedCtx(_, lfs, Global xs) ->
        Assert.True(Map.containsKey [ mkName "[1]" |> Option.get ] lfs) // Failure on element at index 1
        Assert.Equal<int list>([ 1; 2; 3 ], Seq.toList xs) // All elements preserved
    | _ -> failwith "Expected DisputedCtx"

[<Property>]
let ``VCtxBuilder.DisputeAnyWith: Succeeds if no element fails`` () =
    let input = ValidCtx(Global [ 1; 3; 5 ])
    let fn i a = if a = 2 then Some "found 2" else None
    let result = VCtxBuilder().DisputeAnyWith(input, fn)

    match result with
    | ValidCtx(Global xs) -> Assert.Equal<int list>([ 1; 3; 5 ], Seq.toList xs)
    | _ -> failwith "Expected ValidCtx"

// Tests for disputeAnyWithMany operator
[<Property>]
let ``VCtxBuilder.DisputeAnyWithMany: Disputes if any element fails`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])
    let fn i a = if a = 2 then [ "found 2" ] else []
    let result = VCtxBuilder().DisputeAnyWithMany(input, fn)

    match result with
    | DisputedCtx(_, lfs, Global xs) ->
        Assert.True(Map.containsKey [ mkName "[1]" |> Option.get ] lfs) // Failure on element at index 1
        Assert.Equal<int list>([ 1; 2; 3 ], Seq.toList xs) // All elements preserved
    | _ -> failwith "Expected DisputedCtx"

[<Property>]
let ``VCtxBuilder.DisputeAnyWithMany: Succeeds if no element fails`` () =
    let input = ValidCtx(Global [ 1; 3; 5 ])
    let fn i a = if a = 2 then [ "found 2" ] else []
    let result = VCtxBuilder().DisputeAnyWithMany(input, fn)

    match result with
    | ValidCtx(Global xs) -> Assert.Equal<int list>([ 1; 3; 5 ], Seq.toList xs)
    | _ -> failwith "Expected ValidCtx"

// Tests for disputeAnyWithFact operator
[<Property>]
let ``VCtxBuilder.DisputeAnyWithFact: Disputes if any element fails check`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])
    let fn i a = a <> 2 // Returns False (fails check) when element equals 2
    let result = VCtxBuilder().DisputeAnyWithFact(input, "found 2", fn)

    match result with
    | DisputedCtx(_, lfs, Global xs) ->
        Assert.True(Map.containsKey [ mkName "[1]" |> Option.get ] lfs) // Failure on element at index 1
        Assert.Equal<int list>([ 1; 2; 3 ], Seq.toList xs) // All elements preserved
    | _ -> failwith "Expected DisputedCtx"

[<Property>]
let ``VCtxBuilder.DisputeAnyWithFact: Succeeds if all elements pass check`` () =
    let input = ValidCtx(Global [ 1; 3; 5 ])
    let fn i a = a <> 2 // Returns True (passes) for all elements (none equal 2)
    let result = VCtxBuilder().DisputeAnyWithFact(input, "found 2", fn)

    match result with
    | ValidCtx(Global xs) -> Assert.Equal([ 1; 3; 5 ], xs)
    | _ -> failwith "Expected ValidCtx"

// Tests for disputeAllWith operator
[<Property>]
let ``VCtxBuilder.DisputeAllWith: Disputes if all elements fail`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])
    let fn i a = if a > 0 then Some "all fail" else None // Returns Some for all elements > 0 (all pass)
    let result = VCtxBuilder().DisputeAllWith(input, fn)

    match result with
    | DisputedCtx(gfs, _, _) -> Assert.Equal<string list>([ "all fail" ], gfs)
    | _ -> failwith "Expected DisputedCtx"

[<Property>]
let ``VCtxBuilder.DisputeAllWith: Succeeds if not all elements fail`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])

    let fn i a =
        if a < 2 then Some "too small" else None

    let result = VCtxBuilder().DisputeAllWith(input, fn)

    match result with
    | ValidCtx(Global xs) -> Assert.Equal<int list>([ 1; 2; 3 ], xs)
    | _ -> failwith "Expected ValidCtx"

// Tests for disputeAllWithMany operator (includes bug fix verification)
[<Property>]
let ``VCtxBuilder.DisputeAllWithMany: Disputes if all elements fail`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])
    let fn i a = if a > 0 then [ "all fail" ] else [] // Returns list for all elements > 0 (all pass)
    let result = VCtxBuilder().DisputeAllWithMany(input, fn)

    match result with
    | DisputedCtx(gfs, _, _) -> Assert.Equal<string list>([ "all fail" ], gfs)
    | _ -> failwith "Expected DisputedCtx"

[<Property>]
let ``VCtxBuilder.DisputeAllWithMany: Succeeds if not all elements fail`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])
    let fn i a = if a < 2 then [ "too small" ] else []
    let result = VCtxBuilder().DisputeAllWithMany(input, fn)

    match result with
    | ValidCtx(Global xs) -> Assert.Equal<int list>([ 1; 2; 3 ], xs)
    | _ -> failwith "Expected ValidCtx when not all elements fail"

[<Property>]
let ``VCtxBuilder.DisputeAllWithMany: Produces valid DisputedCtx with failures`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])
    let fn i a = if a > 0 then [ "fail" ] else [] // All elements > 0, so all fail
    let result = VCtxBuilder().DisputeAllWithMany(input, fn)

    match result with
    | DisputedCtx(gfs, lfs, _) ->
        Assert.False(List.isEmpty gfs) // Should have global failure since ALL elements failed
        Assert.True(Map.toList lfs |> List.isEmpty) // No field-level failures when all fail
    | _ -> failwith "Expected valid DisputedCtx state"

[<Property>]
let ``VCtxBuilder.DisputeAllWithFact: Disputes if all elements fail check`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])
    let fn i a = a < 1 // Returns False (fails check) for all elements (all >= 1)
    let result = VCtxBuilder().DisputeAllWithFact(input, "all fail", fn)

    match result with
    | DisputedCtx(gfs, _, _) -> Assert.Equal<string list>([ "all fail" ], gfs)
    | _ -> failwith "Expected DisputedCtx"

[<Property>]
let ``VCtxBuilder.DisputeAllWithFact: Succeeds if not all elements fail check`` () =
    let input = ValidCtx(Global [ 1; 2; 3 ])
    let fn i a = a < 2
    let result = VCtxBuilder().DisputeAllWithFact(input, "too small", fn)

    match result with
    | ValidCtx(Global xs) -> Assert.Equal<int list>([ 1; 2; 3 ], xs)
    | _ -> failwith "Expected ValidCtx"

[<Property>]
let ``VCtxBuilder.RefuteWithProof: When element validation fails, the valid context becomes a refuted context``
    (NegativeInt i)
    =
    let field1 = mkName "field1" |> Option.get
    let a = Element(1, i)
    let ctx = ValidCtx a
    let success = "success"
    let failure = "failure"
    let func x = if x > 0 then Valid success else Invalid([failure], Map.ofList [([field1],[failure])])
    let result = VCtxBuilder().RefuteWithProof(ctx, func)
    let name = mkName "[1]" |> Option.get
    let expected = RefutedCtx([], Map.ofList [([field1],[failure]); ([name],[failure])])
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWithProof: When element validation fails, the disputed context becomes a refuted context`` (NegativeInt i) =
    let field1 = mkName "field1" |> Option.get
    let field2 = mkName "field2" |> Option.get
    let a = Element(1, i)
    let failure = "failure"
    let ctx = DisputedCtx([], Map.ofList [([field1],[failure])], a)
    let success = "success"
    let func x = if x > 0 then Valid success else Invalid([failure], Map.ofList [([field2],[failure])])
    let result = VCtxBuilder().RefuteWithProof(ctx, func)
    let name = mkName "[1]" |> Option.get
    let expected = RefutedCtx([], Map.ofList [([field1],[failure]); ([field2],[failure]); ([name],[failure])])
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWithProof: When element validation succeeds, the valid context remains the same`` (PositiveInt i) =
    let field1 = mkName "field1" |> Option.get
    let a = Element(1, i)
    let ctx = ValidCtx a
    let success = "success"
    let failure = "failure"
    let func x = if x > 0 then Valid success else Invalid([failure], Map.ofList [([field1],[failure])])
    let result = VCtxBuilder().RefuteWithProof(ctx, func)
    let b = Element(1, success)
    let expected = ValidCtx b
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWithProof: When element validation succeeds, the disputed context remains the same`` (PositiveInt i) =
    let field1 = mkName "field1" |> Option.get
    let field2 = mkName "field2" |> Option.get
    let a = Element(1, i)
    let failure = "failure"
    let ctx = DisputedCtx([], Map.ofList [([field1],[failure])], a)
    let success = "success"
    let func x = if x > 0 then Valid success else Invalid([failure], Map.ofList [([field2],[failure])])
    let result = VCtxBuilder().RefuteWithProof(ctx, func)
    let b = Element(1, success)
    let expected = DisputedCtx([], Map.ofList [([field1],[failure])], b)
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWithProof: When field validation fails, the valid context becomes a refuted context`` (NegativeInt i) =
    let field1 = mkName "field1" |> Option.get
    let field2 = mkName "field2" |> Option.get
    let a = Field(field1, i)
    let ctx = ValidCtx a
    let success = "success"
    let failure = "failure"
    let func x = if x > 0 then Valid success else Invalid([failure], Map.ofList [([field2],[failure])])
    let result = VCtxBuilder().RefuteWithProof(ctx, func)
    let expected = RefutedCtx([], Map.ofList [([field1],[failure]); ([field2],[failure])])
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWithProof: When field validation fails, the disputed context becomes a refuted context`` (NegativeInt i) =
    let field1 = mkName "field1" |> Option.get
    let field2 = mkName "field2" |> Option.get
    let field3 = mkName "field3" |> Option.get
    let a = Field(field1, i)
    let failure = "failure"
    let ctx = DisputedCtx([], Map.ofList [([field2],[failure])], a)
    let success = "success"
    let func x = if x > 0 then Valid success else Invalid([failure], Map.ofList [([field3],[failure])])
    let result = VCtxBuilder().RefuteWithProof(ctx, func)
    let expected = RefutedCtx([], Map.ofList [([field1],[failure]); ([field2],[failure]); ([field3],[failure])])
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWithProof: When field validation succeeds, the valid context remains the same`` (PositiveInt i) =
    let field1 = mkName "field1" |> Option.get
    let field2 = mkName "field2" |> Option.get
    let a = Field(field1, i)
    let ctx = ValidCtx a
    let success = "success"
    let failure = "failure"
    let func x = if x > 0 then Valid success else Invalid([failure], Map.ofList [([field2],[failure])])
    let result = VCtxBuilder().RefuteWithProof(ctx, func)
    let b = Field(field1, success)
    let expected = ValidCtx b
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWithProof: When field validation succeeds, the disputed context remains the same`` (PositiveInt i) =
    let field1 = mkName "field1" |> Option.get
    let field2 = mkName "field2" |> Option.get
    let field3 = mkName "field3" |> Option.get
    let a = Field(field1, i)
    let failure = "failure"
    let ctx = DisputedCtx([], Map.ofList [([field2],[failure])], a)
    let success = "success"
    let func x = if x > 0 then Valid success else Invalid([failure], Map.ofList [([field3],[failure])])
    let result = VCtxBuilder().RefuteWithProof(ctx, func)
    let b = Field(field1, success)
    let expected = DisputedCtx([], Map.ofList [([field2],[failure])], b)
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWithProof: When gobal validation fails, the valid context becomes a refuted context`` (NegativeInt i) =
    let a = Global i
    let ctx = ValidCtx a
    let success = "success"
    let failure = "failure"
    let func x = if x > 0 then Valid success else Invalid([failure], Map.ofList [])
    let result = VCtxBuilder().RefuteWithProof(ctx, func)
    let expected = RefutedCtx([failure], Map.ofList [])
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWithProof: When global validation fails, the disputed context becomes a refuted context`` (NegativeInt i) =
    let a = Global i
    let failure1 = "failure1"
    let ctx = DisputedCtx([failure1], Map.ofList [], a)
    let success = "success"
    let failure2 = "failure2"
    let func x = if x > 0 then Valid success else Invalid([failure2], Map.ofList [])
    let result = VCtxBuilder().RefuteWithProof(ctx, func)
    let expected = RefutedCtx([failure1; failure2], Map.ofList [])
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWithProof: When global validation succeeds, the valid context remains the same`` (PositiveInt i) =
    let a = Global i
    let ctx = ValidCtx a
    let success = "success"
    let failure = "failure"
    let func x = if x > 0 then Valid success else Invalid([failure], Map.ofList [])
    let result = VCtxBuilder().RefuteWithProof(ctx, func)
    let b = Global success
    let expected = ValidCtx b
    Assert.Equal(expected, result)

[<Property>]
let ``VCtxBuilder.RefuteWithProof: When global validation succeeds, the disputed context remains the same`` (PositiveInt i) =
    let a = Global i
    let failure1 = "failure1"
    let ctx = DisputedCtx([failure1], Map.ofList [], a)
    let success = "success"
    let failure2 = "failure2"
    let func x = if x > 0 then Valid success else Invalid([failure2], Map.ofList [])
    let result = VCtxBuilder().RefuteWithProof(ctx, func)
    let b = Global success
    let expected = DisputedCtx([failure1], Map.ofList [], b)
    Assert.Equal(expected, result)

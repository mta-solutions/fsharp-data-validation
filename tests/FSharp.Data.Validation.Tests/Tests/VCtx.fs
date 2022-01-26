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
let ``VCtxBuilder.MergeSources: Merging one Valid and one DisputedCtx results in RefutedCtx``
    (a : int, b : int, NonWhiteSpaceString n1, lf1 : int, gf1: int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs = [gf1]
    let lfs = Map.ofList [([field1], [lf1])]

    let input1 = ValidCtx a, DisputedCtx (gfs, lfs, b)
    let input2 = DisputedCtx (gfs, lfs, b), ValidCtx a
    let expected = RefutedCtx(gfs, lfs)

    Assert.Equal(expected, VCtxBuilder().MergeSources(input1))
    Assert.Equal(expected, VCtxBuilder().MergeSources(input2))

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
let ``VCtxBuilder.MergeSources: Merging two DisputedCtx results in RefutedCtx``
    (a : int, b : int, NonWhiteSpaceString n1, lf1 : int, lf2 : int, gf1 : int, gf2 : int)
    =
    let field1 = mkName n1 |> Option.get
    let gfs1 = [gf1]
    let lfs1 = Map.ofList [([field1], [lf1])]
    let gfs2 = [gf2]
    let lfs2 = Map.ofList [([field1], [lf2])]

    let input1 = DisputedCtx (gfs1, lfs1, a), DisputedCtx (gfs2, lfs2, b)
    let input2 = DisputedCtx (gfs2, lfs2, b), DisputedCtx (gfs1, lfs1, a)
    let expected1 = RefutedCtx(gfs1 @ gfs2, Map.ofList [([field1], [lf1; lf2])])
    let expected2 = RefutedCtx(gfs2 @ gfs1, Map.ofList [([field1], [lf2; lf1])])

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
    (NonWhiteSpaceString n1, a: int)
    =
    let field1 = mkName n1 |> Option.get
    let input = ValidCtx (Field (field1, None))
    Assert.Equal(input, VCtxBuilder().Optional(input, mk5))

[<Property>]
let ``VCtxBuilder.Optional: Optional of a ValidCtx with Some valid returns ValidCtx``
    (NonWhiteSpaceString n1, a: int)
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
    (NonWhiteSpaceString n1, NonWhiteSpaceString n2, lf1 : int, gf1 : int)
    =
    let field1 = mkName n1 |> Option.get
    let field2 = mkName n2 |> Option.get
    let input = DisputedCtx ([gf1], Map.ofList [([field1], [gf1])], Field (field2, None))
    Assert.Equal(input, VCtxBuilder().Optional(input, mk5))

[<Property>]
let ``VCtxBuilder.Optional: Optional of a DisputedCtx with Some valid returns DisputedCtx``
    (NonWhiteSpaceString n1, NonWhiteSpaceString n2, lf1 : int, gf1 : int)
    =
    let field1 = mkName n1 |> Option.get
    let field2 = mkName n2 |> Option.get
    let input = DisputedCtx ([gf1], Map.ofList [([field1], [gf1])], Field (field2, Some 5))
    let expected = DisputedCtx ([gf1], Map.ofList [([field1], [gf1])], Global (Some 5))
    Assert.Equal(expected, VCtxBuilder().Optional(input, mk5))

[<Property>]
let ``VCtxBuilder.Optional: Optional of a DisputedCtx with Some disputed returns DisputedCtx``
    (NonWhiteSpaceString n1, NonWhiteSpaceString n2, lf1 : int, gf1 : int)
    =
    let field1 = mkName n1 |> Option.get
    let field2 = mkName n2 |> Option.get
    let input = DisputedCtx ([gf1], Map.ofList [([field1], [gf1])], Field (field2, Some 1))
    let expected = DisputedCtx ([gf1], Map.ofList [([field1], [gf1]); ([field2], [-5])], Global (Some 1))
    Assert.Equal(expected, VCtxBuilder().Optional(input, mk5))

[<Property>]
let ``VCtxBuilder.Optional: Optional of a DisputedCtx with Some refuted returns RefutedCtx``
    (NonWhiteSpaceString n1, NonWhiteSpaceString n2, lf1 : int, gf1 : int)
    =
    let field1 = mkName n1 |> Option.get
    let field2 = mkName n2 |> Option.get
    let input = DisputedCtx ([gf1], Map.ofList [([field1], [gf1])], Field (field2, Some 1))
    let expected = RefutedCtx ([gf1], Map.ofList [([field1], [gf1]); ([field2], [-5])])
    Assert.Equal(expected, VCtxBuilder().Optional(input, mk5r))

module FSharp.Data.Validation.Tests.VCtx

open Xunit
open FsCheck
open FsCheck.Xunit

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
let ``bind: Transforms a global context``
    (a : int)
    =
    let input = ValidCtx a
    let result = VCtx.map (fun b -> b.ToString()) input
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

// TODO: VCtxBuilder tests

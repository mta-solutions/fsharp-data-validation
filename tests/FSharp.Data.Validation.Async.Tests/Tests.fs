module FSharp.Data.Validation.Async.Tests

open FsCheck.Xunit
open FsUnit.Xunit

open FSharp.Data.Validation

[<Property>]
let ``bindAsync with ValidCtx should bind and return ValidCtx``
    (a: int) =
    let result = async { return ValidCtx a }
    let asyncFunc x = async { return ValidCtx (x * 2) }
    let expected = ValidCtx (a * 2)
    let actual = VCtx.bindAsync asyncFunc result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindAsync with RefutedCtx should return RefutedCtx``
    (f1: int) =
    let result = async { return RefutedCtx ([f1], Map.empty) }
    let asyncFunc x = async { return ValidCtx (x * 2) }
    let expected: VCtx<int,int> = RefutedCtx ([f1], Map.empty)
    let actual = VCtx.bindAsync asyncFunc result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindAsync with DisputedCtx should bind and return DisputedCtx``
    (a: int, f1: int) =
    let result = async { return DisputedCtx ([f1], Map.empty, a)}
    let asyncFunc x = async { return ValidCtx (x * 2) }
    let expected = DisputedCtx ([f1], Map.empty, a * 2)
    let actual = VCtx.bindAsync asyncFunc result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``mergeSourcesAsync with two ValidCtx should return ValidCtx with tuple``
    (a: int, b: int) =
    let result1 = async { return ValidCtx a }
    let result2 = async { return ValidCtx b }
    let expected = ValidCtx (a, b)
    let actual = VCtx.mergeSourcesAsync result1 result2 |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``mergeSourcesAsync with ValidCtx and RefutedCtx should return RefutedCtx``
    (a: int, f1: int) =
    let result1 = async { return ValidCtx a }
    let result2 = async { return RefutedCtx ([f1], Map.empty) }
    let expected: VCtx<int,int * obj> = RefutedCtx ([f1], Map.empty)
    let actual = VCtx.mergeSourcesAsync result1 result2 |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindAndMergeSourcesAsync with ValidCtx should bind and merge sources``
    (a: int) =
    let result = async { return ValidCtx a }
    let asyncFunc x = async { return ValidCtx (x * 2) }
    let expected: VCtx<obj,int * int> = ValidCtx (a, a * 2)
    let actual = VCtx.bindAndMergeSourcesAsync asyncFunc result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindToAndMergeSourcesAsync with ValidCtx should bind and merge sources``
    (a: int) =
    let result = ValidCtx a
    let func x = async { return ValidCtx (x * 2) }
    let expected: VCtx<obj,int * int> = ValidCtx (a, a * 2)
    let actual = VCtx.bindToAndMergeSourcesAsync func result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindFromAndMergeSourcesAsync with ValidCtx should bind and merge sources``
    (a: int) =
    let result = async { return ValidCtx a }
    let func x = ValidCtx (x * 2)
    let expected: VCtx<obj,int * int> = ValidCtx (a, a * 2)
    let actual = VCtx.bindFromAndMergeSourcesAsync func result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``mapAsync with ValidCtx should map and return ValidCtx``
    (a: int) =
    let result = async { return ValidCtx a }
    let asyncFunc x = async { return x * 2 }
    let expected = ValidCtx (a * 2)
    let actual = VCtx.mapAsync asyncFunc result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``mapAsync with RefutedCtx should return RefutedCtx``
    (f1: int) =
    let result = async { return RefutedCtx ([f1], Map.empty) }
    let asyncFunc x = async { return x * 2 }
    let expected: VCtx<int,int> = RefutedCtx ([f1], Map.empty)
    let actual = VCtx.mapAsync asyncFunc result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``mapAsync with DisputedCtx should map and return DisputedCtx``
    (a: int, f1: int) =
    let result = async { return DisputedCtx ([f1], Map.empty, a)}
    let asyncFunc x = async { return x * 2 }
    let expected: VCtx<int,int> = DisputedCtx ([f1], Map.empty, a * 2)
    let actual = VCtx.mapAsync asyncFunc result |> Async.RunSynchronously
    actual |> should equal expected

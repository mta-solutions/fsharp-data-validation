module FSharp.Data.Validation.Async.Tests.Default

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
[<Property>]
let ``bindToAsync with ValidCtx should bind value to async computation and return ValidCtx``
    (a: int) =
    let asyncFunc x = async { return ValidCtx (x * 2) }
    let input = ValidCtx a
    let expected = ValidCtx (a * 2)
    let actual = VCtx.bindToAsync asyncFunc input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindToAsync with RefutedCtx should return RefutedCtx unchanged``
    (f1: int) =
    let asyncFunc x = async { return ValidCtx (x * 2) }
    let input: VCtx<int,int> = RefutedCtx ([f1], Map.empty)
    let expected = input
    let actual = VCtx.bindToAsync asyncFunc input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindToAsync with DisputedCtx should bind value to async computation and preserve disputes``
    (a: int, f1: int) =
    let asyncFunc x = async { return ValidCtx (x * 2) }
    let input = DisputedCtx ([f1], Map.empty, a)
    let expected = DisputedCtx ([f1], Map.empty, a * 2)
    let actual = VCtx.bindToAsync asyncFunc input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindToAsync handles async computation returning DisputedCtx``
    (a: int, f1: int, f2: int) =
    let asyncFunc x = async { return DisputedCtx ([f2], Map.empty, x * 2) }
    let input = ValidCtx a
    let expected = DisputedCtx ([f2], Map.empty, a * 2)
    let actual = VCtx.bindToAsync asyncFunc input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindToAsync handles async computation returning RefutedCtx``
    (a: int, f1: int) =
    let asyncFunc x = async { return RefutedCtx ([f1], Map.empty) }
    let input = ValidCtx a
    let expected = RefutedCtx ([f1], Map.empty)
    let actual = VCtx.bindToAsync asyncFunc input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindFromAsync with ValidCtx should bind async result to value``
    (a: int) =
    let func x = ValidCtx (x * 2)
    let input = async { return ValidCtx a }
    let expected = ValidCtx (a * 2)
    let actual = VCtx.bindFromAsync func input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindFromAsync with RefutedCtx should return RefutedCtx unchanged``
    (f1: int) =
    let func x = ValidCtx (x * 2)
    let input: Async<VCtx<int,int>> = async { return RefutedCtx ([f1], Map.empty) }
    let expected: VCtx<int,int> = RefutedCtx ([f1], Map.empty)
    let actual = VCtx.bindFromAsync func input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindFromAsync with DisputedCtx should bind async result to value and preserve disputes``
    (a: int, f1: int) =
    let func x = ValidCtx (x * 2)
    let input = async { return DisputedCtx ([f1], Map.empty, a) }
    let expected = DisputedCtx ([f1], Map.empty, a * 2)
    let actual = VCtx.bindFromAsync func input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindFromAsync handles synchronous function returning DisputedCtx``
    (a: int, f1: int) =
    let func x = DisputedCtx ([f1], Map.empty, x * 2)
    let input = async { return ValidCtx a }
    let expected = DisputedCtx ([f1], Map.empty, a * 2)
    let actual = VCtx.bindFromAsync func input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``bindFromAsync handles synchronous function returning RefutedCtx``
    (a: int, f1: int) =
    let func x = RefutedCtx ([f1], Map.empty)
    let input = async { return ValidCtx a }
    let expected = RefutedCtx ([f1], Map.empty)
    let actual = VCtx.bindFromAsync func input |> Async.RunSynchronously
    actual |> should equal expected

// ========== AsyncVCtx Module Tests ==========

[<Property>]
let ``AsyncVCtx.ofVCtx should lift ValidCtx to AsyncVCtx``
    (a: int) =
    let input = ValidCtx a
    let expected = ValidCtx a
    let actual = AsyncVCtx.ofVCtx input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.ofVCtx should lift RefutedCtx to AsyncVCtx``
    (f1: int) =
    let input: VCtx<int,int> = RefutedCtx ([f1], Map.empty)
    let expected = input
    let actual = AsyncVCtx.ofVCtx input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.ofVCtx should lift DisputedCtx to AsyncVCtx``
    (a: int, f1: int) =
    let input = DisputedCtx ([f1], Map.empty, a)
    let expected = input
    let actual = AsyncVCtx.ofVCtx input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.ofAsync should lift pure async value to ValidCtx``
    (a: int) =
    let input = async { return a }
    let expected = ValidCtx a
    let actual = AsyncVCtx.ofAsync input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.ofResult should lift Ok to ValidCtx``
    (a: int) =
    let input = Ok a
    let expected = ValidCtx a
    let actual = AsyncVCtx.ofResult input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.ofResult should lift Error to RefutedCtx``
    (f1: int) =
    let input = Error f1
    let actual = AsyncVCtx.ofResult input |> Async.RunSynchronously
    match actual with
    | RefutedCtx (failures, fieldFailures) ->
        failures |> should equal [f1]
        Map.isEmpty fieldFailures |> should equal true
    | _ -> failwith "Expected RefutedCtx"

[<Property>]
let ``AsyncVCtx.ofProof should lift Valid to ValidCtx``
    (a: int) =
    let input = Valid a
    let expected = ValidCtx a
    let actual = AsyncVCtx.ofProof input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.ofProof should lift Invalid to RefutedCtx``
    (f1: int) =
    let input: Proof<int, int> = Invalid ([f1], Map.empty)
    let expected: VCtx<int,int> = RefutedCtx ([f1], Map.empty)
    let actual = AsyncVCtx.ofProof input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.bind with ValidCtx should bind and return ValidCtx``
    (a: int) =
    let input = AsyncVCtx.ofVCtx (ValidCtx a)
    let fn x = AsyncVCtx.ofVCtx (ValidCtx (x * 2))
    let expected = ValidCtx (a * 2)
    let actual = AsyncVCtx.bind fn input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.bind with RefutedCtx should return RefutedCtx``
    (f1: int) =
    let input: AsyncVCtx<int,int> = AsyncVCtx.ofVCtx (RefutedCtx ([f1], Map.empty))
    let fn x = AsyncVCtx.ofVCtx (ValidCtx (x * 2))
    let expected: VCtx<int,int> = RefutedCtx ([f1], Map.empty)
    let actual = AsyncVCtx.bind fn input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.bind with DisputedCtx should bind and merge disputes``
    (a: int, f1: int, f2: int) =
    let input = AsyncVCtx.ofVCtx (DisputedCtx ([f1], Map.empty, a))
    let fn x = AsyncVCtx.ofVCtx (DisputedCtx ([f2], Map.empty, x * 2))
    let expected = DisputedCtx ([f1; f2], Map.empty, a * 2)
    let actual = AsyncVCtx.bind fn input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.bindFrom should bind sync function to async context``
    (a: int) =
    let input = AsyncVCtx.ofVCtx (ValidCtx a)
    let fn x = ValidCtx (x * 2)
    let expected = ValidCtx (a * 2)
    let actual = AsyncVCtx.bindFrom fn input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.map should transform value in ValidCtx``
    (a: int) =
    let input = AsyncVCtx.ofVCtx (ValidCtx a)
    let fn x = x * 2
    let expected = ValidCtx (a * 2)
    let actual = AsyncVCtx.map fn input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.map should preserve RefutedCtx``
    (f1: int) =
    let input: AsyncVCtx<int,int> = AsyncVCtx.ofVCtx (RefutedCtx ([f1], Map.empty))
    let fn x = x * 2
    let expected: VCtx<int,int> = RefutedCtx ([f1], Map.empty)
    let actual = AsyncVCtx.map fn input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.map should transform value and preserve disputes``
    (a: int, f1: int) =
    let input = AsyncVCtx.ofVCtx (DisputedCtx ([f1], Map.empty, a))
    let fn x = x * 2
    let expected = DisputedCtx ([f1], Map.empty, a * 2)
    let actual = AsyncVCtx.map fn input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.mapAsync should map async function over ValidCtx``
    (a: int) =
    let input = AsyncVCtx.ofVCtx (ValidCtx a)
    let fn x = async { return x * 2 }
    let expected = ValidCtx (a * 2)
    let actual = AsyncVCtx.mapAsync fn input |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.mergeSources should merge two ValidCtx``
    (a: int, b: int) =
    let input1 = AsyncVCtx.ofVCtx (ValidCtx a)
    let input2 = AsyncVCtx.ofVCtx (ValidCtx b)
    let expected = ValidCtx (a, b)
    let actual = AsyncVCtx.mergeSources input1 input2 |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.mergeSources should merge ValidCtx and DisputedCtx``
    (a: int, b: int, f1: int) =
    let input1 = AsyncVCtx.ofVCtx (ValidCtx a)
    let input2 = AsyncVCtx.ofVCtx (DisputedCtx ([f1], Map.empty, b))
    let expected = DisputedCtx ([f1], Map.empty, (a, b))
    let actual = AsyncVCtx.mergeSources input1 input2 |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.mergeSources should prioritize RefutedCtx``
    (a: int, f1: int) =
    let input1 = AsyncVCtx.ofVCtx (ValidCtx a)
    let input2: AsyncVCtx<int,int> = AsyncVCtx.ofVCtx (RefutedCtx ([f1], Map.empty))
    let actual = AsyncVCtx.mergeSources input1 input2 |> Async.RunSynchronously
    match actual with
    | RefutedCtx (failures, fieldFailures) ->
        failures |> should equal [f1]
        Map.isEmpty fieldFailures |> should equal true
    | _ -> failwith "Expected RefutedCtx"

// ========== asyncValidationBuilder Tests ==========

[<Property>]
let ``asyncValidationBuilder can bind ValidCtx directly``
    (a: int) =
    let result =
        asyncValidationBuilder {
            let! value = ValidCtx a
            return value * 2
        }
    let expected = ValidCtx (a * 2)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder can bind AsyncVCtx directly``
    (a: int) =
    // Explicitly create an AsyncVCtx and bind it
    let asyncCtx = async { return ValidCtx a }
    let result =
        asyncValidationBuilder {
            let! value = asyncCtx
            return value * 2
        }
    let expected = ValidCtx (a * 2)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder can bind pure Async value using ofAsync``
    (a: int) =
    let result =
        asyncValidationBuilder {
            let! value = AsyncVCtx.ofAsync (async { return a })
            return value * 2
        }
    let expected = ValidCtx (a * 2)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder can bind Result Ok``
    (a: int) =
    let result =
        asyncValidationBuilder {
            let! value = Ok a
            return value * 2
        }
    let expected = ValidCtx (a * 2)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder can bind Result Error``
    (f1: int) =
    let result =
        asyncValidationBuilder {
            let! value = Error f1
            return value * 2
        }
    let expected: VCtx<int, int> = RefutedCtx ([f1], Map.empty)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder can bind Proof Valid``
    (a: int) =
    let result =
        asyncValidationBuilder {
            let! value = Valid a
            return value * 2
        }
    let expected = ValidCtx (a * 2)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder can bind Proof Invalid``
    (f1: int) =
    let result =
        asyncValidationBuilder {
            let! value = Invalid ([f1], Map.empty)
            return value * 2
        }
    let expected: VCtx<int, int> = RefutedCtx ([f1], Map.empty)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder can use and! to merge sources``
    (a: int, b: int) =
    let result =
        asyncValidationBuilder {
            let! value1 = ValidCtx a
            and! value2 = ValidCtx b
            return value1 + value2
        }
    let expected = ValidCtx (a + b)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder can mix different source types with and!``
    (a: int, b: int) =
    let result =
        asyncValidationBuilder {
            let! value1 = ValidCtx a
            and! value2 = AsyncVCtx.ofAsync (async { return b })
            and! value3 = Ok (a + b)
            return value1 + value2 + value3
        }
    let expected = ValidCtx (a + b + a + b)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder preserves RefutedCtx when binding``
    (a: int, f1: int) =
    let result =
        asyncValidationBuilder {
            let! value1 = ValidCtx a
            and! value2 = RefutedCtx ([f1], Map.empty)
            return value1 + value2
        }
    let expected: VCtx<int, int> = RefutedCtx ([f1], Map.empty)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``AsyncVCtx.ofAsyncResult with Ok should lift to ValidCtx``
    (a: int) =
    let asyncResult: Async<Result<int, int>> = async { return Ok a }
    let actual = AsyncVCtx.ofAsyncResult asyncResult |> Async.RunSynchronously
    match actual with
    | ValidCtx value -> value |> should equal a
    | _ -> failwith "Expected ValidCtx"

[<Property>]
let ``AsyncVCtx.ofAsyncResult with Error should lift to RefutedCtx``
    (f1: int) =
    let asyncResult: Async<Result<int, int>> = async { return Error f1 }
    let actual = AsyncVCtx.ofAsyncResult asyncResult |> Async.RunSynchronously
    match actual with
    | RefutedCtx (failures, fieldFailures) ->
        failures |> should equal [f1]
        Map.isEmpty fieldFailures |> should equal true
    | _ -> failwith "Expected RefutedCtx"

[<Property>]
let ``asyncValidationBuilder Source overload binds Async<Result> directly``
    (a: int, b: int) =
    let result =
        asyncValidationBuilder {
            let! x = Ok a  // Result - automatic Source overload
            let! y =
                async {
                    // Async<Result> - automatic Source overload
                    return Ok b
                }
            return x + y
        }
    let expected = ValidCtx (a + b)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder Source overload for AsyncValue wraps pure async``
    (a: int) =
    let result =
        asyncValidationBuilder {
            // AsyncValue wrapper - automatic Source overload
            let! x = AsyncValue (async { return a })
            return x * 2
        }
    let expected = ValidCtx (a * 2)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder combines AsyncValue with other types``
    (a: int, b: int, c: int) =
    let result =
        asyncValidationBuilder {
            let! x = AsyncValue (async { return a })
            let! y = Ok b
            let! z = ValidCtx c
            return x + y + z
        }
    let expected = ValidCtx (a + b + c)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder parallel binds with AsyncValue``
    (a: int, b: int) =
    let result =
        asyncValidationBuilder {
            let! x = AsyncValue (async { return a })
            and! y = AsyncValue (async { return b })
            return x + y
        }
    let expected = ValidCtx (a + b)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder handles Async<Result> Error with Source overload``
    (a: int, f1: int) =
    let result =
        asyncValidationBuilder {
            let! x = Ok a
            let! y =
                async {
                    // Return an error - Source overload converts to RefutedCtx
                    return Error f1
                }
            return x + y
        }
    let actual = result |> Async.RunSynchronously
    match actual with
    | RefutedCtx (failures, fieldFailures) ->
        failures |> should equal [f1]
        Map.isEmpty fieldFailures |> should equal true
    | _ -> failwith "Expected RefutedCtx"

[<Property>]
let ``asyncValidationBuilder preserves DisputedCtx when binding``
    (a: int, b: int, f1: int) =
    let result =
        asyncValidationBuilder {
            let! value1 = ValidCtx a
            and! value2 = DisputedCtx ([f1], Map.empty, b)
            return value1 + value2
        }
    let expected = DisputedCtx ([f1], Map.empty, a + b)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected

[<Property>]
let ``asyncValidationBuilder merges multiple disputes``
    (a: int, b: int, f1: int, f2: int) =
    let result =
        asyncValidationBuilder {
            let! value1 = DisputedCtx ([f1], Map.empty, a)
            and! value2 = DisputedCtx ([f2], Map.empty, b)
            return value1 + value2
        }
    let expected = DisputedCtx ([f1; f2], Map.empty, a + b)
    let actual = result |> Async.RunSynchronously
    actual |> should equal expected
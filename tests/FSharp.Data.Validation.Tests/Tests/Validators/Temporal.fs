module FSharp.Data.Validation.Tests.Temporal

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open FSharp.Data.Validation

[<Fact>]
let ``isBefore: Returns true when date is before threshold`` () =
    let date1 = System.DateTime(2020, 1, 1)
    let date2 = System.DateTime(2021, 1, 1)
    isBefore date2 date1 |> should be True

[<Fact>]
let ``isBefore: Returns false when date is after threshold`` () =
    let date1 = System.DateTime(2021, 1, 1)
    let date2 = System.DateTime(2020, 1, 1)
    isBefore date2 date1 |> should be False

[<Fact>]
let ``isBefore: Works with DateTimeOffset`` () =
    let date1 = System.DateTimeOffset(2020, 1, 1, 0, 0, 0, System.TimeSpan.Zero)
    let date2 = System.DateTimeOffset(2021, 1, 1, 0, 0, 0, System.TimeSpan.Zero)
    isBefore date2 date1 |> should be True

[<Fact>]
let ``isAfter: Returns true when date is after threshold`` () =
    let date1 = System.DateTime(2021, 1, 1)
    let date2 = System.DateTime(2020, 1, 1)
    isAfter date2 date1 |> should be True

[<Fact>]
let ``isAfter: Returns false when date is before threshold`` () =
    let date1 = System.DateTime(2020, 1, 1)
    let date2 = System.DateTime(2021, 1, 1)
    isAfter date2 date1 |> should be False

[<Fact>]
let ``isBetweenDates: Returns true when date is within range`` () =
    let start = System.DateTime(2020, 1, 1)
    let end' = System.DateTime(2022, 1, 1)
    let date = System.DateTime(2021, 1, 1)
    isBetweenDates start end' date |> should be True

[<Fact>]
let ``isBetweenDates: Returns false when date is outside range`` () =
    let start = System.DateTime(2020, 1, 1)
    let end' = System.DateTime(2021, 1, 1)
    let date = System.DateTime(2022, 1, 1)
    isBetweenDates start end' date |> should be False

[<Fact>]
let ``isInPast: Returns true for past dates`` () =
    let pastDate = System.DateTime.UtcNow.AddDays(-1.0)
    isInPast pastDate |> should be True

[<Fact>]
let ``isInPast: Returns false for future dates`` () =
    let futureDate = System.DateTime.UtcNow.AddDays(1.0)
    isInPast futureDate |> should be False

[<Fact>]
let ``isInFuture: Returns true for future dates`` () =
    let futureDate = System.DateTime.UtcNow.AddDays(1.0)
    isInFuture futureDate |> should be True

[<Fact>]
let ``isInFuture: Returns false for past dates`` () =
    let pastDate = System.DateTime.UtcNow.AddDays(-1.0)
    isInFuture pastDate |> should be False

[<Fact>]
let ``isWeekday: Returns true for Monday`` () =
    let monday = System.DateTime(2024, 1, 1) // This is a Monday
    isWeekday monday |> should be True

[<Fact>]
let ``isWeekday: Returns false for Saturday`` () =
    let saturday = System.DateTime(2024, 1, 6) // This is a Saturday
    isWeekday saturday |> should be False

[<Fact>]
let ``isWeekday: Returns false for Sunday`` () =
    let sunday = System.DateTime(2024, 1, 7) // This is a Sunday
    isWeekday sunday |> should be False

[<Fact>]
let ``minimumAge: Returns true when age meets minimum`` () =
    let dateOfBirth = System.DateTime.Today.AddYears(-20)
    minimumAge 18 dateOfBirth |> should be True

[<Fact>]
let ``minimumAge: Returns false when age does not meet minimum`` () =
    let dateOfBirth = System.DateTime.Today.AddYears(-16)
    minimumAge 18 dateOfBirth |> should be False

[<Fact>]
let ``minimumAge: Handles birthday edge case`` () =
    let dateOfBirth = System.DateTime.Today.AddYears(-18)
    minimumAge 18 dateOfBirth |> should be True

[<AutoOpen>]
module FSharp.Data.Validation.Temporal

/// Checks that a date/time value is before the given threshold.
/// Works with DateTime, DateTimeOffset, and other IComparable types.
/// If not, it adds the given failure to the result and validation continues.
let isBefore (threshold: 'T :> System.IComparable<'T>) (value: 'T) = value.CompareTo(threshold) < 0

/// Checks that a date/time value is after the given threshold.
/// Works with DateTime, DateTimeOffset, and other IComparable types.
/// If not, it adds the given failure to the result and validation continues.
let isAfter (threshold: 'T :> System.IComparable<'T>) (value: 'T) = value.CompareTo(threshold) > 0

/// Checks that a date/time value is between the given start and end values (inclusive).
/// Works with DateTime, DateTimeOffset, and other IComparable types.
/// If not, it adds the given failure to the result and validation continues.
let isBetweenDates (start: 'T :> System.IComparable<'T>) (end': 'T) (value: 'T) =
    value.CompareTo(start) >= 0 && value.CompareTo(end') <= 0

/// Checks that a DateTime value is in the past relative to the current UTC time.
/// If not, it adds the given failure to the result and validation continues.
let isInPast (value: System.DateTime) = value < System.DateTime.UtcNow

/// Checks that a DateTimeOffset value is in the past relative to the current UTC time.
/// If not, it adds the given failure to the result and validation continues.
let isInPastOffset (value: System.DateTimeOffset) = value < System.DateTimeOffset.UtcNow

/// Checks that a DateTime value is in the future relative to the current UTC time.
/// If not, it adds the given failure to the result and validation continues.
let isInFuture (value: System.DateTime) = value > System.DateTime.UtcNow

/// Checks that a DateTimeOffset value is in the future relative to the current UTC time.
/// If not, it adds the given failure to the result and validation continues.
let isInFutureOffset (value: System.DateTimeOffset) = value > System.DateTimeOffset.UtcNow

/// Checks that a DateTime value is a weekday (Monday through Friday).
/// If not, it adds the given failure to the result and validation continues.
let isWeekday (value: System.DateTime) =
    value.DayOfWeek <> System.DayOfWeek.Saturday
    && value.DayOfWeek <> System.DayOfWeek.Sunday

/// Checks that a DateTimeOffset value is a weekday (Monday through Friday).
/// If not, it adds the given failure to the result and validation continues.
let isWeekdayOffset (value: System.DateTimeOffset) =
    value.DayOfWeek <> System.DayOfWeek.Saturday
    && value.DayOfWeek <> System.DayOfWeek.Sunday

/// Checks that a date of birth meets the minimum age requirement (in years).
/// If not, it adds the given failure to the result and validation continues.
let minimumAge (minYears: int) (dateOfBirth: System.DateTime) =
    let today = System.DateTime.Today
    let age = today.Year - dateOfBirth.Year

    let age =
        if
            today.Month < dateOfBirth.Month
            || (today.Month = dateOfBirth.Month && today.Day < dateOfBirth.Day)
        then
            age - 1
        else
            age

    age >= minYears

/// Checks that a date of birth meets the minimum age requirement (in years).
/// If not, it adds the given failure to the result and validation continues.
let minimumAgeOffset (minYears: int) (dateOfBirth: System.DateTimeOffset) =
    let today = System.DateTimeOffset.UtcNow.Date
    let dob = dateOfBirth.Date
    let age = today.Year - dob.Year

    let age =
        if today.Month < dob.Month || (today.Month = dob.Month && today.Day < dob.Day) then
            age - 1
        else
            age

    age >= minYears

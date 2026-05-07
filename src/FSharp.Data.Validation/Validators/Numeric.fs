[<AutoOpen>]
module FSharp.Data.Validation.Numeric

/// Checks that a value is equal to another.
let isEqual = (=)

/// Checks that a value is not equal to another.
let isNotEqual a b = a = b |> not

/// Checks that b is less than a, as b is our validation input.
let isLessThan = (>)

/// Checks that b is greater than a, as b is our validation input.
let isGreaterThan = (<)

/// Checks that b is less than or equal to a, as b is our validation input.
let isLessThanOrEqual = (>=)

/// Checks that b is greater than or equal to a, as b is our validation input.
let isGreaterThanOrEqual = (<=)

/// Checks that a value is within the given inclusive range [min, max].
let inRange (min: 'a) (max: 'a) (value: 'a) = value >= min && value <= max

/// Checks that a value is within the given exclusive range (min, max).
let inRangeExclusive (min: 'a) (max: 'a) (value: 'a) = value > min && value < max

/// Checks that a numeric value is positive (greater than zero).
let inline isPositive value = value > LanguagePrimitives.GenericZero

/// Checks that a numeric value is negative (less than zero).
let inline isNegative value = value < LanguagePrimitives.GenericZero

/// Checks that a numeric value is not zero.
let inline isNonZero value = value <> LanguagePrimitives.GenericZero

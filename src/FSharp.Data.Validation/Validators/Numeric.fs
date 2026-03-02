[<AutoOpen>]
module FSharp.Data.Validation.Numeric

/// Checks that a value is equal to another.
/// If not, it adds the given failure to the result and validation continues.
let isEqual = (=)

/// Checks that a value is not equal to another.
/// If equal, it adds the given failure to the result and validation continues.
let isNotEqual a b = a = b |> not

/// Checks that b is less than a, as b is our validation input.
/// If not, it adds the given failure to the result and validation continues.
let isLessThan = (>)

/// Checks that b is greater than a, as b is our validation input.
/// If not, it adds the given failure to the result and validation continues.
let isGreaterThan = (<)

/// Checks that b is less than or equal to a, as b is our validation input.
/// If not, it adds the given failure to the result and validation continues.
let isLessThanOrEqual = (>=)

/// Checks that b is greater than or equal to a, as b is our validation input.
/// If not, it adds the given failure to the result and validation continues.
let isGreaterThanOrEqual = (<=)

/// Checks that a value is within the given inclusive range [min, max].
/// If not, it adds the given failure to the result and validation continues.
let inRange (min: 'a) (max: 'a) (value: 'a) = value >= min && value <= max

/// Checks that a value is within the given exclusive range (min, max).
/// If not, it adds the given failure to the result and validation continues.
let inRangeExclusive (min: 'a) (max: 'a) (value: 'a) = value > min && value < max

/// Checks that a numeric value is positive (greater than zero).
/// If not, it adds the given failure to the result and validation continues.
let inline isPositive value = value > LanguagePrimitives.GenericZero

/// Checks that a numeric value is negative (less than zero).
/// If not, it adds the given failure to the result and validation continues.
let inline isNegative value = value < LanguagePrimitives.GenericZero

/// Checks that a numeric value is not zero.
/// If not, it adds the given failure to the result and validation continues.
let inline isNonZero value = value <> LanguagePrimitives.GenericZero

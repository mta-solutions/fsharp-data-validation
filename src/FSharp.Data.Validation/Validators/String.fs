[<AutoOpen>]
module FSharp.Data.Validation.String

open System.Text.RegularExpressions

/// Checks that a string matches the given regular expression pattern.
let matchesRegex (pattern: Regex) (str: string) = pattern.IsMatch str

/// Checks that a string contains any of the given characters.
let containsAny (chars: char seq) (str: string) =
    chars |> Seq.exists str.Contains

/// Checks that a string contains all of the given characters.
let containsAll (chars: char seq) (str: string) =
    chars |> Seq.forall str.Contains

/// Checks that a string starts with the given prefix.
let startsWith (prefix: string) (str: string) = str.StartsWith prefix

/// Checks that a string ends with the given suffix.
let endsWith (suffix: string) (str: string) = str.EndsWith suffix

/// Checks that a string contains only alphanumeric characters (letters and digits).
let isAlphanumeric (str: string) =
    str |> Seq.forall System.Char.IsLetterOrDigit

/// Checks that a string contains only alphabetic characters (letters).
let isAlpha (str: string) = str |> Seq.forall System.Char.IsLetter

/// Checks that a string contains only numeric characters (digits).
let isNumeric (str: string) = str |> Seq.forall System.Char.IsDigit

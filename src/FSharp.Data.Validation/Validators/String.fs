[<AutoOpen>]
module FSharp.Data.Validation.String

open System.Text.RegularExpressions

/// Checks that a string matches the given regular expression pattern.
/// If not, it adds the given failure to the result and validation continues.
let matchesRegex (pattern: Regex) (str: string) = pattern.IsMatch(str)

/// Checks that a string contains any of the given characters.
/// If not, it adds the given failure to the result and validation continues.
let containsAny (chars: char list) (str: string) =
    chars |> List.exists (fun c -> str.Contains(c))

/// Checks that a string contains all of the given characters.
/// If not, it adds the given failure to the result and validation continues.
let containsAll (chars: char list) (str: string) =
    chars |> List.forall (fun c -> str.Contains(c))

/// Checks that a string starts with the given prefix.
/// If not, it adds the given failure to the result and validation continues.
let startsWith (prefix: string) (str: string) = str.StartsWith(prefix)

/// Checks that a string ends with the given suffix.
/// If not, it adds the given failure to the result and validation continues.
let endsWith (suffix: string) (str: string) = str.EndsWith(suffix)

/// Checks that a string contains only alphanumeric characters (letters and digits).
/// If not, it adds the given failure to the result and validation continues.
let isAlphanumeric (str: string) =
    str |> Seq.forall System.Char.IsLetterOrDigit

/// Checks that a string contains only alphabetic characters (letters).
/// If not, it adds the given failure to the result and validation continues.
let isAlpha (str: string) = str |> Seq.forall System.Char.IsLetter

/// Checks that a string contains only numeric characters (digits).
/// If not, it adds the given failure to the result and validation continues.
let isNumeric (str: string) = str |> Seq.forall System.Char.IsDigit

module FSharp.Data.Validation.Samples.Primitives

open System.Text.RegularExpressions

open FSharp.Data.Validation

type MyFailures =
    | NameFailure
    | LengthFailure
    | EmailFailure
    | RequiredFailure
    | OtherFailure

// app specific types
type Username = { unUsername : string }

let mkUsername s =
    validation {
        withValue s
        disputeWithFact NameFailure isNotNull
        whenProven (fun v -> { unUsername = v } )
    } |> fromVCtx

type EmailAddress = { unEmailAddress : string }

let mkEmailAddress s =
    validation {
        withValue s
        disputeWithFact EmailFailure (fun s -> Regex.IsMatch(s, "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+"))
        whenProven (fun v -> { unEmailAddress = v })
    } |> fromVCtx

type PhoneNumber = { unPhoneNumber : string }


let mkPhoneNumber s =
    validation {
        withValue s
        disputeWithFact LengthFailure (isLength 7)
        whenProven (fun v -> { unPhoneNumber = v })
    } |> fromVCtx

type ContactPreference =
    | Email
    | Phone

type ZipCode = { unZipCode : string }

let mkZipCode s =
    validation {
        withValue s
        disputeWithFact RequiredFailure isNotNull
        whenProven (fun v -> { unZipCode = v })
    } |> fromVCtx

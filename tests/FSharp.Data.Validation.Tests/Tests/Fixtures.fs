module FSharp.Data.Validation.Tests.Fixtures

open System.Text.RegularExpressions

open Xunit
open FsCheck
open FsCheck.Xunit

open FSharp.Data.Validation

// Type that is required to be greater than 1
type UserId = { unUserId : int }

type UserIdFailures =
    | LessThanOneFailure

let mkUserId s =
    validation {
        withValue s
        disputeWithFact LessThanOneFailure (isGreaterThanOrEqual 1)
        whenProven (fun v -> { unUserId = v } )
    } |> fromVCtx

[<Property>]
let ``mkUserId: Returns Valid when value is greater than or equal to 1`` (PositiveInt a) =
    Assert.Equal(Valid { unUserId = a }, mkUserId a)

[<Property>]
let ``mkUserId: Returns LessThanOneFailure when value is negative`` (NegativeInt a) =
    Assert.Equal(Invalid ([LessThanOneFailure], Map.empty), mkUserId a)

[<Fact>]
let ``mkUserId: Returns LessThanOneFailure when value is zero`` () =
    Assert.Equal(Invalid ([LessThanOneFailure], Map.empty), mkUserId 0)

// Type that is required to be 7 length and only contain numbers
type PhoneNumber = { unPhoneNumber : string }

type PhoneNumberFailures =
    | LengthFailure
    | NonDigitFailure

let mkPhoneNumber s =
    validation {
        withValue s
        disputeWithFact LengthFailure (isLength 7)
        disputeWithFact NonDigitFailure (fun a -> Regex.IsMatch(a, "^[0-9]*$"))
        whenProven (fun v -> { unPhoneNumber = v })
    } |> fromVCtx

[<Fact>]
let ``mkPhoneNumber: Returns Valid when value passes criteria`` () =
    let a = "1231234"
    Assert.Equal(Valid { unPhoneNumber = a }, mkPhoneNumber "1231234")

[<Fact>]
let ``mkPhoneNumber: Returns LengthFailure when value is too short`` () =
    Assert.Equal(Invalid ([LengthFailure], Map.empty), mkPhoneNumber "1")

[<Fact>]
let ``mkPhoneNumber: Returns NonDigitFailure when value contains non-numeric characters`` () =
    Assert.Equal(Invalid ([NonDigitFailure], Map.empty), mkPhoneNumber "123134!")

[<Fact>]
let ``mkPhoneNumber: Returns both failures when wrong length and contains non-numeric characters`` () =
    Assert.Equal(Invalid ([LengthFailure; NonDigitFailure], Map.empty), mkPhoneNumber "-12312345678!")

// Type that has no requirements beyond meeting a specific regex
type EmailAddress = { unEmailAddress : string }

type EmailAddressFailures =
    | InvalidEmail

let mkEmailAddress s =
    validation {
        withValue s
        disputeWithFact InvalidEmail (fun s -> Regex.IsMatch(s, "^[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+$"))
        whenProven (fun v -> { unEmailAddress = v })
    } |> fromVCtx

[<Fact>]
let ``mkEmailAddress: Returns Valid when value passes criteria`` () =
    let a = "test@test.com"
    Assert.Equal(Valid { unEmailAddress = a }, mkEmailAddress a)

[<Fact>]
let ``mkEmailAddress: Returns InvalidEmail when invalid`` () =
    Assert.Equal(Invalid ([InvalidEmail], Map.empty), mkEmailAddress "test@test")

// Record that must include a userid and a phone number
type ContactPreference =
    | Email
    | Phone

type UserContact = {
    UserId : UserId
    PhoneNumber : PhoneNumber option
    EmailAddress : EmailAddress option
    ContactPreference : ContactPreference
}

type RecordFailures =
    | UserIdFailure of UserIdFailures
    | PhoneNumberFailure of PhoneNumberFailures
    | EmailAddressFailure of EmailAddressFailures
    | MissingConditionalEmail
    | MissingConditionalPhone
    | MissingUserId
    | OtherFailure

type UserContactDTO =
    {
        UserId : int option
        PhoneNumber : string option
        EmailAddress : string option
        ContactPreference : ContactPreference
    }
    interface IValidatable<RecordFailures, UserContact> with
        member this.Validation() =
            validation {
                let! uid = validation {
                    withField (mkName (nameof this.UserId)) this.UserId
                    refuteWith (isRequired MissingUserId)
                    refuteWithProof (Proof.mapInvalid UserIdFailure << mkUserId)
                    qed
                }
                and! cp = validation {
                    withField (mkName (nameof this.ContactPreference)) this.ContactPreference
                    qed
                }
                and! pn = validation {
                    withField (mkName "PhoneNumber") this.PhoneNumber
                    disputeWith (isRequiredWhen MissingConditionalPhone (this.ContactPreference = Phone))
                    optional (fun v -> validation {
                        withValue v
                        refuteWithProof (Proof.mapInvalid PhoneNumberFailure << mkPhoneNumber)
                    })
                    qed
                }
                and! ea = validation {
                    withField (mkName "EmailAddress") this.EmailAddress
                    disputeWith (isRequiredUnless MissingConditionalEmail (this.ContactPreference <> Email))
                    optional (fun v -> validation {
                        withValue v
                        refuteWithProof (Proof.mapInvalid EmailAddressFailure << mkEmailAddress)
                    })
                    qed
                }
                and! _ = validation {
                    withValue this
                    disputeWithFact OtherFailure (fun a -> a.UserId <> Some 0)
                    qed
                }
                return { UserId = uid; PhoneNumber = pn; EmailAddress = ea; ContactPreference = cp }
            }

[<Property>]
let ``UserContactDTO: Validated when all values pass criteria`` (PositiveInt uid) =
    let phone = None
    let email = "test@test.com"
    let cp = Email
    let input = {
        UserContactDTO.UserId = Some uid
        PhoneNumber = phone
        EmailAddress = Some email
        ContactPreference = cp
    }
    let expected = Valid {
        UserContact.UserId = { unUserId = uid }
        PhoneNumber = phone
        EmailAddress = Some { unEmailAddress = email }
        ContactPreference = cp
    }
    Assert.Equal(expected, validate input)

[<Property>]
let ``UserContactDTO: Returns single failure when email is invalid`` (PositiveInt uid) =
    let phone = None
    let email = "test@test"
    let cp = Email
    let input = {
        UserContactDTO.UserId = Some uid
        PhoneNumber = phone
        EmailAddress = Some email
        ContactPreference = cp
    }
    let expected =
        Invalid (
            [],
            Map.ofList [
                ([mkName "EmailAddress" |> Option.get], [EmailAddressFailure InvalidEmail])
            ])
    Assert.Equal(expected, validate input)

[<Property>]
let ``UserContactDTO: Returns multiple failures when email and userid are invalid`` (NegativeInt uid) =
    let phone = None
    let email = "test@test"
    let cp = Email
    let input = {
        UserContactDTO.UserId = Some uid
        PhoneNumber = phone
        EmailAddress = Some email
        ContactPreference = cp
    }
    let expected =
        Invalid (
            [],
            Map.ofList [
                ([mkName "UserId" |> Option.get], [UserIdFailure LessThanOneFailure])
                ([mkName "EmailAddress" |> Option.get], [EmailAddressFailure InvalidEmail])
            ])
    Assert.Equal(expected, validate input)

[<Fact>]
let ``UserContactDTO: Returns multiple failures when email is invalid and userid is missing`` () =
    let phone = None
    let email = "test@test"
    let cp = Email
    let input = {
        UserContactDTO.UserId = None
        PhoneNumber = phone
        EmailAddress = Some email
        ContactPreference = cp
    }
    let expected =
        Invalid (
            [],
            Map.ofList [
                ([mkName "UserId" |> Option.get], [MissingUserId])
                ([mkName "EmailAddress" |> Option.get], [EmailAddressFailure InvalidEmail])
            ])
    Assert.Equal(expected, validate input)

[<Fact>]
let ``UserContactDTO: Returns multiple failures and global when email is invalid and userid is 0`` () =
    let phone = None
    let email = "test@test"
    let cp = Email
    let input = {
        UserContactDTO.UserId = Some 0
        PhoneNumber = phone
        EmailAddress = Some email
        ContactPreference = cp
    }
    let expected =
        Invalid (
            [OtherFailure],
            Map.ofList [
                ([mkName "UserId" |> Option.get], [UserIdFailure LessThanOneFailure])
                ([mkName "EmailAddress" |> Option.get], [EmailAddressFailure InvalidEmail])
            ])
    Assert.Equal(expected, validate input)

[<Property>]
let ``UserContactDTO: Returns multiple failures when email is invalid and contact preference is phone`` (PositiveInt uid) =
    let phone = None
    let email = "test@test"
    let cp = Phone
    let input = {
        UserContactDTO.UserId = Some uid
        PhoneNumber = phone
        EmailAddress = Some email
        ContactPreference = cp
    }
    let expected =
        Invalid (
            [],
            Map.ofList [
                ([mkName "EmailAddress" |> Option.get], [EmailAddressFailure InvalidEmail])
                ([mkName "PhoneNumber" |> Option.get], [MissingConditionalPhone])
            ])
    Assert.Equal(expected, validate input)

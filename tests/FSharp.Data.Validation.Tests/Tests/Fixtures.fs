module FSharp.Data.Validation.Tests.Fixtures

open System.Text.RegularExpressions

open Xunit
open FsCheck
open FsCheck.Xunit

open FSharp.Data.Validation

// Type that is required to be greater than 1
type UserId = UserId of int

type UserIdFailures =
    | LessThanOneFailure

module UserId =
    let make s =
        validation {
            withValue s
            disputeWithFact LessThanOneFailure (isGreaterThanOrEqual 1)
            qed UserId
        } |> fromVCtx

[<Property>]
let ``UserId.make: Returns Valid when value is greater than or equal to 1`` (PositiveInt a) =
    Assert.Equal(Valid (UserId a), UserId.make a)

[<Property>]
let ``UserId.make: Returns LessThanOneFailure when value is negative`` (NegativeInt a) =
    Assert.Equal(Invalid ([LessThanOneFailure], Map.empty), UserId.make a)

[<Fact>]
let ``UserId.make: Returns LessThanOneFailure when value is zero`` () =
    Assert.Equal(Invalid ([LessThanOneFailure], Map.empty), UserId.make 0)

// Type that is required to be 7 length and only contain numbers
type PhoneNumber = PhoneNumber of string

type PhoneNumberFailures =
    | LengthFailure
    | NonDigitFailure

module PhoneNumber =
    let make s =
        validation {
            withValue s
            disputeWithFact LengthFailure (isLength 7)
            disputeWithFact NonDigitFailure (fun a -> Regex.IsMatch(a, "^[0-9]*$"))
            qed PhoneNumber
        } |> fromVCtx

[<Fact>]
let ``PhoneNumber.make: Returns Valid when value passes criteria`` () =
    let a = "1231234"
    Assert.Equal(Valid (PhoneNumber a), PhoneNumber.make "1231234")

[<Fact>]
let ``PhoneNumber.make: Returns LengthFailure when value is too short`` () =
    Assert.Equal(Invalid ([LengthFailure], Map.empty), PhoneNumber.make "1")

[<Fact>]
let ``PhoneNumber.make: Returns NonDigitFailure when value contains non-numeric characters`` () =
    Assert.Equal(Invalid ([NonDigitFailure], Map.empty), PhoneNumber.make "123134!")

[<Fact>]
let ``PhoneNumber.make: Returns both failures when wrong length and contains non-numeric characters`` () =
    Assert.Equal(Invalid ([LengthFailure; NonDigitFailure], Map.empty), PhoneNumber.make "-12312345678!")

// Type that has no requirements beyond meeting a specific regex
type EmailAddress = EmailAddress of string

type EmailAddressFailures =
    | InvalidEmail

module EmailAddress =
    let make (s : string) =
        validation {
            withValue s
            disputeWithFact InvalidEmail (fun s -> Regex.IsMatch(s, "^[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+$"))
            qed EmailAddress
        } |> fromVCtx

[<Fact>]
let ``EmailAddress.make: Returns Valid when value passes criteria`` () =
    let a = "test@test.com"
    Assert.Equal(Valid (EmailAddress a), EmailAddress.make a)

[<Fact>]
let ``EmailAddress.make: Returns InvalidEmail when invalid`` () =
    Assert.Equal(Invalid ([InvalidEmail], Map.empty), EmailAddress.make "test@test")

// Record that must include a userid and a phone number
type ContactPreference =
    | Email
    | Phone

type UserContact =
    { UserId : UserId
      PhoneNumber : PhoneNumber option
      EmailAddress : EmailAddress option
      ContactPreference : ContactPreference }

type RecordFailures =
    | UserIdFailure of UserIdFailures
    | PhoneNumberFailure of PhoneNumberFailures
    | EmailAddressFailure of EmailAddressFailures
    | MissingContractPreference
    | MissingConditionalEmail
    | MissingConditionalPhone
    | MissingUserId
    | OtherFailure

type UserContactDTO =
    { UserId : int option
      PhoneNumber : string option
      EmailAddress : string option
      ContactPreference : ContactPreference option }
module UserContactDTO =
    let makeUserContact(vm:UserContactDTO) =
        validation {
            let! uid = validation {
                withField (fun () -> vm.UserId)
                refuteWith (isRequired MissingUserId)
                refuteWithProof (Proof.mapInvalid UserIdFailure << UserId.make)
                qed
            }
            and! cp = validation {
                withField (fun () -> vm.ContactPreference)
                refuteWith (isRequired MissingContractPreference)
                qed
            }
            and! pn = validation {
                withField (fun () -> vm.PhoneNumber)
                disputeWith (isRequiredWhen MissingConditionalPhone (vm.ContactPreference = Some Phone))
                optional (fun v -> validation {
                    withValue v
                    refuteWithProof (Proof.mapInvalid PhoneNumberFailure << PhoneNumber.make)
                })
                qed
            }
            and! ea = validation {
                withField (fun () -> vm.EmailAddress)
                disputeWith (isRequiredUnless MissingConditionalEmail (vm.ContactPreference <> Some Email))
                optional (fun v -> validation {
                    withValue v
                    refuteWithProof (Proof.mapInvalid EmailAddressFailure << EmailAddress.make)
                })
                qed
            }
            and! _ = validation {
                withValue vm
                disputeWithFact OtherFailure (fun a -> a.UserId <> Some 0)
                qed
            }
            return { UserContact.UserId = uid; PhoneNumber = pn; EmailAddress = ea; ContactPreference = cp }
        } |> fromVCtx

[<Property>]
let ``UserContactDTO: Validated when all values pass criteria`` (PositiveInt uid) =
    let phone = None
    let email = "test@test.com"
    let cp = Email
    let input = {
        UserContactDTO.UserId = Some uid
        PhoneNumber = phone
        EmailAddress = Some email
        ContactPreference = Some cp
    }
    let expected = Valid {
        UserContact.UserId = UserId uid
        PhoneNumber = phone
        EmailAddress = Some (EmailAddress email)
        ContactPreference = cp
    }
    Assert.Equal(expected, UserContactDTO.makeUserContact(input))

[<Property>]
let ``UserContactDTO: Returns single failure when email is invalid`` (PositiveInt uid) =
    let phone = None
    let email = "test@test"
    let cp = Email
    let input = {
        UserContactDTO.UserId = Some uid
        PhoneNumber = phone
        EmailAddress = Some email
        ContactPreference = Some cp
    }
    let expected =
        Invalid (
            [],
            Map.ofList [
                ([mkName "EmailAddress" |> Option.get], [EmailAddressFailure InvalidEmail])
            ])
    Assert.Equal(expected, UserContactDTO.makeUserContact(input))

[<Property>]
let ``UserContactDTO: Returns multiple failures when email and userid are invalid`` (NegativeInt uid) =
    let phone = None
    let email = "test@test"
    let cp = Email
    let input = {
        UserContactDTO.UserId = Some uid
        PhoneNumber = phone
        EmailAddress = Some email
        ContactPreference = Some cp
    }
    let expected =
        Invalid (
            [],
            Map.ofList [
                ([mkName "UserId" |> Option.get], [UserIdFailure LessThanOneFailure])
                ([mkName "EmailAddress" |> Option.get], [EmailAddressFailure InvalidEmail])
            ])
    Assert.Equal(expected, UserContactDTO.makeUserContact(input))

[<Fact>]
let ``UserContactDTO: Returns multiple failures when email is invalid and userid is missing`` () =
    let phone = None
    let email = "test@test"
    let cp = Email
    let input = {
        UserContactDTO.UserId = None
        PhoneNumber = phone
        EmailAddress = Some email
        ContactPreference = Some cp
    }
    let expected =
        Invalid (
            [],
            Map.ofList [
                ([mkName "UserId" |> Option.get], [MissingUserId])
                ([mkName "EmailAddress" |> Option.get], [EmailAddressFailure InvalidEmail])
            ])
    Assert.Equal(expected, UserContactDTO.makeUserContact(input))

[<Fact>]
let ``UserContactDTO: Returns multiple failures and global when email is invalid and userid is 0`` () =
    let phone = None
    let email = "test@test"
    let cp = Email
    let input = {
        UserContactDTO.UserId = Some 0
        PhoneNumber = phone
        EmailAddress = Some email
        ContactPreference = Some cp
    }
    let expected =
        Invalid (
            [OtherFailure],
            Map.ofList [
                ([mkName "UserId" |> Option.get], [UserIdFailure LessThanOneFailure])
                ([mkName "EmailAddress" |> Option.get], [EmailAddressFailure InvalidEmail])
            ])
    Assert.Equal(expected, UserContactDTO.makeUserContact(input))

[<Property>]
let ``UserContactDTO: Returns multiple failures when email is invalid and contact preference is phone`` (PositiveInt uid) =
    let phone = None
    let email = "test@test"
    let cp = Phone
    let input = {
        UserContactDTO.UserId = Some uid
        PhoneNumber = phone
        EmailAddress = Some email
        ContactPreference = Some cp
    }
    let expected =
        Invalid (
            [],
            Map.ofList [
                ([mkName "EmailAddress" |> Option.get], [EmailAddressFailure InvalidEmail])
                ([mkName "PhoneNumber" |> Option.get], [MissingConditionalPhone])
            ])
    Assert.Equal(expected, UserContactDTO.makeUserContact(input))

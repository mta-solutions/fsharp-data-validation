module GettingStarted.Types

open FSharp.Data.Validation
open System
open System.Linq
open System.Text.RegularExpressions

type Name = private Name of string

type NameFailure = 
    | Empty

module Name =
    let make (str:string) =
        validation {
            withValue str
            disputeWithFact Empty isNotNull
            qed Name
        } |> fromVCtx

    let unwrap (Name s) = s

type Username = private Username of string

type UsernameFailure =
    | Empty
    | InvalidCharacter

module Username =
    let make (str:string) =
        validation {
            withValue str
            disputeWithFact Empty isNotNull
            disputeWithFact InvalidCharacter (fun str -> str.Any(fun c -> not (Char.IsLetterOrDigit(c) || c = '-' || c = '_')))
            qed Username
        } |> fromVCtx

    let unwrap (Username s) = s

type Password = private Password of string

type PasswordFailure =
    | Empty
    | MinLength
    | NeedsTwoOfLetterNumberSpecial

module Password = 
    let private hasLetter (str:string) = str.Any(fun c -> Char.IsLetter(c))
    let private hasNumber (str:string) = str.Any(fun c -> Char.IsDigit(c))
    let private hasSpecial (str:string) = str.Any(fun c -> Char.IsLetterOrDigit(c) |> not)
    let private countCharTypes (str:string) =
        let letter = if hasLetter str then 1 else 0
        let number = if hasNumber str then 1 else 0
        let special = if hasSpecial str then 1 else 0
        letter + number + special

    let make (str:string) =
        validation {
            withValue str
            disputeWithFact Empty isNotNull
            disputeWithFact NeedsTwoOfLetterNumberSpecial (fun str -> countCharTypes str > 1)
            disputeWithFact MinLength (minLength 8)
            qed Password
        } |> fromVCtx

    let unwrap (Password s) = s

type EmailAddress = private {
    username: string
    domain: string
} with
    member public this.Username = this.username
    member public this.Domain = this.domain
    
type EmailAddressFailure =
    | InvalidDomain
    | InvalidUsername
    | MissingAtSymbol
    | MultipleAtSymbols

module EmailAddress =
    let make (str:string) =
        validation {
            withValue str
            refuteWith (fun s -> // the string passed into `withValue` above is passed in here
                let ss = s.Split([| '@' |])
                match ss.Length with
                | 0 -> Error MissingAtSymbol
                | 1 -> Ok ss // The result has the type of `string[]`
                | _ -> Error MultipleAtSymbols
            )
            // the `string[]` returned above is passed in to the function here
            disputeWithFact InvalidUsername (fun ss -> Regex.IsMatch(ss[0], "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*\")"))
            disputeWithFact InvalidDomain (fun ss -> Regex.IsMatch(ss[1], "(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])"))
            // the `string[]` returned above is passed in to the function here and transformed into an `EmailAddress`
            qed (fun ss -> { username = ss[0]; domain = ss[1] })
        } |> fromVCtx

    let unwrap (e:EmailAddress) = sprintf "%s@%s" (e.Username) (e.Domain)

type PhoneNumber = private {
    areaCode: string
    exchange: string
    lineNumber: string
} with
    member public this.AreaCode = this.areaCode
    member public this.Exchange = this.exchange
    member public this.LineNumber = this.lineNumber
    
type PhoneNumberFailure =
    | Empty
    | MissingAreaCode
    | ToShort
    | ToLong

module PhoneNumber =
    let make (str:string) =
        // remove non-numbers
        let clean = Regex.Replace(str, "\\D", String.Empty)
        validation {
            withValue clean
            disputeWithFact Empty (isNotNull)
            refuteWith (fun s -> if s.Length = 7 then Error MissingAreaCode else Ok s)
            disputeWithFact ToShort (minLength 10)
            disputeWithFact ToLong (minLength 11)
            qed (fun s ->
                let s' = if s.Length = 10 then s else s.Substring(1)
                { areaCode = s.Substring(0,3); exchange = s.Substring(3,3); lineNumber = s.Substring(6) }
            )
        } |> fromVCtx

    let unwrap (e:PhoneNumber) = sprintf "%s-%s-%s" (e.AreaCode) (e.Exchange) (e.LineNumber)

// Contact types
type Contact =
    | Call of PhoneNumber
    | Text of PhoneNumber
    | Email of EmailAddress
      
type ContactFailure = 
    | MissingContactType
    | MissingContactDetails
    | InvalidPhoneNumber of PhoneNumberFailure
    | InvalidEmailAddress of EmailAddressFailure

type ContactType =
    | Call
    | Text
    | Email

type ContactVM =
    { ContactType: ContactType option
      ContactDetails: string option }
    with
    member this.MakeContact() =
        validation {
            let! typ = validation {
                withField (fun () -> this.ContactType)
                refuteWith (isRequired MissingContactType)
                qed
            }
            and! d = validation {
                withField (fun () -> this.ContactDetails)
                refuteWith (isRequired MissingContactDetails)
                qed
            }
            let! result =
                match typ with
                | ContactType.Call -> validation {
                        withField (fun () -> this.ContactDetails) d
                        refuteWithProof (PhoneNumber.make >> Proof.mapInvalid InvalidPhoneNumber)
                        qed (fun pn -> Contact.Call pn)
                    }
                | ContactType.Text -> validation {
                        withField (fun () -> this.ContactDetails) d
                        refuteWithProof (PhoneNumber.make >> Proof.mapInvalid InvalidPhoneNumber)
                        qed (fun pn -> Contact.Text pn)
                    }
                | ContactType.Email -> validation {
                        withField (fun () -> this.ContactDetails) d
                        refuteWithProof (EmailAddress.make >> Proof.mapInvalid InvalidEmailAddress)
                        qed (fun pn -> Contact.Email pn)
                    }
            return result
        } |> fromVCtx

// The validated new user type (the model)
type NewUser = private { 
    name: Name option
    username: Username
    password: Password
    preferedContact: Contact
    additionalContacts: Contact list 
} with
    member public this.Name = this.name
    member public this.Username = this.username
    member public this.Password = this.password
    member public this.PreferedContact = this.preferedContact
    member public this.AdditionalContacts = this.additionalContacts

type NewUserFailure = 
    | RequiredField
    | NameMatchesUsername
    | InvalidName of NameFailure
    | InvalidUsername of UsernameFailure
    | InvalidPassword of PasswordFailure
    | InvalidContact of ContactFailure

// The unvalidated new user type (the view model)
type NewUserVM =
    { Name: string option
      Username: string option
      Password: string option
      PreferedContact: ContactVM option
      AdditionalContacts: ContactVM list }
with
member this.MakeNewUser() = 
    validation {
        let! name = validation {
            withField (fun () -> this.Name)
            optional (fun v -> validation {
                withValue v
                refuteWithProof (Name.make >> Proof.mapInvalid InvalidName)
            })
            qed
        }
        and! username = validation {
            withField (fun () -> this.Username)
            refuteWith (isRequired RequiredField)
            refuteWithProof (Username.make >> Proof.mapInvalid InvalidUsername)
            qed
        }
        and! password = validation {
            withField (fun () -> this.Password)
            refuteWith (isRequired RequiredField)
            refuteWithProof (Password.make >> Proof.mapInvalid InvalidPassword)
            qed
        }
        and! preferedContact = validation {
            withField (fun () -> this.PreferedContact)
            refuteWith (isRequired RequiredField)
            refuteWithProof (fun pc -> pc.MakeContact() |> Proof.mapInvalid InvalidContact)
            qed
        }
        and! additionalContacts = validation {
            withField (fun () -> this.AdditionalContacts)
            refuteEachWithProof (fun (pc: ContactVM) -> pc.MakeContact() |> Proof.mapInvalid InvalidContact)
            qed List.ofSeq
        }
        and! _ = validation {
            withValue this
            disputeWithFact NameMatchesUsername (fun a -> a.Name = a.Username |> not)
            qed
        }
        return { NewUser.name = name; username = username; password = password; preferedContact = preferedContact; additionalContacts = additionalContacts }
    } |> fromVCtx

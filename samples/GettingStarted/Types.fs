module GettingStarted.Types

open FSharp.Data.Validation

type Name = private Name of string

type NameFailure = 
    | Empty

let unName (Name s) = s

let mkName (str:string) =
    validation {
        withValue str
        disputeWithFact Empty isNotNull
        qed Name
    }

type Username = private Username of string

let unUsername (Username s) = s

let mkUsername (str:string) =
    validation {
        withValue str
        disputeWithFact Empty isNotNull
        disputeIfAny (fun f -> f.IsNumber() || f.IsLetter() || f = '-' || f = '_')
        qed Username
    }

type Password = private Password of string

let unPassword (Password s) = s

type EmailAddress = private {
    username: string
    domain: string
} with
    member public this.Username = this.username
    member public this.Domain = this.domain

let unEmailAddress (e:EmailAddress) = sprintf "%s@%s" (e.Username) (e.Domain)

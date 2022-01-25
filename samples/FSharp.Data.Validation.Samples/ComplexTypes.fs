module FSharp.Data.Valdation.Samples.ComplexTypes

open FSharp.Data.Validation

open FSharp.Data.Validation.Samples.Primitives

// Model
type User = {
        Username          : Username;
        EmailAddress      : EmailAddress option;
        PhoneNumber       : PhoneNumber option;
        ContactPreference : ContactPreference;
        ZipCode           : ZipCode option;
    }

// View Model
type UserVM =
    { Username          : string option
      EmailAddress      : string option
      PhoneNumber       : string option
      ContactPreference : ContactPreference option
      ZipCode           : string option }
    interface IValidatable<MyFailures, User> with
        member this.Validation() =
            validation {
                let! cp = validation {
                    withField (mkName (nameof this.ContactPreference)) this.ContactPreference
                    refuteWith (isRequired RequiredFailure)
                    qed
                }
                and! un = validation {
                    withField (mkName (nameof this.Username)) this.Username
                    refuteWith (isRequired RequiredFailure)
                    refuteWithProof mkUsername
                    qed
                }
                and! ea = validation {
                    withField (mkName (nameof this.EmailAddress)) this.EmailAddress
                    optional (fun v -> validation {
                        withValue v
                        refuteWithProof mkEmailAddress
                    })
                    disputeWith (isRequiredWhen RequiredFailure (this.ContactPreference = Some ContactPreference.Email))
                    qed
                }
                and! pn = validation {
                    withField (mkName (nameof this.PhoneNumber)) this.PhoneNumber
                    disputeWith (isRequiredWhen RequiredFailure (this.ContactPreference = Some ContactPreference.Phone))
                    optional (fun v -> validation {
                        withValue v
                        refuteWithProof mkPhoneNumber
                    })
                    qed
                }
                and! z = validation {
                    withField (mkName (nameof this.ZipCode)) this.ZipCode
                    optional (fun v -> validation {
                        withValue v
                        refuteWithProof mkZipCode
                    })
                    qed
                }
                and! _ = validation {
                    withValue this
                    disputeWithFact OtherFailure (fun a -> a.EmailAddress = a.Username |> not)
                    qed
                }
                return { User.Username = un; EmailAddress = ea; PhoneNumber = pn; ContactPreference = cp; ZipCode = z }
            }

module FSharp.Data.Valdation.Sample

open FSharp.Data.Validation

open FSharp.Data.Validation.Samples.Primitives

// Model
type User = private { 
        Username          : Username
        EmailAddress      : EmailAddress option
        PhoneNumber       : PhoneNumber option
        ContactPreference : ContactPreference
        ZipCode           : ZipCode option 
    }

// View Model
type UserVM =
    { Username          : string option
      EmailAddress      : string option
      PhoneNumber       : string option
      ContactPreference : ContactPreference option
      ZipCode           : string option }

module UserVM =
    let makeUser(vm:UserVM) =
        validation {
            let! cp = validation {
                withField (fun () -> vm.ContactPreference)
                refuteWith (isRequired RequiredField)
                qed
            }
            and! un = validation {
                withField (fun () -> vm.Username)
                refuteWith (isRequired RequiredField)
                refuteWithProof (Username.make >> Proof.mapInvalid InvalidUsername)
                qed
            }
            and! ea = validation {
                withField (fun () -> vm.EmailAddress)
                optional (fun v -> validation {
                    withValue v
                    refuteWithProof (EmailAddress.make >> Proof.mapInvalid InvalidEmailAddress)
                })
                disputeWith (isRequiredWhen RequiredField (vm.ContactPreference = Some ContactPreference.Email))
                qed
            }
            and! pn = validation {
                withField (fun () -> vm.PhoneNumber)
                disputeWith (isRequiredWhen RequiredField (vm.ContactPreference = Some ContactPreference.Phone))
                optional (fun v -> validation {
                    withValue v
                    refuteWithProof (PhoneNumber.make >> Proof.mapInvalid InvalidPhoneNumber)
                })
                qed
            }
            and! z = validation {
                withField (fun () -> vm.ZipCode)
                optional (fun v -> validation {
                    withValue v
                    refuteWithProof (ZipCode.make >> Proof.mapInvalid InvalidZipCode)
                })
                qed
            }
            and! _ = validation {
                withValue vm
                disputeWithFact EmailAddressMatchesUsername (fun a -> a.EmailAddress = a.Username |> not)
                qed
            }
            return { User.Username = un; EmailAddress = ea; PhoneNumber = pn; ContactPreference = cp; ZipCode = z }
        } |> fromVCtx

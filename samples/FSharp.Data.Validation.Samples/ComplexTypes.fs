module FSharp.Data.Valdation.Sample

open FSharp.Data.Validation

open FSharp.Data.Validation.Samples.Primitives

// View Model
type UserVM =
    { Username          : string option
      EmailAddress      : string option
      PhoneNumber       : string option
      ContactPreference : ContactPreference option
      ZipCode           : string option }

// Model
type User = private { 
        Username          : Username
        EmailAddress      : EmailAddress option
        PhoneNumber       : PhoneNumber option
        ContactPreference : ContactPreference
        ZipCode           : ZipCode option 
    } with
    static member Make(viewModel: UserVM) =
        validation {
            let! cp = validation {
                withField (fun () -> viewModel.ContactPreference)
                refuteWith (isRequired RequiredField)
                qed
            }
            and! un = validation {
                withField (fun () -> viewModel.Username)
                refuteWith (isRequired RequiredField)
                refuteWithProof (mkUsername >> Proof.mapInvalid InvalidUsername)
                qed
            }
            and! ea = validation {
                withField (fun () -> viewModel.EmailAddress)
                optional (fun v -> validation {
                    withValue v
                    refuteWithProof (mkEmailAddress >> Proof.mapInvalid InvalidEmailAddress)
                })
                disputeWith (isRequiredWhen RequiredField (viewModel.ContactPreference = Some ContactPreference.Email))
                qed
            }
            and! pn = validation {
                withField (fun () -> viewModel.PhoneNumber)
                disputeWith (isRequiredWhen RequiredField (viewModel.ContactPreference = Some ContactPreference.Phone))
                optional (fun v -> validation {
                    withValue v
                    refuteWithProof (mkPhoneNumber >> Proof.mapInvalid InvalidPhoneNumber)
                })
                qed
            }
            and! z = validation {
                withField (fun () -> viewModel.ZipCode)
                optional (fun v -> validation {
                    withValue v
                    refuteWithProof (mkZipCode >> Proof.mapInvalid InvalidZipCode)
                })
                qed
            }
            and! _ = validation {
                withValue viewModel
                disputeWithFact EmailAddressMatchesUsername (fun a -> a.EmailAddress = a.Username |> not)
                qed
            }
            return { User.Username = un; EmailAddress = ea; PhoneNumber = pn; ContactPreference = cp; ZipCode = z }
        } |> fromVCtx

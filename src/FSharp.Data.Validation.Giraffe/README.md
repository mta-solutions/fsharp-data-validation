# FSharp.Data.Validation.Giraffe

## Description

This library is intended to provide useful pieces for implementing model validation in Giraffe.

## IModelValidation Interface and Giraffe Integration

The `IModelValidation` interface is a re-implementation of the Giraffe interface with an added generic type parameter 'U, which represents the type of the validated model returned by the `Validate` method.
It allows you to define model validation logic and transform unvalidated models into validated models.
This guide will walk you through how to use the `IModelValidation` interface to validate models using the FSharp.Data.Validation library and integrate them with Giraffe to build web applications.

### IModelValidation Interface

The `IModelValidation` interface is defined as follows:

```fsharp
type IModelValidation<'T, 'U> =
    abstract member Validate: unit -> Result<'U, HttpHandler>
```

- `'T`: The unvalidated model type.
- `'U`: The validated model type.

The `Validate` method is the contract for validating an object's state.
If the object has a valid state, the method should return the validated model of type `'U`.
Otherwise, it should return a `HttpHandler` function, which will be used to send an error response back to the client.

### Using IModelValidation with FSharp.Data.Validation

Let's consider an example where we have a model `NewUserVM` representing a new user request, and we want to validate this model and transform it into a `NewUser` model with more constraints.
The validation will include checking the presence of required fields and validating the format of the username and password.

```fsharp
type PasswordFailures =
    | Empty
    | TooShort
    | TooLong

type UsernameFailures = | Empty

type MyOtherFailures =
    | RequiredField
    | InvalidUsername of UsernameFailures
    | InvalidPassword of PasswordFailures

type Password = private Password of string

// Define Password validation logic using FSharp.Data.Validation
// ...

type Username = private Username of string

// Define Username validation logic using FSharp.Data.Validation
// ...

type NewUser =
    { Name: string
      Username: Username
      Password: Password }

[<CLIMutable>]
type NewUserVM =
    { Name: string option
      Username: string option
      Password: string option }

    // Implement a ValidateModel function to validate and transform NewUserVM into NewUser.
    member this.ValidateModel() =
        validation {
            let! n =
                validation {
                    withField (fun () -> this.Name)
                    refuteWith (isRequired RequiredField)
                    qed
                }

            and! un =
                validation {
                    withField (fun () -> this.Username)
                    refuteWith (isRequired RequiredField)
                    refuteWithProof (Username.make >> Proof.mapInvalid InvalidUsername)
                    qed
                }

            and! pw =
                validation {
                    withField (fun () -> this.Password)
                    refuteWith (isRequired RequiredField)
                    refuteWithProof (Password.make >> Proof.mapInvalid InvalidPassword)
                    qed
                }

            return
                ({ Name = n
                   Username = un
                   Password = pw }
                : NewUser)
        }
        |> fromVCtx

    // Implement the IModelValidation interface for NewUserVM.
    interface MyModule.IModelValidation<NewUserVM, NewUser> with
        member this.Validate() =
            match
                //
                this.ValidateModel() |> Proof.toResult
            with
            | Ok validatedType -> Ok validatedType
            | Error e -> Error(RequestErrors.BAD_REQUEST e)

// Handle the HTTP request to create a new user.
let handleCreateUser (vm: NewUser) =
    fun (next: HttpFunc) (ctx: HttpContext) -> task { return! Successful.OK $"New user %s{vm.Name} created!" next ctx }

let webApp: HttpFunc -> HttpContext -> HttpFuncResult =
    // Use the `bindModel` function from the Giraffe library to bind the request body to the NewUserVM type.
    // Then use the `validateModel` function from MyModule to validate the model and transform it into the NewUser type.
    POST
    >=> route "/users"
    >=> bindModel<NewUserVM> None (MyModule.validateModel handleCreateUser)
```

In the above example, we define the `Password`, `Username`, `NewUser`, and `NewUserVM` types.
The `NewUserVM` type has an implementation of the `ValidateModel` function using FSharp.Data.Validation.
It validates the `NewUserVM` object and returns a `NewUser` model if it's valid.

To use the `IModelValidation` interface with `NewUserVM`, we implement the interface to define the validation logic for `NewUserVM`.
The `Validate` method of the interface is responsible for executing the validation logic and returning either the validated model or an error response.

### Integration with Giraffe

We can integrate the `NewUserVM` validation with Giraffe by using the `validateModel` function from `MyModule`.
This function takes a function that accepts the validated model and returns a `HttpHandler` function.
It then validates the model and either passes the validated model to the provided function or returns an error response if validation fails.

In the `webApp` function, we use the `bindModel` function to bind the request body to the `NewUserVM` type.
We then use the `validateModel` function to validate the model and transform it into the `NewUser` type.
Finally, we handle the request using the `handleCreateUser` function to create a new user.

### Conclusion

The `IModelValidation` interface provides a flexible way to define and execute model validation logic using FSharp.Data.Validation.
By integrating it with Giraffe, you can easily validate and handle HTTP requests with validated models.
The example provided demonstrates how to use the interface to validate a `NewUserVM` model and transform it into a `NewUser` model with additional constraints.

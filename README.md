# FSharp.Data.Validation <!-- omit in toc -->

*A functional, transformation-oriented approach to data validation.*

## Table of Contents <!-- omit in toc -->

- [Getting Started](#getting-started)
  - [Validating Primitive Types](#validating-primitive-types)
  - [The Proof Type](#the-proof-type)
  - [Failures Are Types Too](#failures-are-types-too)
  - [The Validation Computation Expression](#the-validation-computation-expression)
  - [`withValue`, `withField`, `qed`, and `ValueCtx`](#withvalue-withfield-qed-and-valuectx)
  - [The `dispute*` and `refute*` Operations](#the-dispute-and-refute-operations)
  - [Back to the Example](#back-to-the-example)
  - [Validating Complex Types](#validating-complex-types)
  - [The `let!` Operator](#the-let-operator)
  - [The `and!` Operator](#the-and-operator)
  - [The `return` Operator](#the-return-operator)
  - [The `optional` Operator](#the-optional-operator)
  - [Global Validation](#global-validation)
  - [Validating Nested Types](#validating-nested-types)
  - [Validating Collections](#validating-collections)
  - [Serializing the Proof Type](#serializing-the-proof-type)
- [Validation Operations](#validation-operations)
  - [`refute*` Operations](#refute-operations)
    - [`refute`](#refute)
    - [`refuteMany`](#refutemany)
    - [`refuteWith`](#refutewith)
    - [`refuteWithMany`](#refutewithmany)
    - [`refuteWithProof`](#refutewithproof)
    - [`refuteEachWith`](#refuteeachwith)
    - [`refuteEachWithProof`](#refuteeachwithproof)
  - [`dispute*` Operations](#dispute-operations)
    - [`dispute`](#dispute)
    - [`disputeMany`](#disputemany)
    - [`disputeWith`](#disputewith)
    - [`disputeWithMany`](#disputewithmany)
    - [`disputeWithFact`](#disputewithfact)
    - [`disputeAnyWith`](#disputeanywith)
    - [`disputeAllWith`](#disputeallwith)
    - [`disputeAnyWithMany`](#disputeanywithmany)
    - [`disputeAllWithMany`](#disputeallwithmany)
    - [`disputeAnyWithFact`](#disputeanywithfact)
    - [`disputeAllWithFact`](#disputeallwithfact)
    - [`validateEach`](#validateeach)
- [Validation Helpers](#validation-helpers)
  - [The `isRequired` Helper](#the-isrequired-helper)
  - [The `isRequiredWhen` Helper](#the-isrequiredwhen-helper)
  - [The `isRequiredUnless` Helper](#the-isrequiredunless-helper)
  - [The `isOk` Helper](#the-isok-helper)
  - [The `isError` Helper](#the-iserror-helper)
  - [The `isNull` Helper](#the-isnull-helper)
  - [The `isNotNull` Helper](#the-isnotnull-helper)
  - [The `minLength` Helper](#the-minlength-helper)
  - [The `maxLength` Helper](#the-maxlength-helper)
  - [The `isLength` Helper](#the-islength-helper)
  - [The `hasElem` Helper](#the-haselem-helper)
  - [The `doesNotHaveElem` Helper](#the-doesnothaveelem-helper)
  - [The `isEqual` Helper](#the-isequal-helper)
  - [The `isNotEqual` Helper](#the-isnotequal-helper)
  - [The `isLessThan` Helper](#the-islessthan-helper)
  - [The `isGreaterThan` Helper](#the-isgreaterthan-helper)
  - [The `isLessThanOrEqual` Helper](#the-islessthanorequal-helper)
  - [The `isGreaterThanOrEqual` Helper](#the-isgreaterthanorequal-helper)
  - [The `isValid` Helper](#the-isvalid-helper)
  - [The `isInvalid` Helper](#the-isinvalid-helper)
  - [The `flattenProofs` Helper](#the-flattenproofs-helper)
  - [The `raiseIfInvalid` Helper](#the-raiseifinvalid-helper)
- [Proof Helpers](#proof-helpers)
  - [`toResult` Helper](#toresult-helper)
  - [`toValidationFailures` Helper](#tovalidationfailures-helper)
- [`IValidateable` Interface](#ivalidateable-interface)
  - [Example Usage](#example-usage)
- [Haskell Data-Validation Library](#haskell-data-validation-library)

## Getting Started

*The code examples for this library can be found [here](samples/GettingStarted/).*

The purpose of this library is twofold.
First, it ensures that your code only consumes validated data.
This is achieved by transforming types through validation.
Second, it provides an easy way to build validations by offering various types, functions, and tools that ensure consistency.

In functional programming, a fundamental concept is that it should be impossible to represent invalid states in your application.
This reduces bugs and unexpected behavior, but it requires proper implementation.
One aspect of this implementation is how types are defined within the application.
Well-implemented types should not allow invalid states to exist.
Any attempt to create an invalid state should result in a compile-time error.

The validation problem greatly benefits from this concept.
One of the most significate, core concepts of this library is that validation should transform a type once it has been validated.
This ensures that it becomes impossible to pass invalid data into a function that is not designed to handle it.

Let's illustrate this with an example.
Consider a code snippet that takes an email address and sends an email.
For the purpose of explanation, we won't implement the actual function but focus on the email address parameter.

```fsharp
module Example

let notifyUser (emailAddress:string) =
    // send email
```

This works.
However, you may have already noticed the issue.
The issue is that **any** string can be passed as an email address.
The type of the parameter does not enforce any restrictions.
For example, the function could be invoked like this:

```fsharp
notifyUser "Not an email address"
```

Surprisingly, the compiler would accept this.
In F#, we *really* want to build our program so that it is impossible to introduce these kind of bugs.
Ideally, the function application mentioned above should have triggered a compiler error.
Let's see how we can address this.

### Validating Primitive Types

The first step is to define a new type in a separate module dedicated to types.
This is a common pattern in functional languages.

```fsharp
module Example.Types

type EmailAddress = private EmailAddress of string
```

Note that we made the constructor private.
This ensures that the type can only be constructed within the Types module.
Combining this with smart constructors allows us to apply additional logic before creating an instance of the type.

```fsharp
module Example.Types

...

module EmailAddress =
    let make (str:string): ReturnType?? = 
    // validate
```

But what should the return type be?

### The Proof Type

The result of a validation function needs to fulfill several requirements:

 1. Clearly express the validation result
 1. Preserve all the failures that occurred during validation
 2. For complex types, indicate the specific fields that failed

The first requirement is met by using the `Result<'T, 'E>` type.
Meeting the other requirements would require another type to wrap `'T` so we can accumulate failures without losing the value.
Specifying `Result<SomeWrapper<'T>, 'E>` each time is cumbersome.
Let's come up with something that is easier to use and more idiomatic.

```fsharp
type Proof<'F, 'T> = 
    | Valid of 'T
    | Invalid of 'F list * Map<string, 'F list>
```

The `Invalid` constructor takes a list of failures at the type level and a map of all the failures at the field level.
The keys of this map represent the names of the fields that failed.
Of course, the real `Proof<'F, 'T>` type is a bit more complicated.
The keys of the field level failures are lists of `Name` to support nested validations.
More details on this will be provided later.

Let's update our smart constructor to utilize this type:

```fsharp
module Example.Types

open FSharp.Data.Validation

...

    let make (str:string): Proof<'F??, EmailAddress> = 
        // validation
```

Now, the question remains: What type should we use for `'F`?

### Failures Are Types Too

When discussing validation failures, we refer to them as *failures* rather than *errors*.
The term "error" implies undesirable or unexpected behavior, whereas validation issues are expected and should be handled gracefully by returning meaningful results to the user.
Therefore, validation issues are not errors.
But how do we represent these failures?

With types, of course!

```fsharp
module Example.Types

...

type EmailAddressValidationFailure =
    | MissingDomain
    | MissingUsername
    | MissingAtSymbol
    | MultipleAtSymbols

module EmailAddress =
    let make (str:string): Proof<EmailAddressValidationFailure, EmailAddress> = 
        // validation
```

It is crucial to have a failure type for each data type that requires validation.
We'll discuss how to compose these types later.
Having specific failure cases allows us to handle each failure individually without resorting to catch-all `match` expressions to handle cases we don't care about.
This approach is particularly important because catch-all `match` expressions can lead to code that is not type-safe.

Imagine adding a new case that you want to handle.
If you don't have a catch-all pattern, the compiler will indicate which parts of the program need to be updated.
However, with catch-all patterns, you would need to manually search for every match expression that needs to be updated (not ideal!).

Now, let's explore how we can actually validate our email address string.

### The Validation Computation Expression

Computation expressions are a powerful feature in F# that facilitate the creation of domain-specific languages (DSLs) for important tasks in your code.
You may recognize the syntax from `query` and `async` expressions.
Let's take a look at some code.

```fsharp
module Example.Types

...
    
    let make (str:string): Proof<EmailAddressValidationFailure, EmailAddress> = 
        validation {
            // validation logic goes here
        } |> fromVCtx
```

First, notice the `validation` computation expression.
It follows the standard syntax for computation expressions.
All the validation logic should be encapsulated within this expression.

At the end of the expression, the result is passed to the `fromVCtx` function.
This is necessary because the computation expression internally uses the `VCtx<'F, 'A>` type.
This type holds the value being validated as well as any validation failures.
`fromVCtx` converts the `VCtx` type to the `Proof` type.

We can't use the `Proof` type directly within the expression because it only has two states: `Valid` and `Invalid`.
The `VCtx` type, on the other hand, includes an additional state that allows us to track both the value and the failures simultaneously.
This is essential for handling validations that are performed after other validations have already failed.
For example, if a password fails validation due to a lack of numeric characters, we can still check if it meets the length requirement.
To achieve this, we need to track the password's value and the failed validations.

Now let's proceed to the next part of our validation example, where we specify what we are validating.

```fsharp
module Example.Types

...
    
    let make (str:string): Proof<EmailAddressValidationFailure, EmailAddress> = 
        validation {
            withValue str
            // validation logic goes here
            qed EmailAddress
        } |> fromVCtx
```

### `withValue`, `withField`, `qed`, and `ValueCtx`

When validating a complex type, we often need to apply specific validations to each field.
For primitive values, we validate the value itself.
We use the `validation` computation expression to indicate when we are validating a value versus a field.

Validation failures at the value level are added to the global failures list in the `Proof` type.
Field-level failures are added to the field failure map.
This enables consumers of the validation failures to identify the specific fields that failed and the corresponding reasons.

Behind the scenes, the computation expression utilizes the `ValueCtx<'A>` type.
This type holds the value being validated and, in the case of field validations, the name of the field being validated.
Working directly with the `ValueCtx` type is unnecessary.

To validate a value, we use the `withValue` operation by passing the value to be validated.
For fields, we employ the `withField` operation and provide the field's name (`Name` type) and value.
The `Name` type can be created by passing a string to the `mkName` function.

```fsharp
...

validation {
    let! un = validation {
        withField (mkName (nameof this.Username)) (this.Username)
        // validations
        qed
    }
    // validate additional fields
    return { Username = un; (* set additional fields *) }
}
```

Alternatively, `withField` has an overload that allows you to pass a selector function.
The selector is used to determine the field's name and value.

```fsharp
...

validation {
    let! un = validation {
        withField (fun () -> this.Username)
        // validations
        qed
    }
    // validate additional fields
    return { Username = un; (* set additional fields *) }
}
```

We will see `withField` later when we discuss validating complex types.
For now, we will just use `withValue`.
Now, how do we unwrap a value from the `ValueCtx` when we are done validating it?

#### Don't Forget `qed` <!-- omit in toc -->

The `qed` operation has two overloads.
The one without parameters simply unwraps the value from the `ValueCtx`.
This is useful when the validation transforms the unvalidated type into the validated type during the process.
We will see more examples of this when examining complex type validation.

The second overload of `qed` accepts a function that transforms the unvalidated type into the new type.
In the earlier example, we passed the `EmailAddress` constructor to `qed` to wrap the string in the `EmailAddress` type.

```fsharp
validation {
    withValue str
    // validation stuff
    qed EmailAddress
} |> fromVCtx
```

With all the necessary components in place, let's validate our email address string.
While we could use a regular expression, it wouldn't showcase the library effectively.
Instead, let's perform the validation manually using the `refute*` and `dispute*` operations!

```fsharp
module Example.Types

...
    
    let make (str:string): Proof<EmailAddressValidationFailure, EmailAddress> = 
        validation {
            withValue str
            refuteWith (fun s ->
                let ss = s.Split([| '@' |])
                match ss.Length with
                | 1 -> Error MissingAtSymbol
                | 2 -> Ok ss
                | _ -> Error MultipleAtSymbols
            )
            disputeWithFact MissingUsername (fun ss -> isNotNull ss[0])
            disputeWithFact MissingDomain (fun ss -> isNotNull ss[1])
            qed (fun ss -> EmailAddress (sprintf "%s@%s" ss[0] ss[1]))
        } |> fromVCtx
```

### The `dispute*` and `refute*` Operations

There are two key differences between the `dispute*` operations and the `refute*` operations:

1. Refuting a value halts further validation, while disputing does not.
2. Refuting a value allows for transformation, while disputing does not.

Let's consider a scenario where we validate a password string represented by the type `string option`.
The password field has specific requirements: it is required, must be at least 8 characters long, and contain both letters and numbers.
This implies that the string needs validation.

If the string has the value `Some "mypass"`, we would expect it to pass some checks but fail others.
For instance, it should pass the checks for a required value and containing letters, but fail the checks for minimum length and numbers.

Suppose our validation logic looks like this:

```fsharp
validation {
    // check for value existence
    // check length
    // check for letters
    // check for numbers
} |> fromVCtx
```

In this case, will the check for numbers ever be executed for our value? It should.
We want to verify the presence of letters and numbers even if the length is incorrect.
We want to identify as many validation failures as possible.

This is where the `dispute*` operations come into play.
If one validation fails, the computation expression continues to evaluate the remaining validations.
We can use a dispute operation to verify the length, letters, and numbers.
However, we cannot use it to check if the value exists, as `dispute*` operations do not transform values.

So far, we have only discussed the case when our password `string option` has a value.
What if the value is `None`? Can we perform any further validation in that case? No, because it's an incompatible type.
If we want to continue validation, we need to transform our `string option` into a `string`, but we cannot achieve this without a value.

This is where the `refute*` operations come in handy.
Refute operations attempt to transform a value as part of the validation process.
If the value cannot be validated, it cannot be transformed.
And if the value cannot be transformed, no further checks can be performed.

It is important to conduct as many checks as possible during validation.
However, it is impossible to validate a value that has the wrong type.
Similarly, it is beneficial to transform values into different types during validation, but it is not possible to conduct further validation on a type that cannot be transformed.
Hence, we require both `dispute*` and `refute*` operations to handle different validation scenarios.

### Back to the Example

Now that we understand the distinction between `dispute*` and `refute*`, let's break down our example.
The `refuteWith` operation takes a function with the signature `'A -> Result<'F, 'B>`.
This function checks if a value is suitable for transformation from type `'A` to `'B`.
If it is, the function performs the transformation and returns the result.
Otherwise, it returns a failure.

If the check passes, the returned value is used for further validation.
If the check fails, the failure is added to the result and the validation process ends.

On the other hand, `disputeWithFact` takes a failure value and a check function that returns a `bool`.
If the check function returns `false`, the provided failure value is added to the result, and the validation continues.
If the check function returns `true`, validation proceeds without adding any failures to the result.

Let's revisit the code from the previous example, with some additional clarifications:

```fsharp
module Example.Types

...
    
    let make (str:string): Proof<EmailAddressValidationFailure, EmailAddress> = 
        validation {
            withValue str
            refuteWith (fun s -> // The string passed into `withValue` is passed here
                let ss = s.Split([| '@' |])
                match ss.Length with
                | 0 -> Error MissingAtSymbol
                | 1 -> Ok ss // The result has type `string[]`
                | _ -> Error MultipleAtSymbols
            )
            // The `string[]` returned above is passed to the function here
            disputeWithFact MissingUsername (fun ss -> isNotNull ss[0])
            disputeWithFact MissingDomain (fun ss -> isNotNull ss[1])
            // The `string[]` returned above is passed to the function here and transformed into an `EmailAddress`
            qed (fun ss -> EmailAddress (sprintf "%s@%s" ss[0] ss[1]))
        } |> fromVCtx
```

Now that we have our validation function, let's revisit the original `notifyUser` function:

```fsharp
module Example

let notifyUser (emailAddress:string) =
    // send email
```

The only change required here is to update the type of the `emailAddress` parameter:

```fsharp
module Example

let notifyUser (emailAddress:EmailAddress) =
    // send email
```

Done!
Now our code will only compile if we pass a valid email address to the `notifyUser` function.

Now let's move on to...

### Validating Complex Types

Let's consider a scenario where we have a form for new users to sign up on our website.
The form collects the user's name, username, email address, and password.
All fields except the name are required.
Additionally, we want to ensure that the username is not the same as the user's actual name for security reasons.
Before we can validate this data, we need to define two types to model the data: a validated type (the model) and an unvalidated type (the view model).

```fsharp
module Example.Types

// Primitive types and smart constructors

// Validated new user type (the model)
type NewUser = private { 
    name: Name option
    username: Username
    password: Password
    emailAddress: EmailAddress 
} with
    member public this.Name = this.name
    member public this.Username = this.username
    member public this.Password = this.password
    member public this.EmailAddress = this.emailAddress

// Unvalidated new user type (the view model)
type NewUserVM =
    { Name: string option
      Username: string option
      Password: string option
      EmailAddress: string option }
```

We require two types because type-safe validation involves type transformation.
We accept unvalidated data and transform it into the validated type by performing the validation.
We refer to the unvalidated type as a "view model" and the validated type as a "model."

In the validated type, we mark the constructor as private.
With F# records, this ensures that the fields are not visible to any module outside the declaring module.
Consequently, we define public accessors to allow reading the data.

Notice that we use optional values for each field in the view model.
This approach allows us to accept the data in its simplest state, making minimal assumptions.
Using a simple `string` type would imply the presence of a value.

Now that we have our types defined, let's create a smart constructor for the model.
This smart constructor will accept the view model as a parameter, validate it, and return an instance of the model type.
For complex types, we typically define the smart constructor as a function within a module named after the view model.
This is consistent with the approach used for primitive types.
We can then pass the view model as a parameter to this function.

```fsharp
module Example.Types

...
// Unvalidated new user type (the view model)
type NewUserVM =
    { Name: string option
      Username: string option
      Password: string option
      EmailAddress: string option }

module NewUserVM =
    let makeNewUser() = 
        validation {
            let! name = validation {
                // validate name
            }
            // validate additional fields
            // validate that the username is not equal to the user's name
            // return the model type
        } |> fromVCtx
```

You may notice the nested `validation` blocks, which resemble nested `async` computation expressions.
However, there are some new syntax elements that we need to introduce.

### The `let!` Operator

The `let!` operator enables us to perform validation on individual fields of the view model.
Once the validation is complete, the `let!` operator unwraps the `VCtx` type, providing access to the underlying validated value.
This validated value can be utilized for additional checks or passed to the model's constructor.

However, if the validation is refuted, the entire computation expression terminates.
This can pose a challenge when dealing with records that have multiple fields, as we want to validate all the fields even if one of them fails.
It's crucial to capture as many failures as possible before concluding the validation.

```fsharp
module Example.Types

...

let makeNewUser(vm: NewUserVM) = 
    validation {
        let! name = validation {
            // if this validation is refuted
        }
        let! username = validation {
            // this validation will never execute
        }
        // validate additional fields
        // validate that the username does not equal the user's name
        // return the model type
    } |> fromVCtx
```

This is where the `and!` operator comes into play.

### The `and!` Operator

The `and!` operator functions similarly to the `let!` operator, but it ensures that the computation expression evaluates all the `and!` expressions and the final `let!` expression.
The `let!` and `and!` operators form a chain that begins with the initial `let!` and concludes with the last `and!` statement.
At the end of the chain, the computation expression combines the results from all the branches into a single `VCtx` value.
This guarantees that all the fields have been validated, even if the first one in the code block is refuted.

Let's revisit our example using the `and!` operator:

```fsharp
module Example.Types

...

let makeNewUser(vm: NewUserVM) =  
    validation {
        let! name = validation {
            // this validation always runs
        }
        and! username = validation {
            // so does this one
        }
        and! password = validation {
            // this one too
        }
        and! emailAddress = validation {
            // you get the idea
        }
        // validate that the username does not equal the user's name
        // return the model type
    } |> fromVCtx
```

However, there are a couple of important points to keep in mind.
You cannot access any value assigned by the operators until the chain is complete:

```fsharp
module Example.Types

...

let makeNewUser(vm: NewUserVM) = 
    validation {
        let! name = validation {
            // validate the name field
        }
        and! username = validation {
            printf "%s" name // this will fail to compile because the `name` variable is not accessible yet
            // perform additional validation
        }
...
```

Additionally, if any validations occur after the chain and the chain is refuted, the subsequent validations will not be executed:

```fsharp
module Example.Types

...

// if this chain is refuted
let! name = validation {
    ...
}
and! username = validation {
    ...
}

// this chain will never execute
let! password = validation {
    ...
}
and! emailAddress = validation {
    ...
}

...
```

In most cases, this is not a concern.
Just ensure that you include as many checks as possible within a validation chain to capture relevant failures.

### The `return` Operator

Let's incorporate the `withField` and `qed` operators into our example and return a value with the validated fields.

```fsharp
module Example.Types

...

let makeNewUser(vm: NewUserVM) =  
    validation {
        let! name = validation {
            withField (fun () -> vm.Name)
            // validate name
            qed
        }
        and! username = validation {
            withField (fun () -> vm.Username)
            // validate username
            qed
        }
        and! password = validation {
            withField (fun () -> vm.Password)
            // validate password
            qed
        }
        and! emailAddress = validation {
            withField (fun () -> vm.EmailAddress)
            // validate email address
            qed
        }
        // validate that the username does not equal the user's name
        return { NewUser.Name = name; Username = username; Password = password; EmailAddress = emailAddress }
    } |> fromVCtx
```

We use the `return` operator to wrap the value on the right side in a valid `VCtx`.
As this is the last line of the validation computation expression, it becomes the result of the expression.
Notice that the fields of the model are assigned using the variables bound by the `let!` and `and!` operators.
The variables already have the correct types due to their validation.

Speaking of validating fields, wouldn't it be nice if we could use the primitive smart constructors we already created to validate them?
Yes, it would.
However, we need a way to map the primitive failure types to the failure types specific to our `NewUser` type.

```fsharp
module Example.Types

...

type NewUserFailure = 
    | RequiredField
    | NameMatchesUsername
    | InvalidName of NameFailure
    | InvalidUsername of UsernameFailure
    | InvalidPassword of PasswordFailure
    | InvalidEmailAddress of EmailAddressFailure

...

let makeNewUser(vm: NewUserVM) = 
    validation {
        let! name = validation {
            withField (fun () -> vm.Name)
            // how do we validate an optional field?
            qed
        }
        and! username = validation {
            withField (fun () -> vm.Username)
            refuteWith (isRequired RequiredField)
            refuteWithProof (mkUsername >> Proof.mapInvalid InvalidUsername)
            qed
        }
        and! password = validation {
            withField (fun () -> vm.Password)
            refuteWith (isRequired RequiredField)
            refuteWithProof (mkPassword >> Proof.mapInvalid InvalidPassword)
            qed
        }
        and! emailAddress = validation {
            withField (fun () -> vm.EmailAddress)
            refuteWith (isRequired RequiredField)
            refuteWithProof (mkEmailAddress >> Proof.mapInvalid InvalidEmailAddress)
            qed
        }
        // validate that the username does not equal the user's name
        return { NewUser.Name = name; Username = username; Password = password; EmailAddress = emailAddress }
    } |> fromVCtx
```

That's it! We use `refuteWith` along with the `isRequired` validation helper to transform a type from `'T option` to `'T` or fail the validation.
Then we utilize the smart constructor of our primitive types and pass the result to `Proof.mapInvalid`.
This function takes the errors from the `Invalid` constructor of the `Proof` type and maps them to a new type.
In this case, we encapsulate the failures in the `NewUserFailure` type.

But what about the `name` field? We can't use `isRequired` because it's an optional field.
Furthermore, we can't use `refuteWithProof` since the field has the `string option` type, and `mkName` requires a `string`.
We'll need to use the `optional` operator.

### The `optional` Operator

The `optional` operator is used with values of type `'A option`.
It takes a function with the signature `'A -> VCtx<'F, <ValueCtx<'B'>>>`.
In other words, it unwraps the `'A option`.
If the value is `Some`, the operator passes the unwrapped value to a validation function.
Otherwise, it ignores the value and allows validation to continue.
The result is that the value held by the `VCtx` changes from a `VCtx<'F, 'A option>` to a `VCtx<'F, 'B option>`.

Let's see it in action.

```fsharp
let! name = validation {
    withField (fun () -> this.Name)
    optional (fun v -> validation {
        withValue v
        refuteWithProof (mkName >> Proof.mapInvalid InvalidName)
    })
    qed
}
```

With the `optional` operator, all of our fields are validated.
Now, we need to check if the username matches the user's name.
We can accomplish this with a global validation.

### Global Validation

Global validations are similar to what we did with our primitive types.
We can perform them using the `withValue` operator.

```fsharp
module Example.Types

...

let makeNewUser(vm: NewUserVM) = 
    validation {
        let! name = validation {
            withField (fun () -> vm.Name)
            optional (fun v -> validation {
                withValue v
                refuteWithProof (mkName >> Proof.mapInvalid InvalidEmailAddress)
            })
            qed
        }
        and! username = validation {
            withField (fun () -> vm.Username)
            refuteWith (isRequired RequiredField)
            refuteWithProof (mkUsername >> Proof.mapInvalid InvalidUsername)
            qed
        }
        and! password = validation {
            withField (fun () -> vm.Password)
            refuteWith (isRequired RequiredField)
            refuteWithProof (mkPassword >> Proof.mapInvalid InvalidPassword)
            qed
        }
        and! emailAddress = validation {
            withField (fun () -> vm.EmailAddress)
            refuteWith (isRequired RequiredField)
            refuteWithProof (mkEmailAddress >> Proof.mapInvalid InvalidEmailAddress)
            qed
        }
        and! _ = validation {
            withValue vm
            disputeWithFact NameMatchesUsername (fun a -> a.Name <> a.Username)
            qed
        }
        return { NewUser.Name = name; Username = username; Password = password; EmailAddress = emailAddress }
    } |> fromVCtx
```

In this validation block, we include the global validation as part of the `let!` chain.
However, we can ignore the result since it is denoted with `_`.
With this implementation, our complex type is validated.
However, our example is relatively simple when it comes to complex types.
Let's now explore how to validate a type nested inside another type.

### Validating Nested Types

Let's expand our model to include the ability for the user to choose their preferred contact method.
We'll introduce new types to handle this.

```fsharp
module Example.Types

...

// The validated contact type (the model)
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

// The unvalidated contact type (the view model)
type ContactVM =
    { ContactType: ContactType option
      ContactDetails: string option }

module ContactVM =
    let makeContact(vm: ContactVM) =
        validation {
            let! typ = validation {
                withField (fun () -> vm.ContactType)
                refuteWith (isRequired MissingContactType)
                qed
            }
            and! details = validation {
                withField (fun () -> vm.ContactDetails)
                refuteWith (isRequired MissingContactDetails)
                qed
            }
            let! result =
                match typ with
                | ContactType.Call -> validation {
                        withField (fun () -> vm.ContactDetails) details
                        refuteWithProof (PhoneNumber.make >> Proof.mapInvalid InvalidPhoneNumber)
                        qed (fun pn -> Contact.Call pn)
                    }
                | ContactType.Text -> validation {
                        withField (fun () -> vm.ContactDetails) details
                        refuteWithProof (PhoneNumber.make >> Proof.mapInvalid InvalidPhoneNumber)
                        qed (fun pn -> Contact.Text pn)
                    }
                | ContactType.Email -> validation {
                        withField (fun () -> vm.ContactDetails) details
                        refuteWithProof (EmailAddress.make >> Proof.mapInvalid InvalidEmailAddress)
                        qed (fun ea -> Contact.Email ea)
                    }
            return result
        } |> fromVCtx
```

In this updated code, we introduce the `Contact` type as a discriminated union representing different contact methods and their associated contact details.
We also define a `ContactFailure` discriminated union type to represent the possible validation failures for the `Contact` type.
Additionally, we have a `ContactType` discriminated union that represents the available contact methods.
Lastly, we define the `ContactVM` type as the unvalidated view model for the contact information.

The `makeContact` function in the `ContactVM` module performs the validation for the contact information.
It checks that both the contact type and contact details are present, and then performs additional validation based on the chosen contact type.
The validation for phone numbers and email addresses is delegated to the respective primitive types (`PhoneNumber.make` and `EmailAddress.make`).

Now, let's update our `NewUser` types to include the new `Contact` field.

```fsharp
// The validated new user type (the model)
type NewUser = private { 
    name: Name option
    username: Username
    password: Password
    contact: Contact 
} with
member public this.Name = this.name
member public this.Username = this.username
member public this.Password = this.password
member public this.Contact = this.contact

// The unvalidated new user type (the view model)
type NewUserVM =
    { Name: string option
      Username: string option
      Password: string option
      Contact: ContactVM option }

module NewUserVM =
    let makeNewUser(vm: NewUserVM) = 
        validation {
            // ... nothing new here
            and! contact = validation {
                withField (fun () -> vm.Contact)
                refuteWith (isRequired RequiredField)
                refuteWithProof (ContactVM.makeContact >> Proof.mapInvalid InvalidContact)
                qed
            }
            // ... nothing new here
            return { NewUser.name = name; username = username; password = password; contact = contact; }
        } |> fromVCtx
```

In the `makeNewUser` function, we include the validation for the `Contact` field.
It follows a similar pattern as the other fields, using the `withField` operator to specify the field to be validated.
We then apply the `refuteWith` operator to check if the field is a required field.
Finally, we use `refuteWithProof` and `ContactVM.makeContact` to perform the validation for the contact information, mapping any invalid results to the `InvalidContact` failure type.

With these updates, our `NewUser` type now includes the validated contact information.
We've successfully validated nested types! However, we haven't covered validation for lists or other collections yet.
Let's explore that next.

### Validating Collections

To allow users to list multiple contact options and select their preferred one, we can update our `NewUser` models to include the `preferredContact` and `additionalContacts` fields.
Let's explore how to validate collections in our models.

```fsharp
module Example.Types

...

// The validated new user type (the model)
type NewUser = private { 
    name: Name option
    username: Username
    password: Password
    preferredContact: Contact // we renamed the `contact` field
    additionalContacts: Contact list 
} with
    member public this.Name = this.name
    member public this.Username = this.username
    member public this.Password = this.password
    member public this.PreferredContact = this.preferredContact
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
      PreferredContact: ContactVM option
      AdditionalContacts: ContactVM list }

module NewUserVM =
    let makeNewUser(vm:NewUserVM) = 
        validation {
            // ... nothing new here
            and! preferredContact = validation {
                withField (fun () -> vm.PreferredContact)
                refuteWith (isRequired RequiredField)
                refuteWithProof (ContactVM.makeContact >> Proof.mapInvalid InvalidContact)
                qed
            }
            and! additionalContacts = validation {
                withField (fun () -> vm.AdditionalContacts)
                refuteEachWithProof (ContactVM.makeContact >> Proof.mapInvalid InvalidContact)
                qed List.ofSeq
            }
            // ... nothing new here
            return { NewUser.name = name; username = username; password = password; preferredContact = preferredContact; additionalContacts = additionalContacts }
        } |> fromVCtx
```

In this updated code, we added the `AdditionalContacts` field to our `NewUser` model, which is now a list of contacts.
To validate the list, we use the existing validation logic and introduce the `refuteEachWithProof` operator.
This operator accepts a validation function that returns a `Proof` type.
Each element of the list is passed to the validation function, and any errors are added to the list of field-level failures with the index of the element.

### Serializing the Proof Type

The `Proof` type includes a `JsonConverter` converter written for it using `System.Text.Json`.
This converter serializes all global failures in an array under the `failures` property.
Field failures are serialized in a hash map under the `fields` property.
The keys in the hash map are created using the names of the validated fields using the `withField` operator.
Each failure is serialized using the `ToString` method.
Here's an example of what the serialized JSON might look like:

```json
{
    "failures": ["Name matches username."],
    "fields": {
        "username": ["Username cannot be empty."],
        "preferredContact.contactDetails": ["The phone number is invalid."],
        "additionalContacts.[0].contactDetails": ["The email address is invalid."]
    }
}
```

In the example above, the `failures` array contains global failures, while the `fields` object contains field failures.
Each key in the `fields` object represents a field in the model, and the associated value is an array of failures for that field.
Field names with nested properties are represented using dot notation.
The index `[0]` is used to indicate the element at index 0 in the `additionalContacts` list.

## Validation Operations

### `refute*` Operations

In type-safe validation, we often need to transform types as they are validated.
The `refute*` operations provide a convenient way to perform such transformations during the validation process.


#### `refute`

The simplest operation is `refute`.
It accepts a validation failure and immediately ends the validation process.
Any additional validation operations that come after `refute` will not be processed.

```fsharp
validation {
    ...
    refute MyValidationFailure
    ...
}
```

#### `refuteMany`

The `refuteMany` operation is similar to `refute`, but it accepts multiple failures as a `NonEmptyList`.

```fsharp
validation {
    ...
    refuteMany (NonEmptyList.ofList [FirstFailure; AnotherFailure; MyOtherValidationFailure; MyValidationFailure])
    ...
}
```

#### `refuteWith`

The `refuteWith` operation takes a function with the signature `'A -> Result<'B, 'F>` where `'A` is the value being validated.
The function either transforms the value into a different type or returns an error.
If the result is `Error 'F`, the failure is added to the result, and validation ends.
If the result is `Ok 'B`, validation continues with the new type.

```fsharp
validation {
    withValue (Some "my string")
    ...
    // value is of type `string option` here
    refuteWith (isRequired RequiredField)
    // value is of type `string` here
    ...
}
```

#### `refuteWithMany`

The `refuteWithMany` operation takes a function with the signature `'A -> Result<'B, NonEmptyList<'F>>` where `'A` is the value being validated.
The function either transforms the value into a different type or returns an error.
If the result is `Error fs`, the failures are added to the result, and validation ends.
If the result is `Ok b`, validation continues with the new type.

```fsharp
validation {
    withValue "my string"
    ...
    refuteWithMany (fun s -> 
        if s = "bad string" 
        then Error (NonEmptyList.singleton BadString)
        else Ok (GoodString s)
    )
    ...
}
```

#### `refuteWithProof`

The `refuteWithProof` operation takes a function with the signature `'A -> Proof<'F, 'B>` where `'A` is the value being validated.
This operation is useful when a type's validations are already defined elsewhere.
If the result is `Invalid`, the failures are added to the result, and validation ends.
If the result is `Valid 'B`, validation continues with the new type.

```fsharp
validation {
    withValue (Some "validemail@example.net")
    ...
    // value is of type `string option` here
    refuteWithProof mkEmailAddress
    // value is of type `EmailAddress` here
    ...
}
```

#### `refuteEachWith`

The `refuteEachWith` operation is similar to `refuteWith`, but it is used for validating list-like types.

```fsharp
validation {
    withValue [Some "my string"; None]
    ...
    refuteEachWith (isRequired RequiredField)
    ...
}
```

#### `refuteEachWithProof`

The `refuteEachWithProof` operation is similar to `refuteWithProof`, but it is used for validating list-like types.

```fsharp
validation {
    withValue ["my string"; "validemail@example.net"]
    ...
    refuteEachWithProof mkEmailAddress
    ...
}
```

### `dispute*` Operations

To collect as many validation failures as possible before ending validation, the `dispute*` operations are available.

#### `dispute`

The `dispute` operation is the simplest one.
It accepts a validation failure and adds it to the result before executing the next validation.

```fsharp
validation {
    ...
    dispute MyValidationFailure
    ...
}
```

#### `disputeMany`

The `disputeMany` operation is similar to `dispute`, but it accepts multiple failures.

```fsharp
validation {
    ...
    disputeMany [MyValidationFailure; MyOtherValidationFailure]
    ...
}
```

#### `disputeWith`

The `disputeWith` operation takes a function with the signature `'A -> 'F option`, where `'A` is the value being validated.
The function either returns `None` or a validation failure.
If the result is `Some f`, the failure is added to the result before the next validation is executed.

```fsharp
validation {
    withValue (Some "my string")
    ...
    // value is of type `string option` here
    disputeWith (fun a -> 
        if a = "my invalid string" 
        then Some InvalidString 
        else None
    )
    // value is of type `string` here
    ...
}
```

#### `disputeWithMany`

The `disputeWithMany` operation takes a function with the signature `'A -> 'F list`, where `'A` is the value being validated.
The function returns a list of failures.
If the result contains one or more elements, the failures are added to the result, and validation continues.
Otherwise, validation continues without adding any failures to the result.

```fsharp
validation {
    withValue "my string"
    ...
    disputeWithMany (fun s -> 
        if s = "bad string" 
        then [BadString]
        else []
    )
    ...
}
```

#### `disputeWithFact`

The `disputeWithFact` operation takes a failure value and a function with the signature `'A -> bool`, where `'A` is the value being validated.
If the result of the function is `false`, the failure value is added to the result before the next validation is executed.
Otherwise, the validation proceeds normally.

```fsharp
validation {
    withValue (Some "validemail@example.net")
    ...
    // value is of type `string option` here
    disputeWithFact Empty isNotNull
    // value is of type `EmailAddress` here
    ...
}
```

*Note: The `isNotNull` function is explained in the [Validation Helpers](#validation-helpers) section.*

#### `disputeAnyWith`

The `disputeAnyWith` operation is similar to `disputeWith`, but it is used for validating list-like types.
If any of the elements fail validation, the entire list fails.

```fsharp
validation {
    withValue ["my string"; ""]
    ...
    disputeAnyWith (fun s ->
        if s = ""
        then Some Empty
        else None
    ...
}
```

There is an overload of the operator that takes a function with the signature `int -> 'A -> 'F option`, where the first parameter is the index of the element.

#### `disputeAllWith`

The `disputeAllWith` operation is similar to `disputeWith`, but it is used for validating list-like types.
If every element fails validation, the entire list fails.
Otherwise, no failures are added to the result.

```fsharp
validation {
    withValue ["my string"; ""]
    ...
    disputeAllWith (fun s ->
        if s = ""
        then Some Empty
        else None
    ...
}
```

There is an overload of the operator that takes a function with the signature `int -> 'A -> 'F option`, where the first parameter is the index of the element.

#### `disputeAnyWithMany`

The `disputeAnyWithMany` operation is similar to `disputeWithMany`, but it is used for validating list-like types.
If any of the elements fail validation, the entire list fails.

```fsharp
validation {
    withValue ["my string"; ""]
    ...
    disputeAnyWithMany (fun s ->
        if s = ""
        then [Empty]
        else []
    ...
}
```

There is an overload of the operator that takes a function with the signature `int -> 'A -> 'F list`, where the first parameter is the index of the element.

#### `disputeAllWithMany`

The `disputeAllWithMany` operation is similar to `disputeWithMany`, but it is used for validating list-like types.
If every element fails validation, the entire list fails.
Otherwise, no failures are added to the result.

```fsharp
validation {
    withValue ["my string"; ""]
    ...
    disputeAllWith (fun s ->
        if s = ""
        then [RequiredField]
        else []
    ...
}
```

There is an overload of the operator that takes a function with the signature `int -> 'A -> 'F list`, where the first parameter is the index of the element.

#### `disputeAnyWithFact`

The `disputeAnyWithFact` operation is similar to `disputeWithFact`, but it is used for validating list-like types.
If any of the elements fail validation, the entire list fails.

```fsharp
validation {
    withValue ["my string"; ""]
    ...
    disputeAnyWithFact Empty isNotNull
    ...
}
```

There is an overload of the operator that takes a function with the signature `int -> 'A -> bool`, where the first parameter is the index of the element.

#### `disputeAllWithFact`

The `disputeAllWithFact` operation is similar to `disputeWithFact`, but it is used for validating list-like types.
If every element fails validation, the entire list fails.
Otherwise, no failures are added to the result.

```fsharp
validation {
    withValue ["my string"; ""]
    ...
    disputeAllWithFact Empty isNotNull
    ...
}
```

There is an overload of the operator that takes a function with the signature `int -> 'A -> 'F list`, where the first parameter is the index of the element.

#### `validateEach`

This operation accepts a function with the signature `'A -> VCtx<'F, 'B>` that validates each element.
The result is created from the `validation` computation expression.

```fsharp
validation {
    withValue [Some "my string"; None]
    ...
    validateEach (fun a -> validation { withValue a; ...; qed; })
    ...
}
```

## Validation Helpers

The following are helper functions that can be used with the `refute*` and `dispute*` operations to perform common validation checks.

### The `isRequired` Helper

This function is used with the `refute*` family of validation operations.
It transforms a value of type `'T option` to `'T` or adds the given validation failure to the result.

### The `isRequiredWhen` Helper

This function is used with the `dispute*` family of validation operations.
The boolean parameter determines if the required check should be executed.
If the boolean parameter is `true`, the helper checks that a value of type `'T option` is `Some 'T` or adds the given failure to the result.

### The `isRequiredUnless` Helper

This function is the same as `isRequiredWhen`, except the boolean value must be `false` for the check to occur.

### The `isOk` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a value of type `Result<'A, 'F>` is an `Ok 'A`.

### The `isError` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a value of type `Result<'A, 'F>` is an `Error 'F`.

### The `isNull` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a list-like value is empty.

### The `isNotNull` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a list-like value is not empty.

### The `minLength` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a list-like value has at least the given number of elements.

### The `maxLength` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a list-like value has no more than the given number of elements.

### The `isLength` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a list-like value has exactly the given number of elements.

### The `hasElem` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a list-like value has an element equal to another value.

### The `doesNotHaveElem` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a list-like value does not have an element equal to another value.

### The `isEqual` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a value is equal to another value using the `(=)` operator.

### The `isNotEqual` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a value is not equal to another value using the `(=)` operator.

### The `isLessThan` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a value is less than another value.

### The `isGreaterThan` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a value is greater than another value.

### The `isLessThanOrEqual` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a value is less than or equal to another value.

### The `isGreaterThanOrEqual` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a value is greater than or equal to another value.

### The `isValid` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a `Proof<'F, 'A>` is `Valid`.

### The `isInvalid` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a `Proof<'F, 'A>` is `Invalid`.

### The `flattenProofs` Helper

This function accepts a `Proof<'F, 'A> list` and transforms it into a `Proof<'F, 'A list>`.

### The `raiseIfInvalid` Helper

This function accepts a `Proof<'F, 'A>`.
If the value is `Valid`, it is transformed to `'A`.
Otherwise, an `InvalidProofException` is raised with the given message. 
This is useful when we are receiving data that you know to be valid, such as from a database, and know that validation will succeed.

## Proof Helpers

The following are helper functions for the `Proof` type that make it easier to work with.

### `toResult` Helper

The `toResult` helper converts a `Proof<'F,'A>` value to a `Result<'A,ValidationFailures<'F>>`.

### `toValidationFailures` Helper

If you are only interested in the failures, you can use the `toValidationFailures` function to convert a `Proof<'F,'A>` to an `Option<ValidationFailures<'F>>`.

## `IValidateable` Interface

The `IValidateable` interface defines a contract for types that can be validated using the validation library. It includes an abstract `Validate` member function that performs the validation and returns a `Proof` object representing the validation result.

```fsharp
type IValidateable<'T> =
    abstract member Validate : unit -> Proof<ValidationFailures, 'T>
```

In this interface, `'T` represents the type being validated. The `Validate` member function takes no parameters and returns a `Proof<ValidationFailures, 'T>` object that captures the validation results.

To make a specific type `'T` implement the `IValidateable` interface, you would need to define the `Validate` member function for that type according to your validation logic. The `Validate` function should perform the necessary validations on an instance of type `'T` and return a `Proof<ValidationFailures, 'T>` object representing the validation result.

### Example Usage

Let's see an example of how to use the `IValidateable` interface with the `NewUser` type from the previous examples. We'll assume that the `NewUser` type has been defined to implement the `IValidateable` interface.

```fsharp
type NewUser =
    { Name: string option
      Username: string option
      Password: string option
      Contact: Contact option } 
    interface IValidateable<NewUser> with
        member this.Validate () =
            validation {
                let! name = validation {
                    withField (fun () -> this.Name)
                    optional (fun v -> validation {
                        withValue v
                        refuteWithProof (mkName >> Proof.mapInvalid InvalidName)
                    })
                    qed
                }
                and! username = validation {
                    withField (fun () -> this.Username)
                    refuteWith (isRequired RequiredField)
                    refuteWithProof (mkUsername >> Proof.mapInvalid InvalidUsername)
                }
                and! password = validation {
                    withField (fun () -> this.Password)
                    refuteWith (isRequired RequiredField)
                    refuteWithProof (mkPassword >> Proof.mapInvalid InvalidPassword)
                }
                and! contact = validation {
                    withField (fun () -> this.Contact)
                    refuteWith (isRequired RequiredField)
                    refuteWithProof (ContactVM.makeContact >> Proof.mapInvalid InvalidContact)
                }
                return { NewUser.Name = name; Username = username; Password = password; Contact = contact }
            } |> fromVCtx
```

In this example, the `NewUser` type implements the `IValidateable<NewUser>` interface. The `Validate` member function is defined to perform the validation logic for the `NewUser` type. The validation process follows the same pattern as shown in previous examples, using the validation computation expression to validate each field and return the validated `NewUser` object.

By implementing the `IValidateable` interface, the `NewUser` type provides a consistent way to perform validations and obtain the validation result using the `Validate` method.

## Haskell Data-Validation Library

This library is based on our original library for [Haskell](https://www.haskell.org/).
 - Learn more about this library on Hackage: https://hackage.haskell.org/package/data-validation-0.1.2.5
  - Read the documentation on Hackage: https://hackage.haskell.org/package/data-validation-0.1.2.5/docs/Data-Validation.html
  - Visit the repository: https://github.com/alasconnect/data-validation

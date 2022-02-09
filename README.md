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
  - [Serializing The Proof Type](#serializing-the-proof-type)
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
- [Data-Validation Library for Haskell](#data-validation-library-for-haskell)

## Getting Started

*The code for these examples can be found [here](samples/GettingStarted/).*

This library is intended to accomplish 2 goals.
First, it should be impossible for your code to consume unvalidated data.
We accomplish this by transforming types through validation.
Second, it should be easy to build validations.
This library provides several types, functions, and other tools to build these validations in a consistent manner.

A core concept of functional programming is that it should be impossible to represent invalid states in your application.
This can reduce bugs and unexpected behavior in your program but it does require a proper implementation.
One aspect of this is how the application's types are implemented.
Properly implemented types should not allow invalid states to exist.
Any attempt to create an invalid state should result in a compile time error.

The validation problem is one that can clearly benefit from such a concept.
One of the most significant core concepts of this library is that validation should transform a type once it has been validated.
That way it is impossible to pass invalid data into a function that is not expecting it.

This is easiest to explain with an example.
Let's write some code that takes an email address and sends an email.
We won't actually implement the function, we are more concerned with email address parameter.

```fsharp
module Example

let notifyUser (emailAddress:string) =
    // send email
```

This works.
The only problem is that you could pass any string to it.
The type of the parameter doesn't restrict you at all.
You could apply the function like this:

```fsharp
notifyUser "Not an email address"
```

And the compiler would let it happen.
In F#, we really want to build our program so that it is impossible to introduce this kind of bug.
Really, the function application above should have resulted in a compiler error.
Let's see if we can fix that.

### Validating Primitive Types

The first step is to define a new type in a separate types module.
This is a common pattern in functional languages.

```fsharp
module Example.Types

type EmailAddress = private EmailAddress of string
```

Notice that we made the constructor private.
This makes it so that we can only construct the type in the Types module.
This is especially useful when combined with smart constructors which allow us to perform some logic before constructing the type.

```fsharp
module Example.Types

...

module EmailAddress =
    let make (str:string): ReturnType?? = 
    // validate
```

But what kind of return type do we want?

### The Proof Type

The result of a validation function needs to meet several requirements.

 1. It must clearly express the result of the validation
 1. It must hold all of the failures that occurred during the validation
 1. For complex types, it should also express what fields failed

The first requirement is met by the `Result<'T, 'E>` type.
Meeting the other requirements would require another type to wrap `'T` so we can accumulate failures without losing the value.
We don't want to specify `Result<SomeWrapper<'T>, 'E>` every time.
Let's come up with something easier to use that's more idiomatic.

```fsharp
type Proof<'F, 'T> = 
    | Valid of 'T
    | Invalid of 'F list * Map<string, 'F list>
```

The `Invalid` constructor takes a list of type level failures and a map of all the field level failures.
The keys of this map are the names of the fields that failed.
Of course, the real `Proof<'F, 'T>` type is a bit more complicated.
The keys of the field level failures are actually lists of `Name` so we can support nested validations.
More on that later.

Let's update our smart constructor with this type:

```fsharp
module Example.Types

open FSharp.Data.Validation

...

    let make (str:string): Proof<'F??, EmailAddress> = 
        // validation
```

So, what type should we use for `'F`?

### Failures Are Types Too

Notice that when we discuss validation failures, we do not call them errors.
Error implies some undesirable or unexpected behavior.
Validation issues are expected and our code should be able to handle them smoothly by returning a meaningful result to the user.
Therefore, validation issues are not errors.
But how do we represent failures?

With types of course!

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

It is important that there be one failure type for each data type you want to validate.
We'll talk about how to compose them later.
That way, we can handle the individual failure cases without having to write catch all `match` expressions to handle the cases we don't care about.
Especially because catch all `match` expressions can lead to code that is not type safe.

Imagine adding a new case that you want to handle.
If you don't have a catch all pattern, the compiler will tell you what parts of the program need to be updated.
If you have catch all patterns, you have to search for every match expression by hand (yuk!).

Okay, so how do we actually validate our email address string?

### The Validation Computation Expression

Computation expressions are a very useful feature in F#.
They make it easy to write a domain specific language (DSL) for important tasks in your code.
You should be able to recognize the syntax from `query` and  `async` expressions.
Let's look at some code.

```fsharp
module Example.Types

...
    
    let make (str:string): Proof<EmailAddressValidationFailure, EmailAddress> = 
        validation {
            // validation stuff
        } |> fromVCtx
```

The first thing to notice is the `validation` computation expression.
It uses the typical syntax for computation expressions.
All of the validation logic should be contained within the expression.

At the end of the expression, the `fromVCtx` function is applied to the result of the computation expression.
This is because the computation expression uses the `VCtx<'F, 'A>` type in the background.
This type holds the value of the type being validated and all of the validation failures.
`fromVCtx` converts the `VCtx` type to the `Proof` type.

We can't use the `Proof` type inside the expression because it only has two states, `Valid` and `Invalid`.
`VCtx` has an additional state that lets us track the value and the failures at the same time.
This is needed so the computation expression can handle validations that are performed after other validations have already failed.
For instance, if a password fails validation because it does not have a number character, we can still check to see if it meets the length requirement.
In order to do that, we need to track the password's value and the failed validations.

Let's move on to the next part of our validation example.
We need to tell the computation expression what we are validating.

```fsharp
module Example.Types

...
    
    let make (str:string): Proof<EmailAddressValidationFailure, EmailAddress> = 
        validation {
            withValue str
            // validation stuff
            qed EmailAddress
        } |> fromVCtx
```

### `withValue`, `withField`, `qed`, and `ValueCtx`

When we validate a complex type, we usually need to apply specific validations to each field.
When we validate a primitive value, we just validate the value itself.
We need to tell the `validation` computation expression when we are validating a value and when we are validating a field.

Any value level validation failures are added to the global failures list in the `Proof` type.
Field level failures are added to the field failure map.
This allows the consumers of the validation failures to see exactly which fields failed and why.

In the background, the computation expression uses the `ValueCtx<'A>` type.
This type holds the value that is being validated and, in the case of field validations, the name of the field being validated.
You should never have to work with the `ValueCtx` type directly.

When validating a value, we use the `withValue` operation by passing in the value to validate.
For fields, we use the `withField` operation and pass in the field's `Name` and value.
The `Name` type can be constructed by passing a string to the `mkName` function.

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

However, `withField` has an overload that allows you to pass a selector function.
The selector is used to determine the fields name and value.

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

There are 2 overloads to the `qed` operation.
The one with no parameters simply unwraps the value from the `ValueCtx`.
This is very useful when the validation transforms the unvalidated type into the validate type during validation.
We will see this more when we look at validating complex types.

The second overload for the `qed` operation accepts a function.
This function transforms the unvalidated type into the new type.
In our example above, we pass the `EmailAddress` constructor into the `qed` function to wrap the string in the `EmailAddress` type.

```fsharp
validation {
    withValue str
    // validation stuff
    qed EmailAddress
} |> fromVCtx
```

Now that we have all of the machinery in place, let's validate our email address string.
We could do this with a regular expression but that wouldn't demonstrate the library very well.
Let's do it by hand with the `refute*` and `dispute*` operations!

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

There are 2 key differences between the `dispute*` operations and the `refute*` operations.

1. Refuting a value stops further validation, disputing does not
1. Refuting a value lets you transform it, disputing does not

Imagine you are validating a password string with the type `string option`.
The password field has specific requirements: it is required, must be at least 8 characters long, and contain both letters and numbers.
That sounds like a string that needs some validation!

If the string has the value `Some "mypass"`, we would expect it to pass some checks but not others.
For instance, it would pass the checks for a required value and it contains letters.
However, it would fail the check for minimum length and numbers.

Let's say your validation logic looked something like this:

```fsharp
validation {
    // check that value exists
    // check length
    // check for letters
    // check for numbers
} |> fromVCtx
```

In this case, would the check for numbers ever run for our value?
It should.
We want it to check for letters and numbers even if it does not have the correct length.
We want to know about as many validation failures as possible.

That's why we have the `dispute*` operations.
If one validation fails, it continues to check the other validations.
We can use a dispute operation to check the length, letters, and number.
However, we can't use them to check if the value exists.
This is because `dispute*` operations cannot transform values.

So far, we have only discussed if our password `string option` has a value.
What if the value is `None`?
Can we do any validation after that?
No, because it's the wrong type.
If we want to continue validation, we need to transform our `string option` into a `string` and we can't do that if we don't have a value.

That's why we have the `refute*` operations.
Refute operations will attempt to transform a value as part of the validation process.
If the value cannot be validated, it cannot be transformed.
If the value cannot be transformed, no further checks can be made.

It is good to perform as many checks as possible when performing validation.
But you cannot check a value that's the wrong type.
It is also good to transform values into different types when performing validation.
However, you can't perform any more validation on a type that can't be transformed.
That's why we need both `dispute*` and `refute*` operations.

### Back to the Example

Now that we understand the difference between `dispute*` and `refute*`, let's break our example down.
The `refuteWith` operation takes a function with the signature `'A -> Result<'F, 'B>`.
This function checks if a value is suitable for transformation from `'A` to `'B`.
If so, it performs the transformation and returns it.
Otherwise, it returns the failure.

If the check passes, the value returned is used for further validation.
If the check fails, the failure is added to the result and validation ends.

`disputeWithFact` takes a value of the failure type and a check function that returns a `bool`.
If the check returns `false` the passed in failure is added to the result and validation continues.
Otherwise, validation continues without adding any failures to the result.

Here is the code above with some additional clarification.

```fsharp
module Example.Types

...
    
    let make (str:string): Proof<EmailAddressValidationFailure, EmailAddress> = 
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
            disputeWithFact MissingUsername (fun ss -> isNotNull ss[0])
            disputeWithFact MissingDomain (fun ss -> isNotNull ss[1])
            // the `string[]` returned above is passed in to the function here and transformed into an `EmailAddress`
            qed (fun ss -> EmailAddress (sprintf "%s@%s" ss[0] ss[1]))
        } |> fromVCtx
```

Now that we have our validation function, let's revisit our original `notifyUser` function.

```fsharp
module Example

let notifyUser (emailAddress:string) =
    // send email
```

All we need to change here is the type of the `emailAddress` parameter.

```fsharp
module Example

let notifyUser (emailAddress:EmailAddress) =
    // send email
```

Done!
Now our code will only compile if we pass a valid email address to the `notifyUser` function.
Now let's look at...

### Validating Complex Types

Let's say we have a form to allow new users to sign up on our website.
The data from that form is sent to a REST endpoint which processes the data.
We want to accept a name, username, email address, and password.
All of the fields will be required except for the name.
For some added complexity, we also want to make sure the username does not equal the user's actual name (because, security!).
Of course we will need to validate them but first we need a type to model the data.
Actually, we will need 2 models.

```fsharp
module Example.Types

// primitive types and smart constructors

// The validated new user type (the model)
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

// The unvalidated new user type (the view model)
type NewUserVM =
    { Name: string option
      Username: string option
      Password: string option
      EmailAddress: string option }
```

We need 2 models here because type safe validation requires type transformation.
We accept unvalidated data and transform it to the validated type by performing the validation.
The unvalidated type we call a "view model" while the validated type is a "model".

Like our primitive types, we marked the constructor for the validated type as private.
With F# records, this means that the fields are not visible to any module outside of the declaring module.
Therefore, we need to define public accessors so the data can be read.

Also notice that we use optional values for every field in the view model.
This is because we want to accept the data in its simplest state, the one that makes the least assumptions.
If we just used a `string`, we would be assuming that a value must exist.

Now that we have our types, let's define a smart constructor for the model.
This smart constructor will accept the view model as a parameter, validate it, and return the model type.
For complex types, we typically define the smart constructor as a function in a module named after the view model.
This is just for consistency with the primitive types.
Then we take the view model as a parameter.

```fsharp
module Example.Types

...

// The unvalidated new user type (the view model)
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
            // validate that the username does not equal the user's name
            // return the model type
        } |> fromVCtx
```

The nested `validation` blocks may look familiar from nested `async` computation expressions.
However, there is some new syntax here that we need to introduce.

### The `let!` Operator

The `let!` operator let's us perform validation on individual fields of the view model.
Once the validation is done, the `let!` operator unwraps the `VCtx` type and allows you to access the underlying, validated, value.
The validated value will be available for additional checks or calls to the model's constructor.

However, if the validation is refuted, the entire computation expression ends.
This could be a problem for records with multiple fields because we want to validate all of the fields even if one of them fails.
This is important as we want to record as many failures as possible before ending the validation.

```fsharp
module Example.Types

...

    let makeNewUser(vm:NewUserVM) = 
        validation {
            let! name = validation {
                // if this validation is refuted
            }
            let! username = validation {
                // this validation will never run
            }
            // validate additional fields
            // validate that the username does not equal the user's name
            // return the model type
        } |> fromVCtx
```

That's where `and!` comes in.

### The `and!` Operator

The `and!` operator does the same thing as the `let!` operator except that it forces the computation expression to evaluate all of the `and!`s and the `let!` expression.
The `let!` and `and!` operators form a chain that begins with the `let!` and ends with the last `and!`.
At the end of the chain, the computation expression combines the results of all the branches into a single `VCtx` value.
That way, you can be certain that all of the fields were checked even if the first one in the code block is refuted.

Let's look at our example again using the `and!` operator.


```fsharp
module Example.Types

...

    let makeNewUser(vm:NewUserVM) =  
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

However, there are a couple of things to keep in mind.
You cannot access any value assigned by the operators until the chain is complete.

```fsharp
module Example.Types

...

            let! name = validation {
                // validate the name field
            }
            and! username = validation {
                printf "%s" name // this will fail to compile because the `name` variable is not accessible yet
                // perform additional validation
            }
...

```

In addition, if any validation occurs after the chain and the chain is refuted, the additional validation will not be executed.

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

            // this chain will never run
            let! password = validation {
                ...
            }
            and! emailAddress = validation {
                ...
            }

...

```

Typically, this isn't an issue.
Just be sure to include as many checks as possible in a validation chain.

### The `return` Operator

We have already seen the `withField` and `qed` operators.
Let's include them in our example.
Let's also return a value with the validated fields.

```fsharp
module Example.Types

...

    let makeNewUser(vm:NewUserVM) =  
        validation {
            let! name = validation {
                withField (fun () -> this.Name)
                // validate name
                qed
            }
            and! username = validation {
                withField (fun () -> this.Username)
                // validate username
                qed
            }
            and! password = validation {
                withField (fun () -> this.Password)
                // validate password
                qed
            }
            and! emailAddress = validation {
                withField (fun () -> this.EmailAddress)
                // validate email address
                qed
            }
            // validate that the username does not equal the user's name
            return { NewUser.Name = name; Username = username; Password = password; EmailAddress = emailAddress; }
        } |> fromVCtx
```

We use the `return` operator to wrap the value to the right in a valid `VCtx`.
Because this is the last line of the validation computation expression, it becomes the result of the expression.
Noticed that the fields of the model are set using the variables bound by the `let!` and `and!` operators.
The variables already have the correct types because of their validation.

Speaking of validating fields.
Wouldn't it be nice if we could use the primitive smart constructors we already created to validate them?
Yes, it would.
But we need to be able to map the primitive failure types into the failure type for our `NewUser` type.

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

    let makeNewUser(vm:NewUserVM) = 
        validation {
            let! name = validation {
                withField (fun () -> this.Name)
                // how do we validate an optional field?
                qed
            }
            and! username = validation {
                withField (fun () -> this.Username)
                refuteWith (isRequired RequiredField)
                refuteWithProof (mkUsername >> Proof.mapInvalid InvalidUsername)
                qed
            }
            and! password = validation {
                withField (fun () -> this.Password)
                refuteWith (isRequired RequiredField)
                refuteWithProof (mkPassword >> Proof.mapInvalid InvalidPassword)
                qed
            }
            and! emailAddress = validation {
                withField (fun () -> this.EmailAddress)
                refuteWith (isRequired RequiredField)
                refuteWithProof (mkEmailAddress >> Proof.mapInvalid InvalidEmailAddress)
                qed
            }
            // validate that the username does not equal the user's name
            return { NewUser.name = name; username = username; password = password; emailAddress = emailAddress; }
        } |> fromVCtx
```

That's it.
`refuteWith` uses the `isRequired` validation helper to transform a type from `'T option` to `'T` or it fails validation.
Then we use the smart constructor of our primitive types and forward the result to `Proof.mapInvalid`.
The function takes the errors from the `Invalid` constructor of the `Proof` type and maps them to a new type.
In this case, we just wrap the failures in the `NewUserFailure` type.

But what about the name field.
We can't use `isRequired` because it's an optional field.
We, also, can't use `refuteWithProof` because the field has the `string option` type and `mkName` requires a `string`.
We will need to use the `optional` operator.

### The `optional` Operator

The `optional` operator works on values of type `'A option`.
It takes a function with the signature `'A -> VCtx<'F, <ValueCtx<'B'>>>`.
In other words, it unwraps the `'A option`.
If the value is `Some`, the operator unwraps the value and passes it to a validation function.
Otherwise, the operator ignores the value and allows validation to continue.
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

Now, all of our fields are validated.
We still need to check and see if the username and user's name are equal.
We can do that with a global validation.

### Global Validation

We have already seen global validations.
Its the same thing we did with our primitives.
We can do them with the `withValue` operator.


```fsharp
module Example.Types

...

    let makeNewUser(vm:NewUserVM) = 
        validation {
            let! name = validation {
                withField (fun () -> this.Name)
                optional (fun v -> validation {
                    withValue v
                    refuteWithProof (mkName >> Proof.mapInvalid InvalidEmailAddress)
                })
                qed
            }
            and! username = validation {
                withField (fun () -> this.Username)
                refuteWith (isRequired RequiredField)
                refuteWithProof (mkUsername >> Proof.mapInvalid InvalidUsername)
                qed
            }
            and! password = validation {
                withField (fun () -> this.Password)
                refuteWith (isRequired RequiredField)
                refuteWithProof (mkPassword >> Proof.mapInvalid InvalidPassword)
                qed
            }
            and! emailAddress = validation {
                withField (fun () -> this.EmailAddress)
                refuteWith (isRequired RequiredField)
                refuteWithProof (mkEmailAddress >> Proof.mapInvalid InvalidEmailAddress)
                qed
            }
            and! _ = validation {
                withValue this
                disputeWithFact NameMatchesUsername (fun a -> a.Name = a.Username |> not)
                qed
            }
            return { NewUser.name = name; username = username; password = password; emailAddress = emailAddress; }
        } |> fromVCtx
```

We need to include this in the `let!` chain but we can ignore the result.
Our complex type is validated.
However, as far as complex types go, ours is fairly simple
Let's try validating a type nested inside another type.

### Validating Nested Types

Right now, our model takes an email address.
But what if we wanted to let the user choose how we contact them?
Maybe we want to give them the option to be contacted by email, text message, or a phone call.
Let's add some new types.

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
    let makeContact(vm:ContactVM) =
        validation {
            let! typ = validation {
                withField (fun () -> vm.ContactType)
                refuteWith (isRequired MissingContactType)
                qed
            }
            and! d = validation {
                withField (fun () -> vm.ContactDetails)
                refuteWith (isRequired MissingContactDetails)
                qed
            }
            let! result =
                match typ with
                | ContactType.Call -> validation {
                        withField (fun () -> vm.ContactDetails) d
                        refuteWithProof (PhoneNumber.make >> Proof.mapInvalid InvalidPhoneNumber)
                        qed (fun pn -> Contact.Call pn)
                    }
                | ContactType.Text -> validation {
                        withField (fun () -> vm.ContactDetails) d
                        refuteWithProof (PhoneNumber.make >> Proof.mapInvalid InvalidPhoneNumber)
                        qed (fun pn -> Contact.Text pn)
                    }
                | ContactType.Email -> validation {
                        withField (fun () -> vm.ContactDetails) d
                        refuteWithProof (EmailAddress.make >> Proof.mapInvalid InvalidEmailAddress)
                        qed (fun pn -> Contact.Email pn)
                    }
            return result
        } |> fromVCtx
```

The `Contact` type is a discriminated union of the contact method and the address/phone number used for the message.
Because our endpoint will be taking in JSON, we have to build the `ContactVM` in a more object-oriented way.
We have an enum to represent the contact methods and a simple string field to represent the address/phone number.
We won't go into detail on the validation method here, but the implementation is included.
There is also a `PhoneNumber` primitive type but we're not going to cover that either.
Now we just need to update our `NewUser` types to include the new field.

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
    let makeNewUser(vm:NewUserVM) = 
        validation {
            // ... nothing new here
            and! contact = validation {
                withField (fun () -> this.Contact)
                refuteWith (isRequired RequiredField)
                refuteWithProof (ContactVM.makeContact >> Proof.mapInvalid InvalidContact)
                qed
            }
            // ... nothing new here
            return { NewUser.name = name; username = username; password = password; contact = contact; }
        } |> fromVCtx
```

The validation expression for the contact type should look pretty familiar.
It's exactly the same as validating the primitive types!
Great, but what about lists and other collections?

### Validating Collections

Let's say that we want users to be able to list many contact options and then select their preferred one.
That way we have several options to reach them if the primary method fails.
We already have the `Contact` type, so we just need to update our `NewUser` models.

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
                withField (fun () -> this.PreferredContact)
                refuteWith (isRequired RequiredField)
                refuteWithProof (ContactVM.makeContact >> Proof.mapInvalid InvalidContact)
                qed
            }
            and! additionalContacts = validation {
                withField (fun () -> this.AdditionalContacts)
                refuteEachWithProof (ContactVM.makeContact >> Proof.mapInvalid InvalidContact)
                qed List.ofSeq
            }
            // ... nothing new here
            return { NewUser.name = name; username = username; password = password; preferredContact = preferredContact; additionalContacts = additionalContacts }
        } |> fromVCtx
```

Here, we added a `AdditionalContacts` field which is a list of contacts.
To validate it, we used our existing validation logic and the `refuteEachWithProof` operator.
This operator accepts a validation function that returns a `Proof` type.
Each element of the list is passed to the validation function.
Any errors are added to the list of field level failures with the index of the element.

### Serializing The Proof Type

The `Proof` type has a `JsonConverter` converter written for it using `System.Text.Json`.
It will serialize all global failures in an array under the `failures` property.
All field failures are serialized in a hash map under the `fields` property.
The names of the keys in the hash map are created using the names of the validated field using the `withField` operator.
Each failure is serialized using the `ToString` method.
Here is an example of what it might look like.

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

## Validation Operations

### `refute*` Operations

We already mentioned that type safe validation should transform the types as they are validated.
The easiest way to do this is with the `refute*` operations.

#### `refute`

The simplest operation is `refute`.
It accepts a validation failure and immediately ends the validation process.
This means that any additional validation operations that come after `refute` may not be processed.

```fsharp
validation {
    ...
    refute MyValidationFailure
    ...
}
```

#### `refuteMany`

The `refuteMany` operation is similar to the `refute` operation but it accepts multiple failures as a `NonEmptyList`.

```fsharp
validation {
    ...
    refuteMany (FirstFailure >- AnotherFailure >- MyOtherValidationFailure >< MyValidationFailure)
    ...
}
```

#### `refuteWith`

The `refuteWith` operation takes a function with the signature `'A -> Result<'B, 'F>` where `'A` is the value being validated.
The function either transforms the value into a different type, or gives back an error.
If the result is `Error 'F`, the failure is added to the result and validation ends.
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
The function either transforms the value into a different type, or gives back an error.
If the result is `Error fs`, the failures are added to the result and validation ends.
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
This function is useful when a type's validations are already defined elsewhere.
If the result is `Invalid`, the failures are added to the result and validation ends.
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

Similar to `refuteWith` but used for validating list like types.

```fsharp
validation {
    withValue [Some "my string"; None]
    ...
    refuteEachWith (isRequired RequiredField)
    ...
}
```

#### `refuteEachWithProof`


Similar to `refuteWithProof` but used for validating list like types.

```fsharp
validation {
    withValue ["my string"; "validemail@example.net"]
    ...
    refuteEachWithProof mkEmailAddress
    ...
}
```

### `dispute*` Operations

It is always good to collect as many validation failures as possible before ending validation.
The easiest way to do this is with the `dispute*` operations.

#### `dispute`

The simplest operation is `dispute`.
It accepts a validation failure and adds it to the result before executing the next validation.

```fsharp
validation {
    ...
    dispute MyValidationFailure
    ...
}
```

#### `disputeMany`

The `disputeMany` operation is similar to the `dispute` operation but it accepts multiple failures.

```fsharp
validation {
    ...
    disputeMany (MyValidationFailure >< MyOtherValidationFailure)
    ...
}
```

#### `disputeWith`

The `disputeWith` operation takes a function with the signature `'A -> 'F option` where `'A` is the value being validated.
The function either returns `None` or it returns some validation failure.
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

The `disputeWithMany` operation takes a function with the signature `'A -> 'F list` where `'A` is the value being validated.
The function returns a list of failures.
If the result has one or more elements, the failures are added to the result and validation continues.
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

The `disputeWithFact` operation takes a failure value and a function with the signature `'A -> bool` where `'A` is the value being validated.
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

NOTE: the `isNotNull` function comes from this library and is explained in the [Validation Helpers](#Validation-Helpers) section.

#### `disputeAnyWith`

Similar to `disputeWith` but used for validating list like types.
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

There is an overload to the operator that takes a function with the signature `int -> 'A -> 'F option` where the first parameter is the index of the element.

#### `disputeAllWith`

Similar to `disputeWith` but used for validating list like types.
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

There is an overload to the operator that takes a function with the signature `int -> 'A -> 'F option` where the first parameter is the index of the element.

#### `disputeAnyWithMany`

Similar to `disputeWithMany` but used for validating list like types.
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

There is an overload to the operator that takes a function with the signature `int -> 'A -> 'F list` where the first parameter is the index of the element.

#### `disputeAllWithMany`

Similar to `disputeWithMany` but used for validating list like types.
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

There is an overload to the operator that takes a function with the signature `int -> 'A -> 'F list` where the first parameter is the index of the element.

#### `disputeAnyWithFact`

Similar to `disputeWithFact` but used for validating list like types.
If any of the elements fail validation, the entire list fails.

```fsharp
validation {
    withValue ["my string"; ""]
    ...
    disputeAnyWithFact Empty isNotNull
    ...
}
```

There is an overload to the operator that takes a function with the signature `int -> 'A -> bool` where the first parameter is the index of the element.

#### `disputeAllWithFact`

Similar to `disputeWithFact` but used for validating list like types.
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

There is an overload to the operator that takes a function with the signature `int -> 'A -> 'F list` where the first parameter is the index of the element.

#### `validateEach`

This function accepts a function with a signature of `'A -> VCtx<'F, 'B>` that validates each element.
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

### The `isRequired` Helper

This function is used with the `refute*` family of validation operations.
It transforms a value from type `'T option` to `'T` or adds the given validation failure to the result.

### The `isRequiredWhen` Helper

This function is used with the `dispute*` family of validation operations.
The `bool` parameters decides if the required check should execute.
If the `bool` parameter is `true`, the helper checks that an `'T option` type value is `Some 'T` or adds the given failure to the result.

### The `isRequiredUnless` Helper

This function is the same as `isRequiredWhen` except the `bool` value must be `false` for the check to occur.

### The `isOk` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a value of type `Result<'A, 'F>` is an `Ok 'A`.

### The `isError` Helper

This function is used with the `dispute*` family of validation operations.
It checks that a value of type `Result<'A, 'F>` is an `Error 'F`.

### The `isNull` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a list like value is empty.

### The `isNotNull` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a list like value is not empty.

### The `minLength` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a list like value has at least the given number of elements.

### The `maxLength` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a list like value has no more than the given number of elements.

### The `isLength` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a list like value has exactly the given number of elements.

### The `hasElem` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a list like value has an element equal to another value.

### The `doesNotHaveElem` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a list like value does not have an element equal to another value.

### The `isEqual` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a value is equal to another value using `(=)`.

### The `isNotEqual` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a value is not equal to another value using `(=)`.

### The `isLessThan` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a value is less than another value.

### The `isGreaterThan` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a value is greater than another value.

### The `isLessThanOrEqual` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a value is less or equal to than another value.

### The `isGreaterThanOrEqual` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a value is greater than or equal to another value.

### The `isValid` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a `Proof<'F, 'A>` is `Valid`.

### The `isInvalid` Helper

This function is used with the `dispute*` family of validation operations.
This function checks that a `Proof<'F, 'A>` is `Invalid`.

### The `flattenProofs` Helper

This function accepts a `Proof<'F, 'A> list` and transforms it to a `Proof<'F, 'A list>`.

### The `raiseIfInvalid` Helper

This function accepts a `Proof<'F, 'A>`.
If the value is `Valid`, it is transformed to `'A`.
Otherwise, an `InvalidProofException` is raised with the given message. 
This is useful when we are receiving data that you know to be valid, such as from a database, and know that validation will succeed.

## Data-Validation Library for Haskell

This library is based on our original library for [Haskell](https://www.haskell.org/).
 - Learn more about this library on Hackage: https://hackage.haskell.org/package/data-validation-0.1.2.5
  - Read the documentation on Hackage: https://hackage.haskell.org/package/data-validation-0.1.2.5/docs/Data-Validation.html
  - Visit the repository: https://github.com/alasconnect/data-validation

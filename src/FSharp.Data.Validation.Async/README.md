# FSharp.Data.Validation.Async

## Overview

This library extends FSharp.Data.Validation to support asynchronous workflows. When validation logic requires I/O operations (database lookups, API calls, file operations), you need to work with `Async<VCtx<'F, 'A>>` instead of just `VCtx<'F, 'A>`.

### What This Library Adds

**Base Library (`FSharp.Data.Validation`):**
- Synchronous validation with the `validation` computation expression
- Works with `VCtx<'F, 'A>` directly
- Immediate validation without I/O

**This Library (`FSharp.Data.Validation.Async`):**
- Asynchronous validation with the `asyncValidation` computation expression
- Works with `AsyncVCtx<'F, 'A>` (alias for `Async<VCtx<'F, 'A>>`)
- Supports I/O-based validation (database checks, API calls, file operations)
- **Seamless composition** - directly bind `VCtx`, `Result`, and `Proof` values without manual lifting
- Parallel and sequential async operation support

| Feature | Base Library | Async Library |
|---------|-------------|---------------|
| **Computation Expression** | `validation { }` | `asyncValidation { }` |
| **Core Type** | `VCtx<'F, 'A>` | `AsyncVCtx<'F, 'A>` (= `Async<VCtx<'F, 'A>>`) |
| **I/O Operations** | ❌ Synchronous only | ✅ Full async support |
| **Automatic Type Lifting** | N/A | ✅ `VCtx`, `Result`, `Proof` → `AsyncVCtx` |
| **Use Case** | Pure validation logic | Database checks, API calls, file I/O |

### Key Features

- **`asyncValidation` computation expression** - compose async validations naturally with `let!` and `and!` syntax
- **Source overloads** - automatic type conversion eliminates manual wrapping (inspired by FsToolkit.ErrorHandling)
- **AsyncVCtx module** - conversion functions and combinators for flexible composition
- **VCtx module extensions** - backwards-compatible async functions (`bindAsync`, `mapAsync`, etc.)

## Quick Start

### Using `asyncValidation`

The `asyncValidation` computation expression automatically handles type conversions, letting you mix sync and async validations seamlessly:

```fsharp
open FSharp.Data.Validation

type ValidationFailure = 
    | EmailRequired
    | EmailExists
    | InvalidFormat

// Check if email already exists in database
let emailExistsAsync (email: string): Async<bool> =
    async {
        // Simulate database lookup
        do! Async.Sleep 100
        return email = "taken@example.com"
    }

// Validate email with async check - automatic type lifting in action!
let validateEmailAsync (emailVM: string) : AsyncVCtx<ValidationFailure, string> =
    asyncValidation {
        // Directly bind synchronous VCtx - automatically converted
        let! email =
            validation {
                withValue emailVM
                refuteWith (isRequired EmailRequired)
                refuteWith (hasValidEmailFormat InvalidFormat)
                qed id
            } |> fromVCtx
        
        // Bind Async<Result> directly - automatically converted to AsyncVCtx
        let! _ =
            async {
                let! exists = emailExistsAsync email
                return if exists then Error EmailExists else Ok ()
            }
        
        return email
    }

// Use it
let result = validateEmailAsync "user@example.com" |> Async.RunSynchronously
```

### The Traditional Way: Using VCtx Functions

You can still use explicit combinators when you prefer functional composition:

```fsharp
let validateEmailAsync (emailVM: string) : Async<Proof<ValidationFailure, string>> =
    async {
        let initial = 
            validation {
                withValue emailVM
                refuteWith (isRequired EmailRequired)
                refuteWith (hasValidEmailFormat InvalidFormat)
                qed id
            } |> fromVCtx
        
        let! result =
            initial
            |> VCtx.bindToAsync (fun email ->
                async {
                    let! exists = emailExistsAsync email
                    return 
                        if exists then 
                            Invalid ([EmailExists], Map.empty)
                        else 
                            Valid email
                }
            )
        
        return result |> fromVCtx
    }
```

**Key Difference:** The `asyncValidation` CE automatically converts `VCtx`, `Result`, and `Proof` values when you use `let!`, while the traditional approach requires explicit wrapping with `Invalid`/`Valid` or conversion functions.

## The AsyncVCtx Type

`AsyncVCtx<'F, 'A>` is a type alias for `Async<VCtx<'F, 'A>>` that represents an asynchronous validation context.

```fsharp
type AsyncVCtx<'F, 'A> = Async<VCtx<'F, 'A>>
```

### AsyncVCtx Module Functions

The `AsyncVCtx` module provides conversion and composition functions:

#### Conversion Functions

**`ofVCtx`** - Lift a synchronous validation context to async:
```fsharp
let syncContext = Valid "hello"
let asyncContext : AsyncVCtx<string, string> = AsyncVCtx.ofVCtx syncContext
```

**`ofAsync`** - Convert an async value to async validation context:
```fsharp
let asyncValue : Async<string> = async { return "hello" }
let asyncContext : AsyncVCtx<'F, string> = AsyncVCtx.ofAsync asyncValue
```

**`ofResult`** - Convert a Result to async validation context:
```fsharp
let result = Ok "hello"
let asyncContext : AsyncVCtx<string, string> = AsyncVCtx.ofResult result
```

**`ofProof`** - Convert a Proof to async validation context:
```fsharp
let proof = Valid "hello"
let asyncContext : AsyncVCtx<string, string> = AsyncVCtx.ofProof proof
```

**`ofAsyncResult`** - Convert an async Result to async validation context:
```fsharp
let asyncResult : Async<Result<string, string>> = async { return Ok "hello" }
let asyncContext : AsyncVCtx<string, string> = AsyncVCtx.ofAsyncResult asyncResult
```
This is particularly useful for async I/O operations that return `Result`.

#### Composition Functions

**`bind`** - Chain async validation operations:
```fsharp
let validate1 : AsyncVCtx<Failure, int> = AsyncVCtx.ofVCtx (Valid 5)
let validate2 (x: int) : AsyncVCtx<Failure, string> = 
    AsyncVCtx.ofVCtx (Valid (string x))

let result = validate1 |> AsyncVCtx.bind validate2
```

**`map`** - Transform the success value:
```fsharp
let asyncContext = AsyncVCtx.ofVCtx (Valid 5)
let doubled = asyncContext |> AsyncVCtx.map (fun x -> x * 2)
```

**`mergeSources`** - Combine two independent async validations:
```fsharp
let validation1 : AsyncVCtx<Failure, int> = AsyncVCtx.ofVCtx (Valid 5)
let validation2 : AsyncVCtx<Failure, string> = AsyncVCtx.ofVCtx (Valid "hello")
let combined : AsyncVCtx<Failure, int * string> = 
    AsyncVCtx.mergeSources validation1 validation2
```

### The asyncValidation Computation Expression

The `asyncValidation` computation expression (backed by `asyncValidationBuilder`) provides Source overloads that automatically convert:

- `VCtx<'F, 'A>` → `AsyncVCtx<'F, 'A>`
- `Result<'A, 'F>` → `AsyncVCtx<'F, 'A>`
- `AsyncValue<'A>` → `AsyncVCtx<'F, 'A>` (pure async values, wrapped)
- `Async<Result<'A, 'F>>` → `AsyncVCtx<'F, 'A>` (ideal for I/O operations)
- `Proof<'F, 'A>` → `AsyncVCtx<'F, 'A>`
- `AsyncVCtx<'F, 'A>` → (no conversion needed)

This means you can use `let!` with any of these types without manual conversion:

```fsharp
asyncValidation {
    // VCtx - automatically lifted
    let! x = Valid 5
    
    // Result - automatically converted
    let! y = Ok 10
    
    // AsyncValue wrapper - for pure async values
    let! a = AsyncValue (someAsyncIntCall ())
    
    // Async<Result> from API or database calls - automatically converted
    let! z =
        async {
            // Simulate API call
            let! apiResult = someAsyncApiCall ()
            return apiResult  // Result<'A, 'F>
        }
    
    // Proof - automatically converted  
    let! w = Valid "hello"
    
    // AsyncVCtx - used directly
    let! v = AsyncVCtx.ofAsync (async { return 3 })
    
    // Parallel composition with and!
    let! a = Valid 1
    and! b = Valid 2
    
    return (x + y + a + b, w, v, z)
}
```

**Edge Case - Pure Async Values:**

Plain `Async<'A>` values (not wrapped in Result/Proof) cannot be bound directly due to type ambiguity. You have three options:

**Option 1: Wrap with `AsyncValue` (recommended)**
```fsharp
asyncValidation {
    // Use AsyncValue wrapper - clean and explicit
    let! x = AsyncValue (someAsyncIntCall ())
    let! y = AsyncValue (anotherAsyncCall ())
    return (x, y)
}
```

**Option 2: Use `AsyncVCtx.ofAsync` explicitly**
```fsharp
asyncValidation {
    let! x = AsyncVCtx.ofAsync (someAsyncCall ())
    return x
}
```

**Option 3: Wrap in Result/Proof**
```fsharp
asyncValidation {
    let! x =
        async {
            let! value = someAsyncCall ()
            return Ok value  // Now it's Async<Result>
        }
    return x
}
```

**Why this limitation exists:**

`AsyncVCtx<'F, 'A>` is defined as `Async<VCtx<'F, 'A>>`, which is structurally `Async<...>`. Therefore, adding a Source overload for plain `Async<'A>` would create type ambiguity:
- Should `Async<X>` be treated as a pure value or as a validation context?
- The wrapper type makes this explicit and unambiguous.

**Recommendation:** Use `AsyncValue` for pure async operations—it's the most semantically clear and requires only wrapping the outermost call.

**Note on Conversion Priority:**

The Source overloads are tried in this order during overload resolution:
1. `AsyncVCtx<'F, 'A>` (exact match)
2. `VCtx<'F, 'A>` (sync validation lift)
3. `AsyncValue<'A>` (pure async wrapper)
4. `Async<Result<'A, 'F>>` (async I/O result)
5. `Result<'A, 'F>` (sync result)
6. `Proof<'F, 'A>` (sync proof)

## VCtx Module Extensions (Legacy API)

### bindToAsync

**Signature:** `('A -> Async<VCtx<'F, 'B>>) -> VCtx<'F, 'A> -> Async<VCtx<'F, 'B>>`

**Use when:** You have a synchronous validation context and need to apply an async validation operation to its value.

**Behavior:**
- If context is `ValidCtx a`, applies the async function to `a`
- If context is `RefutedCtx`, short-circuits and returns immediately
- If context is `DisputedCtx`, applies async function and merges failures

**Example:**

```fsharp
let validateUsername (un: string) : VCtx<Failure, Username> =
    validation {
        withValue un
        disputeWithFact Empty (minLength 3)
        qed Username
    } |> fromVCtx

let result = 
    validateUsername "alice"
    |> VCtx.bindToAsync (fun name ->
        async {
            let! exists = checkUserExistsAsync name
            if exists then
                return Invalid([UsernameTaken], Map.empty)
            else
                return Valid name
        }
    )
```

### bindAsync

**Signature:** `('A -> Async<VCtx<'F, 'B>>) -> Async<VCtx<'F, 'A>> -> Async<VCtx<'F, 'B>>`

**Use when:** You have an async validation context and need to apply another async operation.

**Example:**

```fsharp
let getProfileAsync (user: User) : Async<VCtx<Failure, Profile>> = async { ... }
let getPermissionsAsync (profile: Profile) : Async<VCtx<Failure, Permissions>> = async { ... }

let result = 
    getProfileAsync user
    |> VCtx.bindAsync getPermissionsAsync
```

### bindFromAsync

**Signature:** `('A -> VCtx<'F, 'B>) -> Async<VCtx<'F, 'A>> -> Async<VCtx<'F, 'B>>`

**Use when:** You have an async validation context but need a synchronous validation operation afterward.

### mergeSourcesAsync

**Signature:** `Async<VCtx<'F, 'A>> -> Async<VCtx<'F, 'B>> -> Async<VCtx<'F, 'A * 'B>>`

**Use when:** You need to combine results from two independent async operations.

**Example:**

```fsharp
let validateUserAsync (vm: UserVM) : Async<VCtx<Failure, User>> = async { ... }
let validatePrefsAsync (vm: PrefsVM) : Async<VCtx<Failure, Prefs>> = async { ... }

let result =
    VCtx.mergeSourcesAsync 
        (validateUserAsync userVm)
        (validatePrefsAsync prefsVm)
```

### bindAndMergeSourcesAsync

**Signature:** `('A -> Async<VCtx<'F, 'B>>) -> Async<VCtx<'F, 'A>> -> Async<VCtx<'F, 'A * 'B>>`

**Use when:** You have an async operation whose result determines a dependent async operation.

### bindToAndMergeSourcesAsync

**Signature:** `('A -> Async<VCtx<'F, 'B>>) -> VCtx<'F, 'A> -> Async<VCtx<'F, 'A * 'B>>`

**Use when:** You have a synchronous validation context and need a dependent async operation.

### bindFromAndMergeSourcesAsync

**Signature:** `('A -> VCtx<'F, 'B>) -> Async<VCtx<'F, 'A>> -> Async<VCtx<'F, 'A * 'B>>`

**Use when:** You have an async validation context and need a synchronous operation.

### mapAsync

**Signature:** `('A -> Async<'B>) -> Async<VCtx<'F, 'A>> -> Async<VCtx<'F, 'B>>`

**Use when:** You need to transform the valid value with async operation, but don't add failures.

## Common Patterns

### Sequential Async Validation

**Modern approach with `asyncValidation`:**

```fsharp
let validateUserWithProfileAsync (userVm: UserVM) : AsyncVCtx<Failure, User * Profile> =
    asyncValidation {
        let! user = validateUserAsync userVm
        let! profile = getProfileAsync user.Id
        return (user, profile)
    }
```

**Traditional approach with VCtx functions:**

```fsharp
let validateUserWithProfileAsync (userVm: UserVM) : Async<Proof<Failure, User * Profile>> =
    validateUserAsync userVm
    |> VCtx.bindAndMergeSourcesAsync (fun user -> getProfileAsync user.Id)
    |> Async.map fromVCtx
```

### Parallel Async Validation

**Modern approach with `asyncValidation`:**

```fsharp
let validateRegistrationAsync (userVm: UserVM) (prefsVm: PrefsVM) 
    : AsyncVCtx<Failure, User * Prefs> =
    asyncValidation {
        let! user = validateUserAsync userVm
        and! prefs = validatePrefsAsync prefsVm
        return (user, prefs)
    }
```

**Traditional approach with VCtx functions:**

```fsharp
let validateRegistrationAsync (userVm: UserVM) (prefsVm: PrefsVM) 
    : Async<Proof<Failure, User * Prefs>> =
    VCtx.mergeSourcesAsync
        (validateUserAsync userVm)
        (validatePrefsAsync prefsVm)
    |> Async.map fromVCtx
```

### Mixing Sync and Async Validations

**Modern approach with `asyncValidation`:**

```fsharp
let validateOrderAsync (orderVm: OrderVM) : AsyncVCtx<Failure, Order> =
    asyncValidation {
        // Sync validation - automatically lifted
        let! items = 
            validation {
                withValue orderVm.Items
                refuteWith (isNotEmpty EmptyOrder)
                qed id
            } |> fromVCtx
        
        // Async check - used directly
        let! inventory = checkInventoryAsync items
        
        // Result from external API - automatically converted
        let! shipping = calculateShippingAsync orderVm.Address
        
        return { Items = items; Inventory = inventory; Shipping = shipping }
    }
```

**Traditional approach:** Requires manual type conversions with `VCtx.bindToAsync`, `AsyncVCtx.ofResult`, etc.

## Decision Tree: Which Approach to Use?

```
Do you have async operations?
├─ NO: Use standard FSharp.Data.Validation (validation CE)
└─ YES: Choose your style
    ├─ Computation Expression Style
    │   └─ Use asyncValidation { ... }
    │       - Natural let!/and! syntax
    │       - Automatic type conversions
    │       - Best for complex compositions
    │
    └─ Functional Composition Style
        └─ Use AsyncVCtx module or VCtx async functions
            - Explicit combinators (bind, map, mergeSources)
            - Point-free style possible
            - Best for simple pipelines
```

## See Also

- [Main README](../../README.md)
- [Giraffe Integration](../FSharp.Data.Validation.Giraffe/README.md)
- [Samples](../../samples/)

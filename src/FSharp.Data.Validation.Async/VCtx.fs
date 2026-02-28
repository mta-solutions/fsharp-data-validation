namespace FSharp.Data.Validation

open System
open System.Linq.Expressions
open FSharpPlus.Data

/// <summary>
/// Type alias for asynchronous validation contexts.
/// Combines Async computations with VCtx validation contexts.
/// </summary>
type AsyncVCtx<'F, 'A> = Async<VCtx<'F, 'A>>

/// <summary>
/// Wrapper type for pure async values to enable Source overload in asyncValidation CE.
/// Use this when you have a plain Async&lt;'A&gt; that doesn't return a validation context.
/// </summary>
/// <remarks>
/// Due to type ambiguity between Async&lt;'A&gt; and AsyncVCtx&lt;'F,'A&gt; (which is Async&lt;VCtx&lt;'F,'A&gt;&gt;),
/// plain Async values need to be wrapped. Users can either:
/// - Wrap explicitly: AsyncValue (myAsyncCall ())
/// - Use the ofAsync helper: AsyncVCtx.ofAsync (myAsyncCall ())
/// </remarks>
type AsyncValue<'A> = AsyncValue of Async<'A>

/// <summary>
/// AsyncVCtx module provides conversion functions and core operations for asynchronous validation contexts.
/// </summary>
[<RequireQualifiedAccess>]
module AsyncVCtx =
    /// <summary>
    /// Converts a synchronous validation context into an asynchronous one.
    /// </summary>
    let ofVCtx (c: VCtx<'F, 'A>): AsyncVCtx<'F, 'A> = async.Return c

    /// <summary>
    /// Converts a pure asynchronous value into a valid validation context.
    /// </summary>
    let ofAsync (a: Async<'A>): AsyncVCtx<'F, 'A> =
        async {
            let! x = a
            return ValidCtx x
        }

    /// <summary>
    /// Converts a Result into a validation context.
    /// Errors become RefutedCtx, success becomes ValidCtx.
    /// </summary>
    let ofResult (r: Result<'A, 'F>): AsyncVCtx<'F, 'A> =
        async {
            return
                match r with
                | Ok a -> ValidCtx a
                | Error f -> RefutedCtx ([f], Map.empty)
        }

    /// <summary>
    /// Converts a Proof into a validation context.
    /// </summary>
    let ofProof (p: Proof<'F, 'A>): AsyncVCtx<'F, 'A> =
        async {
            match p with
            | Valid a -> return ValidCtx a
            | Invalid (gfs, lfs) -> return RefutedCtx (gfs, lfs)
        }

    /// <summary>
    /// Converts an asynchronous Result into an asynchronous validation context.
    /// </summary>
    /// <remarks>
    /// Useful when you have an async operation that returns a Result.
    /// Success values (Ok) become ValidCtx, while errors (Error) become RefutedCtx
    /// with the error value in the global failures list.
    /// </remarks>
    let ofAsyncResult (ar: Async<Result<'A, 'F>>): AsyncVCtx<'F, 'A> =
        async {
            let! result = ar
            return! ofResult result
        }

    /// <summary>
    /// Binds a function that returns an asynchronous validation context to a synchronous validation context.
    /// </summary>
    let bindToAsync (fn: 'A -> AsyncVCtx<'F, 'B>) (c: VCtx<'F, 'A>): AsyncVCtx<'F, 'B> =
        async {
            match c with
            | ValidCtx a -> return! fn a
            | RefutedCtx (gfs, lfs) -> return RefutedCtx (gfs, lfs)
            | DisputedCtx (gfs, lfs, a) ->
                let! b = fn a
                match b with
                | ValidCtx b -> return DisputedCtx (gfs, lfs, b)
                | DisputedCtx (gfs', lfs', b) ->
                    return DisputedCtx (gfs @ gfs', Utilities.mergeFailures lfs lfs', b)
                | RefutedCtx (gfs', lfs') ->
                    return RefutedCtx (gfs @ gfs', Utilities.mergeFailures lfs lfs')
        }

    /// <summary>
    /// Binds a function that returns an asynchronous validation context to an asynchronous validation context.
    /// </summary>
    let bind (fn: 'A -> AsyncVCtx<'F, 'B>) (c: AsyncVCtx<'F, 'A>): AsyncVCtx<'F, 'B> =
        async {
            let! c' = c
            return! bindToAsync fn c'
        }

    /// <summary>
    /// Binds a function that returns a synchronous validation context to an asynchronous validation context.
    /// </summary>
    let bindFrom (fn: 'A -> VCtx<'F, 'B>) (c: AsyncVCtx<'F, 'A>): AsyncVCtx<'F, 'B> =
        bind (fun a -> async.Return (fn a)) c

    /// <summary>
    /// Maps a synchronous function over the value of an asynchronous validation context.
    /// </summary>
    let map (fn: 'A -> 'B) (c: AsyncVCtx<'F, 'A>): AsyncVCtx<'F, 'B> =
        async {
            let! c' = c
            return
                match c' with
                | ValidCtx a -> ValidCtx (fn a)
                | DisputedCtx (gfs, lfs, a) -> DisputedCtx (gfs, lfs, fn a)
                | RefutedCtx (gfs, lfs) -> RefutedCtx (gfs, lfs)
        }

    /// <summary>
    /// Maps an asynchronous function over the value of an asynchronous validation context.
    /// </summary>
    let mapAsync (fn: 'A -> Async<'B>) (c: AsyncVCtx<'F, 'A>): AsyncVCtx<'F, 'B> =
        async {
            let! c' = c
            match c' with
            | ValidCtx a ->
                let! b = fn a
                return ValidCtx b
            | RefutedCtx (gfs, lfs) -> return RefutedCtx (gfs, lfs)
            | DisputedCtx (gfs, lfs, a) ->
                let! b = fn a
                return DisputedCtx (gfs, lfs, b)
        }

    /// <summary>
    /// Merges two asynchronous validation contexts into a single one containing a tuple of both values.
    /// </summary>
    let mergeSources (c1: AsyncVCtx<'F, 'A>) (c2: AsyncVCtx<'F, 'B>): AsyncVCtx<'F, 'A * 'B> =
        async {
            let! a = c1
            let! b = c2
            return VCtx.mergeSources a b
        }

    // Backwards compatibility: expose old helper names as delegating wrappers
    let bindAsync (fn: 'A -> Async<VCtx<'F, 'B>>) (c: Async<VCtx<'F, 'A>>) =
        bind fn c

    let bindFromAsync (fn: 'A -> VCtx<'F, 'B>) (c: Async<VCtx<'F, 'A>>) =
        bindFrom fn c

    let mergeSourcesAsync
        (c1: Async<VCtx<'F, 'A>>)
        (c2: Async<VCtx<'F, 'B>>)
        : Async<VCtx<'F, 'A * 'B>> =
        mergeSources c1 c2

    let bindAndMergeSourcesAsync
        (fn: 'A -> Async<VCtx<'F, 'B>>)
        (c: Async<VCtx<'F, 'A>>)
        : Async<VCtx<'F, 'A * 'B>> =
        async {
            let! a = c
            let b = bindToAsync fn a
            let! b' = b
            return VCtx.mergeSources a b'
        }

    let bindToAndMergeSourcesAsync
        (fn: 'A -> Async<VCtx<'F, 'B>>)
        (c: VCtx<'F, 'A>)
        : Async<VCtx<'F, 'A * 'B>> =
        async {
            let! b = bindToAsync fn c
            return VCtx.mergeSources c b
        }

    let bindFromAndMergeSourcesAsync
        (fn: 'A -> VCtx<'F, 'B>)
        (c: Async<VCtx<'F, 'A>>)
        : Async<VCtx<'F, 'A * 'B>> =
        async {
            let! a = c
            let b = bindFromAsync fn (async.Return a)
            let! b' = b
            return VCtx.mergeSources a b'
        }

// Backwards compatibility: VCtx module extends with async-specific helpers
[<RequireQualifiedAccess>]
module VCtx =
    let bindToAsync (fn: 'A -> Async<VCtx<'F, 'B>>) (c: VCtx<'F, 'A>): Async<VCtx<'F, 'B>> =
        AsyncVCtx.bindToAsync fn c

    let bindAsync (fn: 'A -> Async<VCtx<'F, 'B>>) (c: Async<VCtx<'F, 'A>>): Async<VCtx<'F, 'B>> =
        AsyncVCtx.bindAsync fn c

    let bindFromAsync (fn: 'A -> VCtx<'F, 'B>) (c: Async<VCtx<'F, 'A>>): Async<VCtx<'F, 'B>> =
        AsyncVCtx.bindFromAsync fn c

    let mergeSourcesAsync
        (c1: Async<VCtx<'F, 'A>>)
        (c2: Async<VCtx<'F, 'B>>)
        : Async<VCtx<'F, 'A * 'B>> =
        AsyncVCtx.mergeSourcesAsync c1 c2

    let bindAndMergeSourcesAsync
        (fn: 'A -> Async<VCtx<'F, 'B>>)
        (c: Async<VCtx<'F, 'A>>)
        : Async<VCtx<'F, 'A * 'B>> =
        AsyncVCtx.bindAndMergeSourcesAsync fn c

    let bindToAndMergeSourcesAsync
        (fn: 'A -> Async<VCtx<'F, 'B>>)
        (c: VCtx<'F, 'A>)
        : Async<VCtx<'F, 'A * 'B>> =
        AsyncVCtx.bindToAndMergeSourcesAsync fn c

    let bindFromAndMergeSourcesAsync
        (fn: 'A -> VCtx<'F, 'B>)
        (c: Async<VCtx<'F, 'A>>)
        : Async<VCtx<'F, 'A * 'B>> =
        AsyncVCtx.bindFromAndMergeSourcesAsync fn c

    let mapAsync
        (fn: 'A -> Async<'B>)
        (c: Async<VCtx<'F, 'A>>)
        : Async<VCtx<'F, 'B>> =
        AsyncVCtx.mapAsync fn c

/// <summary>
/// AsyncVCtxBuilder provides computation expression support for asynchronous validation contexts
/// with seamless type coercion via Source overloads, eliminating manual type lifting.
/// </summary>
type AsyncVCtxBuilder() =
    /// <summary>
    /// Binds an asynchronous validation context to a function, enabling monadic composition.
    /// </summary>
    /// <param name="c">The input asynchronous validation context to bind.</param>
    /// <param name="fn">A function that takes the validated value and returns a new asynchronous validation context.</param>
    /// <returns>An asynchronous validation context resulting from applying the function to the input context.</returns>
    /// <remarks>
    /// This method enables the <c>let!</c> syntax in computation expressions. It threads validation
    /// state (Valid, Disputed, Refuted) through the monadic chain, accumulating failures as needed.
    /// </remarks>
    member this.Bind(c: AsyncVCtx<'F, 'A>, fn: 'A -> AsyncVCtx<'F, 'B>): AsyncVCtx<'F, 'B> =
        AsyncVCtx.bind fn c

    /// <summary>
    /// Wraps a value in a valid asynchronous validation context.
    /// </summary>
    /// <param name="a">The value to wrap.</param>
    /// <returns>An asynchronous validation context containing the value in a ValidCtx state.</returns>
    /// <remarks>
    /// This method enables the <c>return</c> syntax in computation expressions, creating
    /// a successful validation with no failures.
    /// </remarks>
    member this.Return(a: 'A): AsyncVCtx<'F, 'A> =
        AsyncVCtx.ofVCtx (ValidCtx a)

    /// <summary>
    /// Returns an asynchronous validation context as-is without wrapping.
    /// </summary>
    /// <param name="c">The asynchronous validation context to return.</param>
    /// <returns>The same asynchronous validation context, unchanged.</returns>
    /// <remarks>
    /// This method enables the <c>return!</c> syntax in computation expressions, allowing
    /// direct return of existing validation contexts without additional wrapping.
    /// </remarks>
    member this.ReturnFrom(c: AsyncVCtx<'F, 'A>): AsyncVCtx<'F, 'A> =
        c

    /// <summary>
    /// Delays execution of a computation expression until explicitly evaluated.
    /// </summary>
    /// <param name="fn">A function that produces an asynchronous validation context when invoked.</param>
    /// <returns>A delayed computation that can be executed later.</returns>
    /// <remarks>
    /// This method enables lazy evaluation of computation expressions, which is necessary
    /// for proper handling of control flow and side effects.
    /// </remarks>
    member this.Delay(fn: unit -> AsyncVCtx<'F, 'A>): unit -> AsyncVCtx<'F, 'A> =
        fn

    /// <summary>
    /// Executes a delayed computation expression.
    /// </summary>
    /// <param name="fn">The delayed computation to execute.</param>
    /// <returns>The asynchronous validation context produced by executing the computation.</returns>
    /// <remarks>
    /// This method is called automatically by the F# compiler to execute delayed computations
    /// created by the <c>Delay</c> method.
    /// </remarks>
    member this.Run(fn: unit -> AsyncVCtx<'F, 'A>): AsyncVCtx<'F, 'A> =
        fn()

    /// <summary>
    /// Provides an empty computation that produces a unit value in a valid validation context.
    /// </summary>
    /// <returns>An asynchronous validation context containing unit in a ValidCtx state.</returns>
    /// <remarks>
    /// This method enables computation expressions that don't explicitly return a value,
    /// resulting in a successful validation with no data.
    /// </remarks>
    member this.Zero(): AsyncVCtx<'F, unit> =
        AsyncVCtx.ofVCtx (ValidCtx ())

    /// <summary>
    /// Merges two asynchronous validation contexts into a single context containing a tuple of both values.
    /// </summary>
    /// <param name="c1">The first asynchronous validation context.</param>
    /// <param name="c2">The second asynchronous validation context.</param>
    /// <returns>An asynchronous validation context containing a tuple of both values.</returns>
    /// <remarks>
    /// This method enables the <c>and!</c> syntax in computation expressions, allowing parallel
    /// binding of multiple validation contexts. Failures from both contexts are merged according
    /// to VCtx merging semantics (Refuted takes precedence over Disputed over Valid).
    /// </remarks>
    member this.MergeSources(c1: AsyncVCtx<'F, 'A>, c2: AsyncVCtx<'F, 'B>): AsyncVCtx<'F, 'A * 'B> =
        AsyncVCtx.mergeSources c1 c2

    /// <summary>
    /// Binds each element of a collection through a computation, enabling iteration in computation expressions.
    /// </summary>
    /// <param name="c">The collection wrapped in an asynchronous validation context.</param>
    /// <param name="fn">A function to apply to each element.</param>
    /// <returns>An asynchronous validation context containing the results.</returns>
    /// <remarks>
    /// This method enables the <c>for</c> syntax in computation expressions for iterating
    /// over validated collections.
    /// </remarks>
    member this.For(c: AsyncVCtx<'F, 'A>, fn: 'A -> AsyncVCtx<'F, 'B>): AsyncVCtx<'F, 'B> =
        this.Bind(c, fn)

    /// <summary>
    /// Wraps a value in a valid asynchronous validation context (alternative to Return).
    /// </summary>
    /// <param name="a">The value to wrap.</param>
    /// <returns>An asynchronous validation context containing the value in a ValidCtx state.</returns>
    /// <remarks>
    /// This method enables the <c>yield</c> syntax in computation expressions, which is
    /// semantically equivalent to <c>return</c> in this context.
    /// </remarks>
    member this.Yield(a: 'A): AsyncVCtx<'F, 'A> =
        this.Return(a)

    // ========== Source Overloads ==========
    // These enable automatic type coercion in let! binding

    /// <summary>
    /// Source overload for AsyncVCtx - passes through the asynchronous validation context unchanged.
    /// </summary>
    /// <param name="c">The asynchronous validation context to use as a source.</param>
    /// <returns>The same asynchronous validation context.</returns>
    /// <remarks>
    /// This overload enables direct binding of AsyncVCtx values in <c>let!</c> expressions
    /// without requiring manual type conversion.
    /// </remarks>
    member this.Source(c: AsyncVCtx<'F, 'A>): AsyncVCtx<'F, 'A> =
        c

    /// <summary>
    /// Source overload for VCtx - lifts a synchronous validation context to an asynchronous one.
    /// </summary>
    /// <param name="c">The synchronous validation context to lift.</param>
    /// <returns>An asynchronous validation context containing the same validation state.</returns>
    /// <remarks>
    /// This overload enables direct binding of VCtx values in <c>let!</c> expressions,
    /// automatically lifting them to AsyncVCtx without manual conversion via <c>AsyncVCtx.ofVCtx</c>.
    /// </remarks>
    member this.Source(c: VCtx<'F, 'A>): AsyncVCtx<'F, 'A> =
        AsyncVCtx.ofVCtx c

    /// <summary>
    /// Source overload for Result - converts a Result to a validation context.
    /// </summary>
    /// <param name="r">The Result value to convert (Ok becomes ValidCtx, Error becomes RefutedCtx).</param>
    /// <returns>An asynchronous validation context representing the Result.</returns>
    /// <remarks>
    /// This overload enables direct binding of <c>Result&lt;'A, 'F&gt;</c> values in <c>let!</c> expressions.
    /// Success values (<c>Ok</c>) become <c>ValidCtx</c>, while errors (<c>Error</c>) become <c>RefutedCtx</c>
    /// with the error value in the global failures list.
    /// </remarks>
    member this.Source(r: Result<'A, 'F>): AsyncVCtx<'F, 'A> =
        AsyncVCtx.ofResult r

    /// <summary>
    /// Source overload for AsyncResult - converts an async Result to a validation context.
    /// </summary>
    /// <param name="ar">The async Result to convert (Ok becomes ValidCtx, Error becomes RefutedCtx).</param>
    /// <returns>An asynchronous validation context representing the async Result.</returns>
    /// <remarks>
    /// This overload enables direct binding of <c>Async&lt;Result&lt;'A, 'F&gt;&gt;</c> values in <c>let!</c> expressions
    /// returned from async I/O operations, eliminating the need for manual conversion with <c>AsyncVCtx.ofAsyncResult</c>.
    /// </remarks>
    member this.Source(ar: Async<Result<'A, 'F>>): AsyncVCtx<'F, 'A> =
        AsyncVCtx.ofAsyncResult ar

    /// <summary>
    /// Source overload for Proof - converts a Proof to a validation context.
    /// </summary>
    /// <param name="p">The Proof value to convert (Valid becomes ValidCtx, Invalid becomes RefutedCtx).</param>
    /// <returns>An asynchronous validation context representing the Proof.</returns>
    /// <remarks>
    /// This overload enables direct binding of <c>Proof&lt;'F, 'A&gt;</c> values in <c>let!</c> expressions.
    /// Valid proofs become <c>ValidCtx</c>, while invalid proofs become <c>RefutedCtx</c> with their
    /// associated failure information preserved.
    /// </remarks>
    member this.Source(p: Proof<'F, 'A>): AsyncVCtx<'F, 'A> =
        AsyncVCtx.ofProof p

    /// <summary>
    /// Source overload for AsyncValue - converts a wrapper around pure async to validation context.
    /// </summary>
    /// <param name="av">The AsyncValue wrapper containing a pure async value.</param>
    /// <returns>An asynchronous validation context with the async value in a ValidCtx.</returns>
    /// <remarks>
    /// This overload solves the type ambiguity problem with plain Async&lt;'A&gt; values.
    /// Wrap pure async values with <c>AsyncValue</c> to enable direct binding:
    /// <code>
    /// let! x = AsyncValue (someAsyncCall ())
    /// </code>
    /// Alternatively, use <c>AsyncVCtx.ofAsync</c> for the same effect without wrapping.
    /// </remarks>
    member this.Source(av: AsyncValue<'A>): AsyncVCtx<'F, 'A> =
        match av with
        | AsyncValue a -> AsyncVCtx.ofAsync a

/// <summary>
/// Module containing the asyncValidationBuilder instance for creating async validation computation expressions.
/// </summary>
[<AutoOpen>]
module AsyncValidation =
    /// <summary>
    /// Computation expression builder for asynchronous validation contexts.
    /// Enables seamless composition of async operations with validation logic using Source overloads.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The builder provides Source overloads for the following types, enabling direct binding in <c>let!</c> expressions:
    /// </para>
    /// <list type="bullet">
    /// <item><description><c>AsyncVCtx&lt;'F, 'A&gt;</c> - Pass-through for async validation contexts</description></item>
    /// <item><description><c>VCtx&lt;'F, 'A&gt;</c> - Automatically lifts sync validation contexts</description></item>
    /// <item><description><c>Result&lt;'A, 'F&gt;</c> - Converts Result to validation (Ok → ValidCtx, Error → RefutedCtx)</description></item>
    /// <item><description><c>Proof&lt;'F, 'A&gt;</c> - Converts Proof to validation (Valid → ValidCtx, Invalid → RefutedCtx)</description></item>
    /// </list>
    /// <para>
    /// For pure <c>Async&lt;'A&gt;</c> values, use <c>AsyncVCtx.ofAsync</c> explicitly to avoid type ambiguity.
    /// </para>
    /// </remarks>
    /// <example>
    /// <code>
    /// asyncValidationBuilder {
    ///     let! userId = AsyncVCtx.ofAsync (async { return 123 })  // Pure async requires ofAsync
    ///     and! userName = Ok "john_doe"                           // Result automatically lifted
    ///     and! profile = getProfileVCtx userId                    // VCtx automatically lifted
    ///     return (userId, userName, profile)
    /// }
    /// </code>
    /// </example>
    let asyncValidationBuilder = AsyncVCtxBuilder()
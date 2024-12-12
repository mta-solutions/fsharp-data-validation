namespace FSharp.Data.Validation

[<RequireQualifiedAccess>]
module VCtx =
    /// <summary>
    /// Binds a function that returns an asynchronous computation to a validation context.
    /// </summary>
    /// <remarks>
    /// This function takes a function <c>fn</c> that transforms a value of type <c>'A</c> into an
    /// asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c> and a validation context <c>c</c>
    /// of type <c>VCtx&lt;'F, 'A&gt;</c>. It returns an asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c>.
    ///
    /// The function handles the following cases:
    /// <list type="bullet">
    /// <item>
    /// <description><c>ValidCtx a</c>: Applies the function <c>fn</c> to <c>a</c> and returns the result.</description>
    /// </item>
    /// <item>
    /// <description><c>RefutedCtx (gfs, lfs)</c>: Returns the same <c>RefutedCtx</c> without applying the function.</description>
    /// </item>
    /// <item>
    /// <description><c>DisputedCtx (gfs, lfs, a)</c>: Applies the function <c>fn</c> to <c>a</c> and merges the results accordingly.</description>
    /// </item>
    /// </list>
    /// </remarks>
    /// <param name="fn">A function that takes a value of type <c>'A</c> and returns an asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c>.</param>
    /// <param name="c">A validation context of type <c>VCtx&lt;'F, 'A&gt;</c>.</param>
    /// <returns>An asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c>.</returns>
    let bindToAsync (fn:'A -> Async<VCtx<'F, 'B>>) (c: VCtx<'F, 'A>): Async<VCtx<'F, 'B>> =
        async {
            match c with
            | ValidCtx a                -> return! fn a
            | RefutedCtx (gfs,lfs)      -> return RefutedCtx (gfs,lfs)
            | DisputedCtx (gfs,lfs,a)   ->
                let! b = fn a
                match b with
                | ValidCtx b                -> return DisputedCtx (gfs,lfs,b)
                | DisputedCtx (gfs',lfs',b) -> return DisputedCtx (gfs @ gfs', Utilities.mergeFailures lfs lfs', b)
                | RefutedCtx (gfs',lfs')    -> return RefutedCtx (gfs @ gfs', Utilities.mergeFailures lfs lfs')
        }

    /// <summary>
    /// Binds a function that returns an asynchronous validation context to an asynchronous validation computation.
    /// </summary>
    /// <remarks>
    /// This function takes a function <c>fn</c> that transforms a value of type <c>'A</c> into a validation context
    /// of type <c>VCtx&lt;'F, 'B&gt;</c> and an asynchronous computation <c>c</c> of type <c>Async&lt;VCtx&lt;'F, 'A&gt;&gt;</c>.
    /// It returns an asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c>.
    /// </remarks>
    /// <param name="fn">A function that takes a value of type <c>'A</c> and returns a validation context of type <c>VCtx&lt;'F, 'B&gt;</c>.</param>
    /// <param name="c">An asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'A&gt;&gt;</c>.</param>
    /// <returns>An asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c>.</returns>
    let bindAsync (fn:'A -> Async<VCtx<'F, 'B>>) (c: Async<VCtx<'F, 'A>>): Async<VCtx<'F, 'B>> =
        async {
            let! c' = c
            return! bindToAsync fn c'
        }

    /// <summary>
    /// Binds a function that returns a validation context to an asynchronous validation context.
    /// </summary>
    /// <remarks>
    /// This function takes a function <c>fn</c> that transforms a value of type <c>'A</c> into a validation context
    /// of type <c>VCtx&lt;'F, 'B&gt;</c> and a validation context <c>c</c> of type <c>VCtx&lt;'F, 'A&gt;</c>.
    /// It returns a validation context of type <c>VCtx&lt;'F, 'B&gt;</c>.
    /// </remarks>
    /// <param name="fn">A function that takes a value of type <c>'A</c> and returns a validation context of type <c>VCtx&lt;'F, 'B&gt;</c>.</param>
    /// <param name="c">A validation context of type <c>VCtx&lt;'F, 'A&gt;</c>.</param>
    /// <returns>A validation context of type <c>VCtx&lt;'F, 'B&gt;</c>.</returns>
    let bindFromAsync (fn:'A -> VCtx<'F, 'B>) (c: Async<VCtx<'F, 'A>>): Async<VCtx<'F, 'B>> =
        bindAsync (fn >> async.Return) c

    /// <summary>
    /// Merge sources of two asynchronous computations of validation contexts into a single asynchronous validation computation.
    /// </summary>
    /// <remarks>
    /// This function takes two asynchronous computations <c>c1</c> and <c>c2</c> of type <c>Async&lt;VCtx&lt;'F, 'A&gt;&gt;</c>
    /// and returns an asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'A * 'B&gt;&gt;</c> that merges the results.
    /// </remarks>
    /// <param name="c1">An asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'A&gt;&gt;</c>.</param>
    /// <param name="c2">An asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c>.</param>
    /// <returns>An asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'A * 'B&gt;&gt;</c>.</returns>
    /// <seealso cref="VCtx.mergeSources"/>
    let mergeSourcesAsync
        (c1: Async<VCtx<'F,'A>>)
        (c2: Async<VCtx<'F,'B>>)
        : Async<VCtx<'F,'A * 'B>> =
        async {
            let! a = c1
            let! b = c2
            return VCtx.mergeSources a b
        }

    /// <summary>
    /// Binds a function that returns an asynchronous validation computation to an asynchronous validation computation and merges the results.
    /// </summary>
    /// <remarks>
    /// This function takes a function <c>fn</c> that transforms a value of type <c>'A</c> into an asynchronous computation
    /// of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c> and an asynchronous computation <c>c</c> of type <c>Async&lt;VCtx&lt;'F, 'A&gt;&gt;</c>.
    /// It returns an asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'A * 'B&gt;&gt;</c> that merges the results.
    /// </remarks>
    /// <param name="fn">A function that takes a value of type <c>'A</c> and returns an asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c>.</param>
    /// <param name="c">An asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'A&gt;&gt;</c>.</param>
    /// <returns>An asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'A * 'B&gt;&gt;</c>.</returns>
    /// <seealso cref="VCtx.bindAsync"/>
    /// <seealso cref="VCtx.mergeSourcesAsync"/>
    let bindAndMergeSourcesAsync
        (fn: 'A -> Async<VCtx<'F,'B>>)
        (c: Async<VCtx<'F,'A>>)
        : Async<VCtx<'F,'A * 'B>> =
        bindAsync fn c |> mergeSourcesAsync c

    /// <summary>
    /// Binds a function that returns an asynchronous validation computation to a validation context and merges the results.
    /// </summary>
    /// <remarks>
    /// This function takes a function <c>fn</c> that transforms a value of type <c>'A</c> into an
    /// asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c> and a validation context <c>c</c>
    /// of type <c>VCtx&lt;'F, 'A&gt;</c>. It returns an asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c>.
    /// </remarks>
    /// <param name="fn">A function that takes a value of type <c>'A</c> and returns an asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c>.</param>
    /// <param name="c">A validation context of type <c>VCtx&lt;'F, 'A&gt;</c>.</param>
    /// <returns>An asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c>.</returns>
    /// <seealso cref="VCtx.bindToAsync"/>
    /// <seealso cref="VCtx.mergeSources"/>
    let bindToAndMergeSourcesAsync
        (fn: 'A -> Async<VCtx<'F,'B>>)
        (c: VCtx<'F,'A>)
        : Async<VCtx<'F,'A * 'B>> =
        async {
            let! b = bindToAsync fn c
            return VCtx.mergeSources c b
        }

    // bindFromAndMergeSourcesAsync: ('A -> VCtx<'F, 'B>) -> Async<VCtx<'F, 'A>> -> Async<VCtx<'F, 'A * 'B>>

    /// <summary>
    /// Binds a function that returns a validation context to an asynchronous validation computation and merges the results.
    /// </summary>
    /// <remarks>
    /// This function takes a function <c>fn</c> that transforms a value of type <c>'A</c> into a validation context
    /// of type <c>VCtx&lt;'F, 'B&gt;</c> and an asynchronous computation <c>c</c> of type <c>Async&lt;VCtx&lt;'F, 'A&gt;&gt;</c>.
    /// It returns an asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'A * 'B&gt;&gt;</c> that merges the results.
    /// </remarks>
    /// <param name="fn">A function that takes a value of type <c>'A</c> and returns a validation context of type <c>VCtx&lt;'F, 'B&gt;</c>.</param>
    /// <param name="c">An asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'A&gt;&gt;</c>.</param>
    /// <returns>An asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'A * 'B&gt;&gt;</c>.</returns>
    /// <seealso cref="VCtx.bind"/>
    /// <seealso cref="VCtx.mergeSources"/>
    let bindFromAndMergeSourcesAsync
        (fn: 'A -> VCtx<'F, 'B>)
        (c: Async<VCtx<'F, 'A>>)
        : Async<VCtx<'F, 'A * 'B>> =
        async {
            let! c' = c
            let b = VCtx.bind fn c'
            return VCtx.mergeSources c' b
        }

    /// <summary>
    /// Maps a function over the value of a validation context. The function returns an asynchronous computation.
    /// </summary>
    /// <remarks>
    /// This function takes a function <c>fn</c> that transforms a value of type <c>'A</c> into an asynchronous computation
    /// of type <c>Async&lt;'B&gt;</c> and an asynchronous validation context <c>c</c> of type <c>Async&lt;VCtx&lt;'F, 'A&gt;&gt;</c>.
    /// It returns an asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c>.
    /// </remarks>
    /// <param name="fn">A function that takes a value of type <c>'A</c> and returns an asynchronous computation of type <c>Async&lt;'B&gt;</c>.</param>
    /// <param name="c">An asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'A&gt;&gt;</c>.</param>
    /// <returns>An asynchronous computation of type <c>Async&lt;VCtx&lt;'F, 'B&gt;&gt;</c>.</returns>
    let mapAsync
        (fn: 'A -> Async<'B>)
        (c: Async<VCtx<'F,'A>>)
        : Async<VCtx<'F,'B>> =
        async {
            let! c' = c
            match c' with
            | ValidCtx a                ->
                let! b = fn a
                return ValidCtx b
            | RefutedCtx (gfs,lfs)      -> return RefutedCtx (gfs,lfs)
            | DisputedCtx (gfs,lfs,a)   ->
                let! b = fn a
                return DisputedCtx (gfs,lfs, b)
        }
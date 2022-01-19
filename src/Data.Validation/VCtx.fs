namespace Data.Validation

type VCtx<'F, 'A> = 
    internal
    | ValidCtx of 'A
    | DisputedCtx of 'F list * FailureMap<'F> * 'A
    | RefutedCtx of 'F list * FailureMap<'F>

module VCtx =
    let bind fn c =
        match c with
        | ValidCtx a                -> fn a
        | RefutedCtx (gfs,lfs)      -> RefutedCtx (gfs,lfs)
        | DisputedCtx (gfs,lfs,a)   -> 
            match fn a with
            | ValidCtx b                -> DisputedCtx (gfs,lfs,b)
            | DisputedCtx (gfs',lfs',b) -> DisputedCtx (gfs @ gfs', Utilities.mergeFailures lfs lfs', b)
            | RefutedCtx (gfs',lfs')    -> RefutedCtx (gfs @ gfs', Utilities.mergeFailures lfs lfs')

    let map fn c = 
        match c with
        | ValidCtx a                -> ValidCtx (fn a)
        | DisputedCtx (gfs,lfs,a)   -> DisputedCtx (gfs,lfs,fn a)
        | RefutedCtx (gfs,lfs)      -> RefutedCtx (gfs,lfs)

    let applyFailures (v:ValueCtx<'A>) (gfs:'F list,lfs:FailureMap<'F>) (gfs':'F list,lfs':FailureMap<'F>): 'F list * FailureMap<'F> = 
        match v with
        | Global a -> (gfs @ gfs', Utilities.mergeFailures lfs lfs')
        | Field (n, a) -> 
            let lfs2 = Utilities.mapKeys (fun ns -> n :: ns) lfs'
            let lfs3 = Map.add [n] gfs' Map.empty
            (gfs, Utilities.mergeFailures lfs <| Utilities.mergeFailures lfs3 lfs2)

type VCtxBuilder() =
    member this.Bind(v:VCtx<'F, 'A>, fn:'A -> VCtx<'F, 'B>): VCtx<'F, 'B> =
        match v with
        | ValidCtx a                -> fn a
        | RefutedCtx (gfs, lfs)     -> RefutedCtx (gfs, lfs)
        | DisputedCtx (gfs, lfs, a) ->
            match fn a with
            | ValidCtx b                    -> DisputedCtx (gfs, lfs, b)
            | RefutedCtx (gfs', lfs')       -> RefutedCtx (List.append gfs gfs', Utilities.mergeFailures lfs lfs')
            | DisputedCtx (gfs', lfs', b)   -> DisputedCtx (List.append gfs gfs', Utilities.mergeFailures lfs lfs', b)

    member this.MergeSources(v1: VCtx<'F, 'A>, v2: VCtx<'F, 'B>) = 
        match (v1, v2) with
        | ValidCtx a, ValidCtx b                                    -> ValidCtx (a, b)
        | ValidCtx _, DisputedCtx (gfs', lfs', _)                   -> RefutedCtx (gfs', lfs')
        | ValidCtx _, RefutedCtx (gfs', lfs')                       -> RefutedCtx (gfs', lfs')
        | DisputedCtx (gfs, lfs, _), ValidCtx _                     -> RefutedCtx (gfs, lfs)
        | DisputedCtx (gfs, lfs, _), DisputedCtx (gfs', lfs', _)    -> RefutedCtx (gfs @ gfs', Utilities.mergeFailures lfs lfs')
        | DisputedCtx (gfs, lfs, _), RefutedCtx (gfs', lfs')        -> RefutedCtx (gfs @ gfs', Utilities.mergeFailures lfs lfs')
        | RefutedCtx (gfs, lfs), ValidCtx _                         -> RefutedCtx (gfs, lfs)
        | RefutedCtx (gfs, lfs), DisputedCtx (gfs', lfs', _)        -> RefutedCtx (gfs @ gfs', Utilities.mergeFailures lfs lfs')
        | RefutedCtx (gfs, lfs), RefutedCtx (gfs', lfs')            -> RefutedCtx (gfs @ gfs', Utilities.mergeFailures lfs lfs')

    member this.For(v:VCtx<'F, 'A>, fn:'A -> VCtx<'F, 'B>): VCtx<'F, 'B> = this.Bind(v, fn)

    member this.Return(a:'A): VCtx<'F, 'A> = ValidCtx a

    member this.ReturnFrom(ctx:VCtx<'F, 'A>): VCtx<'F, 'A> = ctx

    member this.Yield(a:'A) = this.Return(a)

    //member this.Delay(fn:unit -> VCtx<'F, 'A>): unit -> VCtx<'F, 'A> = fn

    //member this.Run(fn:unit -> VCtx<'F, 'A>): VCtx<'F, 'A> = fn()

    member this.Zero() = ValidCtx ()
        
    /// Performs some given validation using a 'Field' with a given name and value.
    [<CustomOperation("withField")>]
    member this.WithField(c, n:Name, b) = 
        c |> VCtx.map (fun _v -> Field (n, b))
    
    /// Performs some given validation using a 'Field' with a given name and value.
    [<CustomOperation("withField")>]
    member this.WithField(c, mn:Name option, b) = 
        match mn with
        | None -> this.WithValue(c, b)
        | Some n -> this.WithField(c, n, b)
    
    /// Performs some given validation using a 'Global' with a given value.
    [<CustomOperation("withValue")>]
    member this.WithValue(c, b) = 
        c |> VCtx.map (fun _v -> Global b)
    
    /// Maps a proven value with a given function.
    [<CustomOperation("whenProven")>]
    member this.WhenProven(c:VCtx<'F, ValueCtx<'A>>, fn:'A -> 'B): VCtx<'F, 'B> = 
        c |> VCtx.map (fun a -> ValueCtx.getValue a |> fn)
        
    /// Maps a proven value with a given function.
    [<CustomOperation("optional")>]
    member this.Optional(c:VCtx<'F, ValueCtx<'A option>>, fn:'A -> VCtx<'F, ValueCtx<'B>>): VCtx<'F, ValueCtx<'B option>> =
        match c with
        | ValidCtx v                -> 
            match ValueCtx.getValue v with
            | None -> ValidCtx (ValueCtx.setValue v None)
            | Some a -> //(fn a) |> (VCtx.map (ValueCtx.map Some))
                match fn a with
                | ValidCtx b                -> ValidCtx (ValueCtx.map Some b)
                | DisputedCtx (gfs,lfs,b)   -> 
                    let (gfs',lfs') = VCtx.applyFailures v ([], Map.empty) (gfs,lfs)
                    DisputedCtx (gfs',lfs',ValueCtx.map Some b)
                | RefutedCtx (gfs,lfs)      -> RefutedCtx (VCtx.applyFailures v ([], Map.empty) (gfs,lfs))
        | DisputedCtx (gfs,lfs,v)   ->
            match ValueCtx.getValue v with
            | None -> DisputedCtx (gfs,lfs,ValueCtx.setValue v None)
            | Some a -> 
                match fn a with
                | ValidCtx b                -> DisputedCtx (gfs,lfs,ValueCtx.map Some b)
                | DisputedCtx (gfs',lfs',b) -> 
                    let (gfs2,lfs2) = VCtx.applyFailures v (gfs,lfs) (gfs',lfs')
                    DisputedCtx (gfs2,lfs2,ValueCtx.map Some b)
                | RefutedCtx (gfs',lfs')    -> RefutedCtx (VCtx.applyFailures v (gfs,lfs) (gfs',lfs'))
        | RefutedCtx (gfs,lfs)   -> RefutedCtx (gfs,lfs)
        
    /// Unwraps a proven value.
    [<CustomOperation("qed")>]
    member this.Proven(c:VCtx<'F, ValueCtx<'A>>): VCtx<'F, 'A> = 
        c |> VCtx.map ValueCtx.getValue
    
    /// Adds a validation failure to the result and ends validation.
    [<CustomOperation("refute")>]
    member this.Refute(_, v, f) = this.Refute(v, f)

    member private this.Refute(v, f) = 
        match v with
        | Field (n, _)  -> RefutedCtx (List.empty, (Map.add [n] [f] Map.empty))
        | Global _      -> RefutedCtx ([f], Map.empty)
    
    /// Adds validation failures to the result and ends validation.
    [<CustomOperation("refuteMany")>]
    member this.RefuteMany(_, v, fs) = this.RefuteMany(v, fs)

    member private this.RefuteMany(v, fs) = 
        match v with
        | Field (n, _)  -> RefutedCtx (List.empty, (Map.add [n] fs Map.empty))
        | Global _      -> RefutedCtx (fs, Map.empty)

    // Adds a validation failure to the result and continues validation.
    [<CustomOperation("dispute")>]
    member this.Dispute(_, v, f) = this.Dispute(v, f)

    member private this.Dispute(v, f) = 
        match v with
        | Field (n, _)  -> DisputedCtx (List.empty, (Map.add [n] [f] Map.empty), v)
        | Global _      -> DisputedCtx ([f], Map.empty, v)

    /// Adds validation failures to the result and continues validation.
    [<CustomOperation("disputeMany")>]
    member this.DisputeMany(_, v, fs) = this.DisputeMany(v, fs)

    member private this.DisputeMany(v, fs) = 
        match v with
        | Field (n, _)  -> DisputedCtx (List.empty, (Map.add [n] fs Map.empty), v)
        | Global _      -> DisputedCtx (fs, Map.empty, v)

    /// Performs a validation using a given function and handles the result.
    /// If the result is `Some f`, a validation failure is added to the result and validation continues.
    /// If the result is `None`, validation continues with no failure.
    [<CustomOperation("disputeWith")>]
    member this.DisputeWith (c:VCtx<'F, ValueCtx<'A>>, fn:'A -> 'F option): VCtx<'F, ValueCtx<'A>> =
        this.Bind(c, fun v ->
            match fn (ValueCtx.getValue v) with
            | Some f   -> this.Dispute(v, f)
            | None     -> this.Return(v)
        )

    /// Similar to 'disputeWith' except that the given failure is added if the given function returns False.
    [<CustomOperation("disputeWithFact")>]
    member this.DisputeWithFact(c:VCtx<'F, ValueCtx<'A>>, f:'F, fn:'A -> bool): VCtx<'F, ValueCtx<'A>> = 
        this.DisputeWith(c, fun a -> 
            match fn a with
            | true  -> None
            | false -> Some f
        )

    /// Performs a validation using a given function and handles the result.
    /// If the result is `Error f`, a validation failure is added to the result and validation ends.
    /// If the result is `Ok b`, validation continues with the new value.
    [<CustomOperation("refuteWith")>]
    member this.RefuteWith(c:VCtx<'F, ValueCtx<'A>>, fn:'A -> Result<'B, 'F>): VCtx<'F, ValueCtx<'B>> =
        this.Bind(c, fun v ->
            match fn (ValueCtx.getValue v) with
            | Error f   -> this.Refute(v, f)
            | Ok b      -> this.Return(ValueCtx.setValue v b)
        )

    /// Performs a validation using a given function and handles the result.
    /// If the result is 'Invalid', the validation failures are added to the result and validation ends.
    /// If the result is `Valid b`, validation continues with the new value.
    [<CustomOperation("refuteWithProof")>]
    member this.RefuteWithProof(c:VCtx<'F, ValueCtx<'A>>, fn:'A -> Proof<'F, 'B>) = 
        this.Bind(c, fun v ->
            match v with
            | Global a      -> 
                match fn a with
                | Invalid (gfs, lfs)    -> RefutedCtx (gfs, lfs)
                | Valid b               -> this.Return(Global b)
            | Field (n, a)  ->
                match fn a with
                | Invalid (gfs, lfs)    -> RefutedCtx ([], Map.add [n] gfs lfs)
                | Valid b               -> this.Return(Field (n, b))
        )

[<AutoOpen>]
module Validation =
    let validation = VCtxBuilder()
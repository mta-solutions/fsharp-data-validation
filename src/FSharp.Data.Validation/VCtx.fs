namespace FSharp.Data.Validation
open System.Linq.Expressions
open System

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

    let internal mkElementName i =
        match mkName (sprintf "[%i]" i) with
        | None      -> raise (InvalidOperationException())
        | Some n    -> n

    let applyFailures (v:ValueCtx<'A>) (gfs:'F list,lfs:FailureMap<'F>) (gfs':'F list,lfs':FailureMap<'F>): 'F list * FailureMap<'F> =
        match v with
        | Element (i, _a) ->
            let n = mkElementName i
            let lfs2 = Utilities.mapKeys (fun ns -> n :: ns) lfs'
            let lfs3 = Map.add [n] gfs' Map.empty
            (gfs, Utilities.mergeFailures lfs <| Utilities.mergeFailures lfs3 lfs2)
        | Field (n, _a) ->
            let lfs2 = Utilities.mapKeys (fun ns -> n :: ns) lfs'
            let lfs3 = Map.add [n] gfs' Map.empty
            (gfs, Utilities.mergeFailures lfs <| Utilities.mergeFailures lfs3 lfs2)
        | Global _a -> (gfs @ gfs', Utilities.mergeFailures lfs lfs')

type VCtxBuilder() =
    member this.Bind(v:VCtx<'F, 'A>, fn:'A -> VCtx<'F, 'B>): VCtx<'F, 'B> =
        VCtx.bind fn v

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

    member this.Delay(fn:unit -> VCtx<'F, 'A>): unit -> VCtx<'F, 'A> = fn

    member this.Run(fn:unit -> VCtx<'F, 'A>): VCtx<'F, 'A> = fn()

    member this.Zero() = ValidCtx ()

    /// Performs some given validation using a 'Field' with a given name and value.
    [<CustomOperation("withField", MaintainsVariableSpaceUsingBind=true)>]
    member this.WithField(c:VCtx<'F, 'A>, n:Name, b:'B) = this.Bind(c, fun _ -> ValidCtx (Field (n, b)))

    /// Performs some given validation using a 'Field' with a given name and value.
    [<CustomOperation("withField", MaintainsVariableSpaceUsingBind=true)>]
    member this.WithField(c:VCtx<'F, 'A>, mn:Name option, b:'B) =
        match mn with
        | None -> this.WithValue(c, b)
        | Some n -> this.WithField(c, n, b)
    
    /// Performs some given validation using a 'Field' from a given selector.
    [<CustomOperation("withField", MaintainsVariableSpaceUsingBind=true)>]
    member this.WithField(c:VCtx<'F, 'A>, selector:Expression<Func<'B>>) =
        let exp = selector.Body :?> MemberExpression
        let mn = mkName exp.Member.Name
        let v = selector.Compile().Invoke()
        this.WithField(c, mn, v)
    
    /// Performs some given validation using a 'Field' from a given selector and value.
    [<CustomOperation("withField", MaintainsVariableSpaceUsingBind=true)>]
    member this.WithField(c:VCtx<'F, 'A>, selector:Expression<Func<'C>>, b:'B) =
        let exp = selector.Body :?> MemberExpression
        let mn = mkName exp.Member.Name
        this.WithField(c, mn, b)

    /// Performs some given validation using a 'Global' with a given value.
    [<CustomOperation("withValue", MaintainsVariableSpaceUsingBind=true)>]
    member this.WithValue(c, b) = this.Bind(c, fun _ -> ValidCtx (Global b))

    /// Maps a proven value with a given function.
    [<CustomOperation("optional", MaintainsVariableSpaceUsingBind=true)>]
    member this.Optional(c:VCtx<'F, ValueCtx<'A option>>, fn:'A -> VCtx<'F, ValueCtx<'B>>): VCtx<'F, ValueCtx<'B option>> =
        match c with
        | ValidCtx v                ->
            match ValueCtx.getValue v with
            | None -> ValidCtx (ValueCtx.setValue v None)
            | Some a ->
                match fn a with
                | ValidCtx b                -> ValidCtx (ValueCtx.map Some b)
                | DisputedCtx (gfs,lfs,b)   ->
                    let gfs',lfs' = VCtx.applyFailures v ([], Map.empty) (gfs,lfs)
                    DisputedCtx (gfs',lfs',ValueCtx.map Some b)
                | RefutedCtx (gfs,lfs)      -> RefutedCtx (VCtx.applyFailures v ([], Map.empty) (gfs,lfs))
        | DisputedCtx (gfs,lfs,v)   ->
            match ValueCtx.getValue v with
            | None -> DisputedCtx (gfs,lfs,ValueCtx.setValue v None)
            | Some a ->
                match fn a with
                | ValidCtx b                -> DisputedCtx (gfs,lfs,ValueCtx.map Some b)
                | DisputedCtx (gfs',lfs',b) ->
                    let gfs2,lfs2 = VCtx.applyFailures v (gfs,lfs) (gfs',lfs')
                    DisputedCtx (gfs2,lfs2,ValueCtx.map Some b)
                | RefutedCtx (gfs',lfs')    -> RefutedCtx (VCtx.applyFailures v (gfs,lfs) (gfs',lfs'))
        | RefutedCtx (gfs,lfs)   -> RefutedCtx (gfs,lfs)

    /// Performs a validation on each member of a list using a given function and handles the validation.
    [<CustomOperation("validateEach", MaintainsVariableSpaceUsingBind=true)>]
    member this.ValidateEach(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:int -> 'A -> VCtx<'F, ValueCtx<'B>>): VCtx<'F, ValueCtx<seq<'B>>> =
        this.Bind(c, fun v1 ->
            let xs = ValueCtx.getValue v1
            let ys = xs |> Seq.mapi (fun i x -> 
                match fn i x with
                | ValidCtx v2               -> ValidCtx (Element (i, (ValueCtx.getValue v2)))
                | DisputedCtx (gfs,lfs,v2)  -> 
                    let v2' = Element (i, ValueCtx.getValue v2)
                    let gfs',lfs' = VCtx.applyFailures v2' (List.empty, Map.empty) (gfs, lfs)
                    DisputedCtx (gfs', lfs', v2')
                | RefutedCtx (gfs,lfs)      -> 
                    let v2' = Element (i, ())
                    let gfs',lfs' = VCtx.applyFailures v2' (List.empty, Map.empty) (gfs, lfs)
                    RefutedCtx (gfs', lfs')
            )
            let appendToCtx d d' = d |> ValueCtx.map (fun zs -> Seq.append zs [ValueCtx.getValue d'])
            (ValidCtx (ValueCtx.setValue v1 Seq.empty), ys) ||> Seq.fold (fun acc x -> 
                match (acc, x) with
                | ValidCtx a, ValidCtx b                                -> 
                    ValidCtx (appendToCtx a b)
                | ValidCtx a, DisputedCtx (gfs',lfs',b)                 -> 
                    let gfs2,lfs2 = VCtx.applyFailures v1 (List.empty, Map.empty) (gfs',lfs')
                    DisputedCtx (gfs2,lfs2,appendToCtx a b)
                | ValidCtx a, RefutedCtx (gfs',lfs')                    -> 
                    RefutedCtx (VCtx.applyFailures v1 (List.empty, Map.empty) (gfs',lfs'))
                | DisputedCtx (gfs,lfs,a), ValidCtx b                   -> 
                    DisputedCtx (gfs,lfs,appendToCtx a b)
                | DisputedCtx (gfs,lfs,a), DisputedCtx (gfs',lfs',b)    -> 
                    let gfs2,lfs2 = VCtx.applyFailures v1 (gfs,lfs) (gfs',lfs')
                    DisputedCtx (gfs2,lfs2,appendToCtx a b)
                | DisputedCtx (gfs,lfs,_), RefutedCtx (gfs',lfs')       -> 
                    RefutedCtx (VCtx.applyFailures v1 (gfs,lfs) (gfs',lfs'))
                | RefutedCtx (gfs,lfs), ValidCtx _                      -> 
                    RefutedCtx (gfs,lfs)
                | RefutedCtx (gfs,lfs), DisputedCtx (gfs',lfs',b)       -> 
                    RefutedCtx (VCtx.applyFailures v1 (gfs,lfs) (gfs',lfs'))
                | RefutedCtx (gfs,lfs), RefutedCtx (gfs',lfs')          -> 
                    RefutedCtx (VCtx.applyFailures v1 (gfs,lfs) (gfs',lfs'))
            )
        )

     /// Performs a validation on each member of a list using a given function and handles the validation.
    [<CustomOperation("validateEach", MaintainsVariableSpaceUsingBind=true)>]
    member this.ValidateEach(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:'A -> VCtx<'F, ValueCtx<'B>>): VCtx<'F, ValueCtx<seq<'B>>> =
        this.ValidateEach(c, fun _ a -> fn a)

    /// Maps a proven value with a given function.
    [<CustomOperation("qed", MaintainsVariableSpaceUsingBind=true)>]
    member this.Proven(c:VCtx<'F, ValueCtx<'A>>, fn:'A -> 'B): VCtx<'F, 'B> =
        c |> VCtx.map (fun a -> ValueCtx.getValue a |> fn)

    /// Unwraps a proven value.
    [<CustomOperation("qed", MaintainsVariableSpaceUsingBind=true)>]
    member this.Proven(c:VCtx<'F, ValueCtx<'A>>): VCtx<'F, 'A> =
        c |> VCtx.map ValueCtx.getValue

    /// Adds a validation failure to the result and ends validation.
    [<CustomOperation("refute", MaintainsVariableSpaceUsingBind=true)>]
    member this.Refute(c: VCtx<'F, ValueCtx<'A>>, f) = this.Bind(c, fun v -> this.Refute(v, f))

    member private this.Refute(v: ValueCtx<'A>, f) = this.RefuteMany(v, NonEmptyList.singleton f)

    /// Adds validation failures to the result and ends validation.
    [<CustomOperation("refuteMany", MaintainsVariableSpaceUsingBind=true)>]
    member this.RefuteMany(c: VCtx<'F, ValueCtx<'A>>, fs:NonEmptyList<'F>) = this.Bind(c, fun v -> this.RefuteMany(v, fs))

    member private this.RefuteMany(v: ValueCtx<'A>, fs:NonEmptyList<'F>) =
        let fs' = NonEmptyList.toList fs
        match v with
        | Element (i, _)    -> RefutedCtx (List.empty, (Map.add [VCtx.mkElementName i] fs' Map.empty))
        | Field (n, _)      -> RefutedCtx (List.empty, (Map.add [n] fs' Map.empty))
        | Global _          -> RefutedCtx (fs', Map.empty)

    /// Performs a validation using a given function and handles the result.
    /// If the result is `Error f`, a validation failure is added to the result and validation ends.
    /// If the result is `Ok b`, validation continues with the new value.
    [<CustomOperation("refuteWith", MaintainsVariableSpaceUsingBind=true)>]
    member this.RefuteWith(c:VCtx<'F, ValueCtx<'A>>, fn:'A -> Result<'B, 'F>): VCtx<'F, ValueCtx<'B>> =
        this.Bind(c, fun v ->
            match fn (ValueCtx.getValue v) with
            | Error f   -> this.Refute(v, f)
            | Ok b      -> this.Return(ValueCtx.setValue v b)
        )

    /// Performs a validation using a given function and handles the result.
    /// If the result is `Error fs`, a validation failure is added to the result and validation ends.
    /// If the result is `Ok b`, validation continues with the new value.
    [<CustomOperation("refuteWithMany", MaintainsVariableSpaceUsingBind=true)>]
    member this.RefuteWith(c:VCtx<'F, ValueCtx<'A>>, fn:'A -> Result<'B, NonEmptyList<'F>>): VCtx<'F, ValueCtx<'B>> =
        this.Bind(c, fun v ->
            match fn (ValueCtx.getValue v) with
            | Error fs  -> this.RefuteMany(v, fs)
            | Ok b      -> this.Return(ValueCtx.setValue v b)
        )

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If the result of any element is `Error f`, a validation failure is added to the result and validation ends.
    /// If the result of all elements are `Ok b`, validation continues with the new value.
    [<CustomOperation("refuteEachWith", MaintainsVariableSpace=true)>]
    member this.RefuteEachWith(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:int -> 'A -> Result<'B, 'F>): VCtx<'F, ValueCtx<seq<'B>>> =
        this.ValidateEach(c, fun i a -> 
            match fn i a with
            | Ok b      -> ValidCtx (Global b)
            | Error f   -> RefutedCtx ([f], Map.empty)
        )

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If the result of any element is `Error f`, a validation failure is added to the result and validation ends.
    /// If the result of all elements are `Ok b`, validation continues with the new value.
    [<CustomOperation("refuteEachWith", MaintainsVariableSpace=true)>]
    member this.RefuteEachWith(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:'A -> Result<'B, 'F>): VCtx<'F, ValueCtx<seq<'B>>> =
        this.RefuteEachWith(c, fun _ a -> fn a)

    /// Performs a validation using a given function and handles the result.
    /// If the result is 'Invalid', the validation failures are added to the result and validation ends.
    /// If the result is `Valid b`, validation continues with the new value.
    [<CustomOperation("refuteWithProof", MaintainsVariableSpaceUsingBind=true)>]
    member this.RefuteWithProof(c:VCtx<'F, ValueCtx<'A>>, fn:'A -> Proof<'F, 'B>) =
        this.Bind(c, fun v ->
            match v with
            | Element (i, a)  ->
                match fn a with
                | Invalid (gfs, lfs)    -> RefutedCtx ([], Map.add [VCtx.mkElementName i] gfs lfs)
                | Valid b               -> this.Return(Element (i, b))
            | Field (n, a)  ->
                match fn a with
                | Invalid (gfs, lfs)    -> RefutedCtx ([], Map.add [n] gfs lfs)
                | Valid b               -> this.Return(Field (n, b))
            | Global a      ->
                match fn a with
                | Invalid (gfs, lfs)    -> RefutedCtx (gfs, lfs)
                | Valid b               -> this.Return(Global b)
        )

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If the result of any element is `Invalid`, a validation failures are added to the result and validation ends.
    /// If the result of all elements are `Valid b`, validation continues with the new value.
    [<CustomOperation("refuteEachWithProof", MaintainsVariableSpace=true)>]
    member this.RefuteEachWithProof(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:int -> 'A -> Proof<'F, 'B>): VCtx<'F, ValueCtx<seq<'B>>> =
        this.ValidateEach(c, fun i a -> 
            match fn i a with
            | Valid b           -> ValidCtx (Global b)
            | Invalid (gfs,lfs) -> RefutedCtx (gfs,lfs)
        )

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If the result of any element is `Error f`, a validation failure is added to the result and validation ends.
    /// If the result of all elements are `Ok b`, validation continues with the new value.
    [<CustomOperation("refuteEachWithProof", MaintainsVariableSpace=true)>]
    member this.RefuteEachWithProof(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:'A -> Proof<'F, 'B>): VCtx<'F, ValueCtx<seq<'B>>> =
        this.RefuteEachWithProof(c, fun _ a -> fn a)

    // Adds a validation failure to the result and continues validation.
    [<CustomOperation("dispute", MaintainsVariableSpace=true)>]
    member this.Dispute(c:VCtx<'F, ValueCtx<'A>>, f) = this.Bind(c, fun v -> this.Dispute(v, f))

    member private this.Dispute(v, f) = this.DisputeMany(v, NonEmptyList.singleton f)

    /// Adds validation failures to the result and continues validation.
    [<CustomOperation("disputeMany", MaintainsVariableSpace=true)>]
    member this.DisputeMany(c:VCtx<'F, ValueCtx<'A>>, fs:NonEmptyList<'F>) = this.Bind(c, fun v -> this.DisputeMany(v, fs))

    member private this.DisputeMany(v, fs:NonEmptyList<'F>) =
        let fs' = NonEmptyList.toList fs
        match v with
        | Element (i, _)    -> DisputedCtx (List.empty, (Map.add [VCtx.mkElementName i] fs' Map.empty), v)
        | Field (n, _)      -> DisputedCtx (List.empty, (Map.add [n] fs' Map.empty), v)
        | Global _          -> DisputedCtx (fs', Map.empty, v)

    /// Performs a validation using a given function and handles the result.
    /// If the result is `Some f`, a validation failure is added to the result and validation continues.
    /// If the result is `None`, validation continues with no failure.
    [<CustomOperation("disputeWith", MaintainsVariableSpace=true)>]
    member this.DisputeWith (c:VCtx<'F, ValueCtx<'A>>, fn:'A -> 'F option): VCtx<'F, ValueCtx<'A>> =
        this.DisputeWithMany(c, fun a ->
            match fn a with
            | None      -> []
            | Some f    -> [f]
        )

    /// Performs a validation using a given function and handles the result.
    /// If the result has one or more elements, the validation failures are added to the result and validation continues.
    /// Otherwise, validation continues normally.
    [<CustomOperation("disputeWithMany", MaintainsVariableSpace=true)>]
    member this.DisputeWithMany (c:VCtx<'F, ValueCtx<'A>>, fn:'A -> 'F list): VCtx<'F, ValueCtx<'A>> =
        this.Bind(c, fun v ->
            match fn (ValueCtx.getValue v) with
            | []        -> this.Return(v)
            | h :: t    -> this.DisputeMany(v, NonEmptyList.ofList h t)
        )

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If the result of any element is `Some f`, a validation failure is added to the result and validation continues.
    /// Otherwise, validation continues normally.
    [<CustomOperation("disputeAnyWith", MaintainsVariableSpace=true)>]
    member this.DisputeAnyWith(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:int -> 'A -> 'F option): VCtx<'F, ValueCtx<seq<'A>>> =
        this.DisputeAnyWithMany(c, fun i a -> 
            match fn i a with
            | None      -> []
            | Some f    -> [f]
        )

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If the result of any element is `Some f`, a validation failure is added to the result and validation continues.
    /// Otherwise, validation continues normally.
    [<CustomOperation("disputeAnyWith", MaintainsVariableSpace=true)>]
    member this.DisputeAnyWith(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:'A -> 'F option): VCtx<'F, ValueCtx<seq<'A>>> =
        this.DisputeAnyWith(c, fun _ a -> fn a)

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If the result of any element is `Some f`, a validation failure is added to the result and validation continues.
    /// Otherwise, validation continues normally.
    [<CustomOperation("disputeAnyWithMany", MaintainsVariableSpace=true)>]
    member this.DisputeAnyWithMany(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:int -> 'A -> 'F list): VCtx<'F, ValueCtx<seq<'A>>> =
        this.ValidateEach(c, fun i a -> 
            match fn i a with
            | []    -> ValidCtx (Global a)
            | fs    -> DisputedCtx (fs,Map.empty,Global a)
        )

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If the result of any element is `Some f`, a validation failure is added to the result and validation continues.
    /// Otherwise, validation continues normally.
    [<CustomOperation("disputeAnyWithMany", MaintainsVariableSpace=true)>]
    member this.DisputeAnyWithMany(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:'A -> 'F list): VCtx<'F, ValueCtx<seq<'A>>> =
        this.DisputeAnyWithMany(c, fun _ a -> fn a)

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If every element fails validation, all unique validation failure are added to the result and validation continues.
    /// Otherwise, no failures are added and validation continues normally.
    [<CustomOperation("disputeAllWith", MaintainsVariableSpace=true)>]
    member this.DisputeAllWith(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:int -> 'A -> 'F option): VCtx<'F, ValueCtx<#seq<'A>>> =
        this.DisputeAllWithMany(c, fun i a -> 
            match fn i a with
            | None      -> []
            | Some f    -> [f]
        )

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If every element fails validation, all unique validation failure are added to the result and validation continues.
    /// Otherwise, no failures are added and validation continues normally.
    [<CustomOperation("disputeAllWith", MaintainsVariableSpaceUsingBind=true)>]
    member this.DisputeAllWith(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:'A -> 'F option): VCtx<'F, ValueCtx<#seq<'A>>> =
        this.DisputeAllWith(c, fun _ a -> fn a)

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If every element fails validation, all unique validation failure are added to the result and validation continues.
    /// Otherwise, no failures are added and validation continues normally.
    [<CustomOperation("disputeAllWithMany", MaintainsVariableSpace=true)>]
    member this.DisputeAllWithMany(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:int -> 'A -> 'F list): VCtx<'F, ValueCtx<#seq<'A>>> =
        this.Bind(c, fun v ->
            let xs = Seq.mapi fn (ValueCtx.getValue v)
            let fs = xs |> Seq.filter (List.isEmpty)
            if Seq.length xs = Seq.length fs then // if every element fails validation
                let fs' = fs |> Seq.collect id |> Seq.distinct
                DisputedCtx (Seq.toList fs',Map.empty,v)
            else
                ValidCtx v
        )

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If the result of all of the elements is `Some f`, all validation failure are added to the result and validation continues.
    /// Otherwise, no failures are added and validation continues normally.
    [<CustomOperation("disputeAllWithMany", MaintainsVariableSpaceUsingBind=true)>]
    member this.DisputeAllWithMany(c:VCtx<'F, ValueCtx<#seq<'A>>>, fn:'A -> 'F list): VCtx<'F, ValueCtx<#seq<'A>>> =
        this.DisputeAllWithMany(c, fun _ a -> fn a)

    /// Similar to 'disputeWith' except that the given failure is added if the given function returns False.
    [<CustomOperation("disputeWithFact", MaintainsVariableSpace=true)>]
    member this.DisputeWithFact(c:VCtx<'F, ValueCtx<'A>>, f:'F, fn:'A -> bool): VCtx<'F, ValueCtx<'A>> =
        this.DisputeWith(c, fun a ->
            match fn a with
            | true  -> None
            | false -> Some f
        )

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If the result of any element is `false`, the given validation failure is added to the result and validation continues.
    /// Otherwise, validation continues normally.
    [<CustomOperation("disputeAnyWithFact", MaintainsVariableSpace=true)>]
    member this.DisputeAnyWithFact(c:VCtx<'F, ValueCtx<#seq<'A>>>, f:'F, fn:int -> 'A -> bool): VCtx<'F, ValueCtx<seq<'A>>> =
        this.DisputeAnyWith(c, fun i a -> 
            match fn i a with
            | true  -> None
            | false -> Some f
        )

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If the result of any element is `false`, the given validation failure is added to the result and validation continues.
    /// Otherwise, validation continues normally.
    [<CustomOperation("disputeAnyWithFact", MaintainsVariableSpace=true)>]
    member this.DisputeAnyWithFact(c:VCtx<'F, ValueCtx<#seq<'A>>>, f:'F, fn:'A -> bool): VCtx<'F, ValueCtx<seq<'A>>> =
        this.DisputeAnyWithFact(c, f, fun _ a -> fn a)

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If every element fails validation, all unique validation failure are added to the result and validation continues.
    /// Otherwise, no failures are added and validation continues normally.
    [<CustomOperation("disputeAllWithFact", MaintainsVariableSpace=true)>]
    member this.DisputeAllWithFact(c:VCtx<'F, ValueCtx<#seq<'A>>>, f:'F, fn:int -> 'A -> bool): VCtx<'F, ValueCtx<#seq<'A>>> =
        this.DisputeAllWith(c, fun i v ->
            match fn i v with
            | true      -> None
            | false     -> Some f
        )

    /// Performs a validation on each member of a list using a given function and handles the result.
    /// If every element fails validation, all unique validation failure are added to the result and validation continues.
    /// Otherwise, no failures are added and validation continues normally.
    [<CustomOperation("disputeAllWithFact", MaintainsVariableSpaceUsingBind=true)>]
    member this.DisputeAllWithFact(c:VCtx<'F, ValueCtx<#seq<'A>>>, f:'F, fn:'A -> bool): VCtx<'F, ValueCtx<#seq<'A>>> =
        this.DisputeAllWithFact(c, f, fun _ a -> fn a)

[<AutoOpen>]
module Validation =
    let validation = VCtxBuilder()

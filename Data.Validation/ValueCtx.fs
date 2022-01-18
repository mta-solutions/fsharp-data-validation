namespace Data.Validation

type ValueCtx<'a> =
    | Field of Name * 'a
    | Global of 'a

module ValueCtx =
    let getValue<'A> (v:ValueCtx<'A>): 'A =
        match v with
        | Field (_, a) -> a
        | Global a     -> a

    let setValue<'A, 'B> (v:ValueCtx<'A>) (b:'B): ValueCtx<'B> =
        match v with
        | Field (n, _a) -> Field (n, b)
        | Global _a     -> Global b
        
    let map (fn:'A -> 'B) (v:ValueCtx<'A>): ValueCtx<'B> =
        getValue v |> fn |> setValue v

    let bind (fn:'A -> ValueCtx<'B>) (v:ValueCtx<'A>): ValueCtx<'B> = 
        getValue v |> fn
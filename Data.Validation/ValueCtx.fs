[<AutoOpen>]
module Data.Validation.ValueCtx

type ValueCtx<'a> =
    | Field of Name * 'a
    | Global of 'a

let getValue<'A> (v:ValueCtx<'A>): 'A =
    match v with
    | Field (_, a) -> a
    | Global a     -> a

let setValue<'A, 'B> (v:ValueCtx<'A>) (b:'B): ValueCtx<'B> =
    match v with
    | Field (n, a) -> Field (n, b)
    | Global a     -> Global b
    
let map (fn:'A -> 'B) (v:ValueCtx<'A>): ValueCtx<'B> =
    getValue v |> fn |> setValue v

let bind (fn:'A -> ValueCtx<'B>) (v:ValueCtx<'A>): ValueCtx<'B> = 
    getValue v |> fn
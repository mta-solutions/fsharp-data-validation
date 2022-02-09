namespace FSharp.Data.Validation

type NonEmptyList<'a> =
    | Single of 'a
    | Cons of 'a * NonEmptyList<'a>

[<RequireQualifiedAccess>]
module NonEmptyList =
    let singleton x = Single x
    let cons x xs   = Cons (x, xs)
    let (><) y x    = cons y (singleton x)
    let (>-) x xs   = cons x xs

    let rec ofList x xs = 
        match xs with
        | []        -> Single x
        | x' :: xs' -> x >- (ofList x' xs')

    let rec append xs ys = 
        match xs with
        | Single x      -> Cons (x,ys)
        | Cons (x,xs')  -> Cons (x, append xs' ys)

    let appendList xs ys =
        match xs with 
        | []        -> ys
        | h :: t    -> append (ofList h t) ys

    let rec toList xs =
        match xs with
        | Single x -> [x]
        | Cons (x,xs') -> List.Cons (x, toList xs')

    //TODO: implement remaining functions.
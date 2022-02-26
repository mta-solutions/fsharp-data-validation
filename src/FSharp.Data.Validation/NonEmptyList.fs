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

    // Convert a NonEmptyList to a List
    let rec toList xs =
        match xs with
        | Single x      -> [x]
        | Cons (x, xs') -> List.Cons (x, toList xs')

    // Convert a List to NonEmptyList
    let rec ofList x xs = 
        match xs with
        | []     -> Single x
        | h :: t -> x >- (ofList h t)

    // Combine two NonEmptyLists, putting the first before the second
    let rec append xs ys = 
        match xs with
        | Single x      -> Cons (x, ys)
        | Cons (x, xs') -> Cons (x, append xs' ys)

    // Combine a List and a NonEmptyList, putting the first before the second
    // Results in a NonEmptyList
    let appendList xs ys =
        match xs with 
        | []     -> ys
        | h :: t -> append (ofList h t) ys

    // Combine two NonEmptyLists, putting the second before the first
    let rec prepend xs ys = 
        match ys with
        | Single y      -> Cons (y, xs)
        | Cons (y, ys') -> Cons (y, prepend ys' xs)

    // Combine a List and a NonEmptyList, putting the second before the first
    // Results in a NonEmptyList
    let prependList xs ys =
        match xs with 
        | []     -> ys
        | h :: t -> prepend (ofList h t) ys

    // Get the number of items in the NonEmptyList
    let rec length xs =
        match xs with
        | Single _      -> 1
        | Cons (_, xs') -> 1 + length xs'

    // Get the first item in the NonEmptyList
    let head xs =
        match xs with
        | Single x    -> x
        | Cons (x, _) -> x

    // Get the last item in the NonEmptyList
    let rec last xs =
        match xs with
        | Single x      -> x
        | Cons (_, xs') -> last xs'

    // Get the first n items in the NonEmptyList
    // If n is greater than the count, returns the original NonEmptyList
    // n must be greater than zero in order to return a result
    let take i xs =
        if i < 1 then None
        else
            match List.truncate i (toList xs) with
            | []     -> None // theoretically impossible
            | h :: t -> Some (ofList h t)

    // Get the first items in the NonEmptyList that meet the provided criteria
    let takeWhile f xs =
        match List.takeWhile f (toList xs) with
        | []     -> None
        | h :: t -> Some (ofList h t)

    // Get all items in the NonEmptyList except for the last one
    let init xs =
        match xs with
        | Single _ -> None
        | _        -> take (length xs - 1) xs

    // Get all items in the NonEmptyList except for the first one
    let tail xs =
        match xs with
        | Single _      -> None
        | Cons (_, xs') -> Some xs'

    // Remove the first n items in the NonEmptyList
    // n must be greater than zero and less than the count in order to return a result
    // Technically this function can throw exceptions (List.skip), but the initial checks will avoid that
    let skip i xs =
        let xs' = toList xs
        if i < 0 then None
        elif i >= (List.length xs') then None
        else
            match List.skip i xs' with
            | []     -> None // theoretically impossible
            | h :: t -> Some (ofList h t)

    // Remove the first items in the NonEmptyList that meet the provided criteria
    let skipWhile f xs =
        match List.skipWhile f (toList xs) with
        | []     -> None
        | h :: t -> Some (ofList h t)

    // Apply the provided function to each item in the NonEmptyList
    let rec map f xs =
        match xs with
        | Single x      -> Single (f x)
        | Cons (x, xs') -> Cons (f x, map f xs')

    // Apply the provided function to each element of the collection, threading an
    // accumulator through the computation, returning a final result
    let fold f a xs = List.fold f a (toList xs)

    // Determine if the provided item is in the NonEmptyList
    let contains x xs = List.contains x (toList xs)

    // Get the provided item from the NonEmptyList if it exists
    let rec find x xs =
        match xs with
        | Single x'      -> if x = x' then (Some x') else None
        | Cons (x', xs') -> if x = x' then (Some x') else find x xs'

    // Determine if any item in the NonEmptyList meets the provided criteria
    let exists f xs =
        match List.where f (toList xs) with
        | [] -> false
        | _  -> true

    // Determine if all items in the NonEmptyList meet the provided criteria
    let forall f xs = List.forall f (toList xs)

    // Gets the items in the NonEmptyList that meet the provided criteria
    let where f xs =
        match List.where f (toList xs) with
        | []     -> None
        | h :: t -> Some (ofList h t)

    // Get the item at the provided index (index zero) from the NonEmptyList
    // If the index is less than zero, get the item at the beginning of the NonEmptyList
    // If the index is greater than the (count - 1), get the item at the end of the NonEmptyList
    // Technically this function can throw exceptions (List.item), but the initial checks will avoid that
    let item i xs =
        let e = length xs - 1
        let i' = if i < 0 then 0 elif i > e then e else i  
        List.item i' (toList xs)

    // Add the provided item to the NonEmptyList at the provided index (index zero)
    // If the index is less than zero, the item will go at the beginning of the NonEmptyList
    // If the index is greater than the (count - 1), the item will go at the end of the NonEmptyList
    // Technically this function can throw exceptions (List.insertAt), but the initial checks will avoid that
    let insertAt i x xs =
        let e = length xs - 1
        let i' = if i < 0 then 0 elif i > e then e else i  
        match List.insertAt i' x (toList xs) with
        | []     -> Single x // theoretically impossible
        | h :: t -> ofList h t

    // Remove the item at the provided index (index zero) from the NonEmptyList
    // If the index is less than zero, the item at the beginning of the NonEmptyList will be removed
    // If the index is greater than the (count - 1), the item at the end of the NonEmptyList will be removed
    // If the NonEmptyList is a singleton, it will be returned as is
    // Technically this function can throw exceptions (List.removeAt), but the initial checks will avoid that
    let removeAt i xs =
        match xs with
        | Single _ -> xs
        | _        ->
            let e = length xs - 1
            let i' = if i < 0 then 0 elif i > e then e else i
            match List.removeAt i' (toList xs) with
            | []     -> xs // theoretically impossible
            | h :: t -> ofList h t

    // Replace the item at the provided index (index zero) in the NonEmptyList
    // If the index is less than zero, the item at the beginning of the NonEmptyList will be replaced
    // If the index is greater than the (count - 1), the item at the end of the NonEmptyList will be replaced
    // If the NonEmptyList is a singleton, the single value will be replaced
    // Technically this function can throw exceptions (List.updateAt), but the initial checks will avoid that
    let updateAt i x xs =
        match xs with
        | Single _ -> Single x
        | _        ->
            let e = length xs - 1
            let i' = if i < 0 then 0 elif i > e then e else i
            match List.updateAt i' x (toList xs) with
            | []     -> xs // theoretically impossible
            | h :: t -> ofList h t

    // Split the NonEmptyList into two NonEmptyLists, the first of which the provided constraint is true and the second is false
    // No result signifies that all the items in the NonEmptyList meet the constraint in the same way
    let partition f xs =
        match List.partition f (toList xs) with
        | ([], _)            -> None
        | (_, [])            -> None
        | (h :: t, h' :: t') -> Some (ofList h t, ofList h' t')

    // Split the NonEmptyList at the provided index (index zero)
    // If the index is less than zero, the item at the beginning of the NonEmptyList will become a singleton
    // If the index is greater than the (count - 1), the item at the end of the NonEmptyList will become a singleton
    // No result signifies that the NonEmptyList is a singleton
    // Technically this function can throw exceptions (List.splitAt), but the initial checks will avoid that
    let splitAt i xs =
        match xs with
        | Single _ -> None
        | _        ->
            let e = length xs - 1
            let i' = if i < 0 then 0 elif i > e then e else i
            match List.splitAt i' (toList xs) with
            | ([], _)            -> None // theoretically impossible
            | (_, [])            -> None // theoretically impossible
            | (h :: t, h' :: t') -> Some (ofList h t, ofList h' t')

    // Get the items in the NonEmptyList in reverse order
    // Technically this function can throw exceptions (List.head, List.tail), but the source NonEmptyList will avoid that 
    let reverse xs =
        let xs' = toList xs |> List.rev
        ofList (List.head xs') (List.tail xs')

    // Sort the NonEmptyList with Operators.compare
    let sort xs =
        match List.sort (toList xs) with
        | []     -> xs // theoretically impossible
        | h :: t -> ofList h t

    // Sort the NonEmptyList with the keys given in the projection
    let sortBy f xs =
        match List.sortBy f (toList xs) with
        | []     -> xs // theoretically impossible
        | h :: t -> ofList h t

    // Removes all duplicate items in the NonEmptyList
    let distinct xs =
        match List.distinct (toList xs) with
        | []     -> xs // theoretically impossible
        | h :: t -> ofList h t

    // Removes all duplicate items in the NonEmptyList, determined by the provided function
    let distinctBy f xs =
        match List.distinctBy f (toList xs) with
        | []     -> xs // theoretically impossible
        | h :: t -> ofList h t

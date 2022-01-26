module internal FSharp.Data.Validation.Utilities

// Given a sequence of options, return list of Some
let catOptions l = Seq.choose id l

let oks l =
    (Seq.empty, l) ||> Seq.fold (fun acc v ->
        match v with
        | Error _ -> acc
        | Ok a -> Seq.append acc [a]
    )

let errors l =
    (Seq.empty, l) ||> Seq.fold (fun acc v ->
        match v with
        | Error a -> Seq.append acc [a]
        | Ok _ -> acc
    )

let mergeFailures (a:FailureMap<'F>) (b:FailureMap<'F>): FailureMap<'F> =
    let bs = Map.toList b
    let rec mergeInto (am:FailureMap<'F>) (bs':(Name list * 'F list) list) =
        match bs' with
        | []            -> am
        | (ns, fs)::bs2 ->
            if Map.containsKey ns am then
                let vs = am[ns] @ fs
                mergeInto (Map.add ns vs am) bs2
            else
                mergeInto (Map.add ns fs am) bs2
    mergeInto a bs

let mapKeys (fn:'K -> 'L) (m:Map<'K, 'T>): Map<'L, 'T> =
    Map.toSeq m |> Seq.map (fun (k, v) -> (fn k, v)) |> Map.ofSeq

let testMatch f a1 a2 =
    match a1 = a2 with
    | true  -> None
    | false -> Some f

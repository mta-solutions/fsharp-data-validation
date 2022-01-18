namespace Data.Validation

type Proof<'F, 'A> =
    | Valid of 'A
    | Invalid of 'F list * FailureMap<'F>

module Proof =
    let bind fn p = 
        match p with
        | Invalid (gfs, lfs)    -> Invalid (gfs, lfs)
        | Valid a               -> Valid (fn a)

    let combine fn p1 p2 =
        match p1 with
        | Valid a1 ->
            match p2 with
            | Valid a2              -> Valid (fn a1 a2)
            | Invalid (gfs', lfs')  -> Invalid (gfs', lfs')
        | Invalid (gfs, lfs) ->
            match p2 with
            | Valid _               -> Invalid (gfs, lfs)
            | Invalid (gfs', lfs')  -> Invalid (gfs @ gfs', Utilities.mergeFailures lfs lfs')

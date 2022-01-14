[<AutoOpen>]
module Data.Validation.Types

open System

type Name = private { _value: string } with
    member public this.Value = this._value

let mkName (n:string): Name option =
    if String.IsNullOrEmpty(n.Trim()) then
        None
    else
        Some { _value = n.Trim()}

type FailureMap<'F> = Map<Name list, 'F list>
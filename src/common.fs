[<AutoOpen>]
module Common

open System.Collections.Generic
open System.Windows

let inline ($) a b = a b
let (?) (e: FrameworkElement) (name: string) = e.FindName name

// generic cache helper
type Cache<'K, 'V when 'K: equality>(creator) =
    let cache = Dictionary<'K, 'V>()

    member this.Pairs =
        cache |> Seq.toArray

    member this.Item key =
        let mutable value = Unchecked.defaultof<_>
        if cache.TryGetValue(key, &value) then value
        else
            let value = creator key
            cache.Add(key, value)
            value
[<AutoOpen>]
module Common

open System.Collections.Generic
open System.ComponentModel
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

// generic Cell with PropertyChanged support
type Cell<'T>(initial) =
    let mutable value: 'T = initial
    let event = Event<_, _>()

    member this.Value
        with get () = value
         and set v =
            value <- v
            event.Trigger(this, PropertyChangedEventArgs("Value"))

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = event.Publish

    static member Map (cell: Cell<_>) f =
        let res = Cell(f cell.Value)
        (cell :> INotifyPropertyChanged).PropertyChanged.Add(fun _ -> res.Value <- f cell.Value)
        res
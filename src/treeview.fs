module TreeView

open System
open System.Collections.Generic
open System.Windows
open System.Windows.Controls

// group an array by equal string prefixes
let private groupByPrefix data offset getPrefixLength =
    // group by first prefix
    let groups = Dictionary<string, List<_>>()

    for (_, text: string, _) as item in data do
        let prefix = text.Substring(offset, getPrefixLength text offset)

        match groups.TryGetValue(prefix) with
        | true, lst -> lst.Add(item)
        | _ ->
            let lst = List<_>()
            lst.Add(item)
            groups.Add(prefix, lst)

    // find largest prefix in each group
    groups
    |> Seq.toArray
    |> Array.map (fun p ->
        let prefix = p.Key
        let group = p.Value.ToArray()
        let (_, text, _) = group.[0]

        // find largest prefix in the group
        let rec findPrefix offset =
            let pn = getPrefixLength text offset
            if pn > 0 && group |> Array.forall (fun (_, ti, _) ->
                let pi = getPrefixLength ti offset
                pi = pn && String.Compare(text, offset, ti, offset, pn) = 0) then
                findPrefix (offset + pn)
            else
                offset

        let plength = findPrefix (offset + prefix.Length)

        // get prefix and all items
        text.Substring(0, plength), group)

type Group(prefix, size, items: obj array Lazy) =
    member this.Prefix = prefix
    member this.Size = size
    member this.Items = items.Value

let rec private buildTree items (prefix: string) getPrefixLength =
    // group by prefix
    let groups = groupByPrefix items prefix.Length getPrefixLength

    // convert to nodes
    let nodes =
        groups
        |> Array.map (fun (name, subitems) ->
            // standalone group?
            if subitems.Length = 1 then
                let (size, _, item) = subitems.[0]
                size, box item
            else
                let (_, text, _) = subitems.[0]

                let subnodes =
                    lazy
                    if Array.forall (fun (_, text, _) -> name = text) subitems then
                        subitems
                        |> Array.sortBy (fun (size, _, _) -> -size)
                        |> Array.map (fun (size, _, item) -> box item)
                    else
                        buildTree subitems name getPrefixLength

                let size = Array.sumBy (fun (size, _, _) -> size) subitems
                let item = Group(name, size, subnodes)

                size, box item)

    // return sorted nodes
    nodes
    |> Array.sortBy (fun (size, node) -> -size)
    |> Array.map (fun (size, node) -> node)

let getNodes data getPrefixLength =
    buildTree data "" getPrefixLength
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

// convert the dump output to a tree of nodes
let private buildTreeItem text (size: int) tag =
    TreeViewItem(
        Header = text + size.ToString(" (#,0)"),
        Tag = box tag,
        HorizontalContentAlignment = HorizontalAlignment.Left,
        VerticalContentAlignment = VerticalAlignment.Center)

let rec private buildTree items (prefix: string) getText getPrefixLength =
    // group by prefix
    let groups = groupByPrefix items prefix.Length getPrefixLength

    // convert to nodes
    let nodes =
        groups
        |> Array.map (fun (name, subitems) ->
            // standalone group?
            if subitems.Length = 1 then
                let (size, _, item) = subitems.[0]
                size, buildTreeItem (getText item) size item
            else
                let (_, text, _) = subitems.[0]

                let subnodes =
                    lazy
                    if Array.forall (fun (_, text, _) -> name = text) subitems then
                        subitems
                        |> Array.sortBy (fun (size, _, _) -> -size)
                        |> Array.map (fun (size, _, item) -> buildTreeItem (getText item) size item)
                    else
                        buildTree subitems name getText getPrefixLength

                let size = Array.sumBy (fun (size, _, _) -> size) subitems
                let item = buildTreeItem name size subnodes
                item.ItemsSource <- [|null|]

                size, item)

    // return sorted nodes
    nodes
    |> Array.sortBy (fun (size, node) -> -size)
    |> Array.map (fun (size, node) -> node)

type Binding(view: TreeView) =
    do view.AddHandler(TreeViewItem.ExpandedEvent,
        RoutedEventHandler(fun _ e ->
            let item = e.OriginalSource :?> TreeViewItem

            match item.Tag with
            | :? Lazy<TreeViewItem[]> as t ->
                item.ItemsSource <- t.Value
                item.Tag <- null
            | _ -> ()))

    member this.Update(data, getText, getPrefixLength) =
        let nodes = buildTree data "" getText getPrefixLength
        view.ItemsSource <- nodes
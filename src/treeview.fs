module TreeView

open System
open System.Collections.Generic
open System.Windows
open System.Windows.Controls

// group an array by equal string prefixes
let private groupByPrefix (pred: 'a -> string) data =
    // get keyed items
    let items = data |> Array.map (fun i -> pred i, i)

    // group by first letter
    let groups = Dictionary<char, List<_>>()

    for item in items do
        let key = fst item
        let prefix = if key.Length > 0 then key.[0] else char 0

        match groups.TryGetValue(prefix) with
        | true, lst -> lst.Add(item)
        | _ ->
            let lst = List<_>()
            lst.Add(item)
            groups.Add(prefix, lst)

    // find largest prefix in each group
    groups.Values
    |> Seq.toArray
    |> Array.map (fun lst ->
        let group = lst.ToArray()
        let first = fst group.[0]

        // find largest prefix in the group
        let prefix = ref 0

        while first.Length > !prefix && group |> Array.forall (fun (key, _) -> key.Length > !prefix && key.[!prefix] = first.[!prefix]) do
            incr prefix

        // get prefix and all items
        first.Substring(0, !prefix), group |> Array.map snd)

// convert the dump output to a tree of nodes
let private getNodeText name group (size: int) =
    name + (if group = "" then "" else " [" + group + "]") + size.ToString(" (#,0)")

let rec private buildTree items index =
    // group by the first letter
    let groups = groupByPrefix (fun (_, _, name: string) -> name.Substring(index)) items

    // convert to nodes
    let nodes = groups |> Array.map (fun (key, subitems) ->
        // standalone group?
        if subitems.Length = 1 then
            let (size, group, name) = subitems.[0]
            size, TreeViewItem(Header = getNodeText name group size)
        else
            let (_, _, first_name) = subitems.[0]
            let name = first_name.Substring(0, index + key.Length)
            let size = Array.sumBy (fun (size, _, _) -> size) subitems
            let subnodes =
                lazy
                if Array.forall (fun (_, _, name) -> name = first_name) subitems then
                    Array.map (fun (size, group, name) -> TreeViewItem(Header = getNodeText name group size)) subitems
                else
                    buildTree subitems (index + key.Length)

            size, TreeViewItem(Header = getNodeText name "" size, ItemsSource = [|TreeViewItem()|], Tag = subnodes))

    // return sorted nodes
    nodes
    |> Array.sortBy (fun (size, node) -> size)
    |> Array.rev
    |> Array.map (fun (size, node) -> node)

let bindToView (view: TreeView) data =
    let items = data
    let nodes = buildTree items 0
    view.ItemsSource <- nodes

    view.AddHandler(TreeViewItem.ExpandedEvent,
        RoutedEventHandler(fun _ e ->
            let item = e.OriginalSource :?> TreeViewItem

            if item.Tag <> null then
                let subnodes = (item.Tag :?> TreeViewItem array Lazy).Force()
                item.ItemsSource <- subnodes
                item.Tag <- null))
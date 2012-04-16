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
let private buildTreeItem text (size: int) =
    TreeViewItem(Header = text + size.ToString(" (#,0)"),
        HorizontalContentAlignment = HorizontalAlignment.Left, VerticalContentAlignment = VerticalAlignment.Center)

let rec private buildTree items (prefix: string) getText =
    // group by the first letter
    let groups = groupByPrefix (fun (_, text: string, _) ->
        assert (text.StartsWith(prefix))
        text.Substring(prefix.Length)) items

    // convert to nodes
    let nodes = groups |> Array.map (fun (key, subitems) ->
        // standalone group?
        if subitems.Length = 1 then
            let (size, _, item) = subitems.[0]
            size, buildTreeItem (getText item) size
        else
            let name = prefix + key
            let size = Array.sumBy (fun (size, _, _) -> size) subitems
            let subnodes =
                lazy
                if Array.forall (fun (_, text, _) -> name = text) subitems then
                    Array.map (fun (size, _, item) -> buildTreeItem (getText item) size) subitems
                else
                    buildTree subitems name getText

            let item = buildTreeItem name size
            item.ItemsSource <- [|null|]
            item.Tag <- subnodes

            size, item)

    // return sorted nodes
    nodes
    |> Array.sortBy (fun (size, node) -> size)
    |> Array.rev
    |> Array.map (fun (size, node) -> node)

type Binding(view: TreeView) =
    do view.AddHandler(TreeViewItem.ExpandedEvent,
        RoutedEventHandler(fun _ e ->
            let item = e.OriginalSource :?> TreeViewItem

            if item.Tag <> null then
                let subnodes = (item.Tag :?> TreeViewItem array Lazy).Force()
                item.ItemsSource <- subnodes
                item.Tag <- null))

    member this.Update(items, getText) =
        let nodes = buildTree items "" getText
        view.ItemsSource <- nodes
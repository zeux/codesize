module TreeView

open System
open System.Collections.Generic
open System.Windows
open System.Windows.Controls

// group an array by equal string prefixes
let private groupByPrefixSorted data offset getPrefixLength =
    let inline prefix (_, text, _) offset = getPrefixLength text offset
    let inline compare (_, text1, _) len1 (_, text2, _) len2 offset = len1 = len2 && String.Compare(text1, offset, text2, offset, len1) = 0
    let rec forall rs re pred = if rs = re then true else pred rs && forall (rs + 1) re pred

    let pls = data |> Array.map (fun d -> prefix d offset)

    seq {
        let start = ref 0
        let rend = ref 0

        while !start < data.Length do
            // find a range of values with the same prefix
            rend := !start + 1

            while !rend < data.Length && compare data.[!start] pls.[!start] data.[!rend] pls.[!rend] offset do
                incr rend

            // check if there is a larger common prefix
            let rec findPrefix start rend offset =
                let pn = prefix data.[start] offset
                if pn > 0 && forall (start + 1) rend (fun i -> compare data.[start] pn data.[i] (prefix data.[i] offset) offset) then
                    findPrefix start rend (offset + pn)
                else
                    offset

            let plength = findPrefix !start !rend (offset + pls.[!start]) - offset

            // yield prefix information
            yield !start, !rend - !start, plength
            start := !rend
    }

// convert the dump output to a tree of nodes
let private buildTreeItem text (size: int) =
    TreeViewItem(Header = text + size.ToString(" (#,0)"),
        HorizontalContentAlignment = HorizontalAlignment.Left, VerticalContentAlignment = VerticalAlignment.Center)

let rec private buildTree items (prefix: string) getText getPrefixLength =
    // group by the first letter
    let groups = groupByPrefixSorted items prefix.Length getPrefixLength

    // convert to nodes
    let nodes =
        groups
        |> Seq.map (fun (start, count, length) ->
            // standalone group?
            if count = 1 then
                let (size, _, item) = items.[start]
                size, buildTreeItem (getText item) size
            else
                let (_, text, _) = items.[start]
                let name = prefix + text.Substring(prefix.Length, length)

                let subitems = items.[start..start+count-1]
                let subnodes =
                    lazy
                    if Array.forall (fun (_, text, _) -> name = text) subitems then
                        Array.map (fun (size, _, item) -> buildTreeItem (getText item) size) subitems
                    else
                        buildTree subitems name getText getPrefixLength

                let size = Array.sumBy (fun (size, _, _) -> size) subitems
                let item = buildTreeItem name size
                item.ItemsSource <- [|null|]
                item.Tag <- subnodes

                size, item)
        |> Seq.toArray

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

    member this.Update(data, getText, getPrefixLength) =
        let items = data |> Array.sortBy (fun (_, text, _) -> text)
        let nodes = buildTree items "" getText getPrefixLength
        view.ItemsSource <- nodes
module UI.Session

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Windows
open System.Windows.Controls
open System.Windows.Documents
open System.Windows.Media

open Symbols

type DisplayData =
| Symbols = 0
| Files = 1

type DisplayView =
| Tree = 0
| List = 1

type FilterTextType =
| Words = 0
| Phrase = 1
| Regex = 2

type GroupTemplates =
| None = 0
| MergeAllTypes = 1
| MergeIncompatibleTypes = 2

type GroupPrefix =
| Letter = 0
| Word = 1

type Controls =
    { content: UserControl
    
      displayData: ComboBox
      displayView: ComboBox
      filterText: TextBox
      filterTextType: ComboBox
      filterSize: TextBox
      filterSections: ComboBox
      groupTemplates: ComboBox
      groupPrefix: ComboBox
      groupLineMerge: TextBox
      pathRemapSource: TextBox
      pathRemapTarget: TextBox
      symbolLocation: TextBox
      symbolLocationLink: Hyperlink
      symbolPanel: GroupBox
      contentsTree: TreeView
      contentsList: ListView

      editor: unit -> Editor.Window

      rebindToViewAgent: AsyncUI.SingleUpdateAgent
      updateSymbolLocationAgent: AsyncUI.SingleUpdateAgent
      jumpToAgent: AsyncUI.SingleUpdateAgent
    }

let protectUI = UI.Exception.protect

let getFilterTextFn typ (text: string) =
    let contains (s: string) (p: string) = s.IndexOf(p, StringComparison.InvariantCultureIgnoreCase) >= 0

    match typ with
    | _ when text = "" ->
        fun name -> true
    | FilterTextType.Words ->
            let words = text.Split(null)
            fun name -> words |> Array.forall (fun w -> contains name w)
    | FilterTextType.Phrase ->
        fun name -> contains name text
    | FilterTextType.Regex ->
        try
            let re = Regex(text, RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant)
            fun name -> re.IsMatch(name)
        with _ ->
            fun name -> true
    | _ ->
        failwithf "Unknown type %O" typ

let getFilterSymbolFn controls =
    let ft = getFilterTextFn (enum controls.filterTextType.SelectedIndex) controls.filterText.Text
    let fs =
        match UInt64.TryParse(controls.filterSize.Text) with
        | true, limit -> fun size -> size >= limit
        | _ -> fun size -> true
    let fg =
        let sections =
            controls.filterSections.Items
            |> Seq.cast<CheckBox>
            |> Seq.choose (fun cb -> if cb.IsChecked.Value then Some cb.Tag else None)
            |> Seq.cast<string>
            |> set
        fun section -> Set.contains section sections

    fun sym -> ft sym.name && fs sym.size && fg sym.section

let getFilterFileFn controls =
    let ft = getFilterTextFn (enum controls.filterTextType.SelectedIndex) controls.filterText.Text
    let fs =
        match UInt64.TryParse(controls.filterSize.Text) with
        | true, limit -> fun size -> size >= limit
        | _ -> fun size -> true

    fun file -> ft file.file && fs file.size

let templateConvertArgsImpl (name: string) (convarg: string -> string) =
    let res = StringBuilder()
    let arg = StringBuilder()
    let mutable count = 0

    let flusharg () =
        res.Append(convarg (arg.ToString())) |> ignore
        arg.Clear() |> ignore

    for c in name do
        match c with
        | '<' ->
            if count = 0 then res.Append(c) |> ignore
            count <- count + 1
        | '>' ->
            count <- count - 1
            if count = 0 then
                flusharg ()
                res.Append(c) |> ignore
        | ',' ->
            if count = 1 then flusharg ()
            if count <= 1 then res.Append(c) |> ignore
        | c when count = 0 ->
            res.Append(c) |> ignore
        | c when count = 1 ->
            arg.Append(c) |> ignore
        | _ -> ()

    // Mismatched braces (i.e. operator<)
    if arg.Length > 0 then flusharg ()

    res.ToString()

let templateConvertArgs convarg (name: string) =
    if name.IndexOf('<') = -1 then name
    else templateConvertArgsImpl name convarg

let getGroupSymbolFn controls =
    let gt: GroupTemplates = enum controls.groupTemplates.SelectedIndex

    match gt with
    | GroupTemplates.None ->
        id
    | GroupTemplates.MergeAllTypes ->
        templateConvertArgs (fun arg -> "?")
    | GroupTemplates.MergeIncompatibleTypes ->
        templateConvertArgs (fun arg ->
            let at = arg.Trim()
            if at.EndsWith("*") || at.EndsWith("* const") then "?*" else "?")
    | _ ->
        failwithf "Unknown type %O" gt

let getPrefixSymbolFn controls =
    let gp: GroupPrefix = enum controls.groupPrefix.SelectedIndex

    match gp with
    | GroupPrefix.Letter ->
        fun (name: string) offset -> if offset < name.Length then 1 else 0
    | GroupPrefix.Word ->
        let inline scan (name: string) offset pred =
            let mutable eo = offset
            while eo < name.Length && pred name.[eo] do eo <- eo + 1
            eo

        fun (name: string) offset ->
            if offset + 1 < name.Length then
                let c1 = name.[offset]
                let c2 = name.[offset + 1]

                if Char.IsUpper(c1) then
                    if Char.IsUpper(c2) then
                        scan name (offset + 2) Char.IsUpper - offset
                    elif Char.IsLower(c2) then
                        scan name (offset + 2) Char.IsLower - offset
                    else
                        1
                elif Char.IsLower(c1) then
                    scan name (offset + 1) Char.IsLower - offset
                else
                    1
            else
                name.Length - offset
    | _ ->
        failwithf "Unknown type %O" gp

let getPathRemapFn controls =
    let source = controls.pathRemapSource.Text
    let target = controls.pathRemapTarget.Text

    controls.pathRemapSource.BorderBrush <- controls.pathRemapTarget.BorderBrush

    let normalize path =
        (try Path.GetFullPath(path) with _ -> path).ToLowerInvariant().Replace('\\', '/')

    if source = "" then
        normalize
    else
        try
            let re = Regex(source, RegexOptions.IgnoreCase)
            fun path -> re.Replace(normalize path, target, 1)
        with _ ->
            controls.pathRemapSource.BorderBrush <- Brushes.Red
            normalize

let getLineRangesForFile file (lines: FileLine seq) mergeDistance =
    let ranges = Stack<FileLineRange>()

    for fl in lines |> Seq.sortBy (fun fl -> fl.lineBegin) do
        if ranges.Count > 0 && (let top = ranges.Peek() in fl.lineBegin - top.lineEnd <= mergeDistance) then
            let top = ranges.Pop()
            ranges.Push({ size = top.size + fl.size; file = file; lineBegin = top.lineBegin; lineEnd = fl.lineEnd })
        else
            ranges.Push({ size = fl.size; file = file; lineBegin = fl.lineBegin; lineEnd = fl.lineEnd })

    ranges.ToArray()

let getLineRanges (ess: ISymbolSource) mergeDistance pathRemap =
    let normalizePath = Cache(fun path -> pathRemap path)

    ess.FileLines
    |> Seq.groupBy (fun fl -> normalizePath.[fl.file])
    |> Seq.toArray
    |> Array.collect (fun (file, lines) -> getLineRangesForFile file lines mergeDistance)

let getStatsSymbol syms =
    // group symbols by section and find total size for each section
    let sections =
        syms
        |> Seq.groupBy (fun sym -> sym.section)
        |> Seq.map (fun (section, syms) -> section, syms |> Seq.sumBy (fun sym -> sym.size))
        |> Seq.toArray

    // get total size
    let totalSize = sections |> Array.sumBy snd

    // get section sizes
    let sizes =
        sections
        |> Array.sortBy (fun (section, size) -> ~~~size)
        |> Array.map (fun (section, size) -> (if section = "" then "other" else section) + size.ToString(": #,0"))

    // statistics string
    totalSize.ToString("#,0") + (if sections.Length = 0 then "" else " (" + String.concat ", " sizes + ")")

let getStatsFile files =
    let totalSize = files |> Array.sumBy (fun f -> f.size)

    totalSize.ToString("#,0")

let deactivateView (view: ItemsControl) =
    view.ItemsSource <- null
    view.Visibility <- Visibility.Hidden

let activateView (view: ItemsControl) =
    view.Visibility <- Visibility.Visible

let updateStatus controls value =
    controls.content.Tag <- value

let rebindToViewSymbolsAsync controls (ess: ISymbolSource) view switchView =
    async {
        let! token = Async.CancellationToken

        let filter = getFilterSymbolFn controls
        let group = getGroupSymbolFn controls
        let prefix = getPrefixSymbolFn controls

        do! AsyncUI.switchToWork ()

        let syms = ess.Symbols |> Array.filter (fun sym -> token.ThrowIfCancellationRequested(); filter sym)
        let stats = getStatsSymbol syms

        match view with
        | DisplayView.Tree ->
            let items = syms |> Array.map (fun sym -> token.ThrowIfCancellationRequested(); int sym.size, group sym.name, sym)
            let nodes = TreeView.getNodes items prefix

            do! AsyncUI.switchToUI ()

            switchView ()
            controls.contentsTree.ItemsSource <- nodes
        | DisplayView.List ->
            let items = syms |> Array.sortBy (fun sym -> token.ThrowIfCancellationRequested(); ~~~sym.size)

            do! AsyncUI.switchToUI ()

            switchView ()
            controls.contentsList.ItemsSource <- items
        | e -> failwithf "Unknown view %O" e

        updateStatus controls $ "Total: " + stats
    }

let rebindToViewFilesAsync controls (ess: ISymbolSource) view switchView =
    async {
        let! token = Async.CancellationToken

        let filter = getFilterFileFn controls
        let prefix = getPrefixSymbolFn controls
        let mergeDistance =
            match Int32.TryParse(controls.groupLineMerge.Text) with
            | true, value -> value
            | _ -> 1
        let pathRemap = getPathRemapFn controls

        do! AsyncUI.switchToWork ()

        let files =
            getLineRanges ess mergeDistance pathRemap
            |> Array.filter (fun file -> token.ThrowIfCancellationRequested(); filter file)
        let stats = getStatsFile files

        match view with
        | DisplayView.Tree ->
            let items = files |> Array.map (fun file -> token.ThrowIfCancellationRequested(); int file.size, file.file, file)
            let nodes = TreeView.getNodes items prefix

            do! AsyncUI.switchToUI ()

            switchView ()
            controls.contentsTree.ItemsSource <- nodes
        | DisplayView.List ->
            let items = files |> Array.sortBy (fun sym -> token.ThrowIfCancellationRequested(); ~~~sym.size)

            do! AsyncUI.switchToUI ()

            switchView ()
            controls.contentsList.ItemsSource <- items
        | e -> failwithf "Unknown view %O" e

        updateStatus controls $ "Total: " + stats
    }

let rebindToViewAsync controls (ess: ISymbolSource) =
    async {
        do! AsyncUI.switchToUI ()

        let view = enum controls.displayView.SelectedIndex
        let data = enum controls.displayData.SelectedIndex

        updateStatus controls $ "Filtering..."

        let switchView () =
            match view with
            | DisplayView.Tree ->
                deactivateView controls.contentsList
                activateView controls.contentsTree
            | DisplayView.List ->
                deactivateView controls.contentsTree
                activateView controls.contentsList
            | e -> failwithf "Unknown view %O" e

        try
            match data with
            | DisplayData.Symbols ->
                do! rebindToViewSymbolsAsync controls ess view switchView
            | DisplayData.Files ->
                do! rebindToViewFilesAsync controls ess view switchView
            | e -> failwithf "Unknown data %O" e
        with
        | :? OperationCanceledException -> ()
    }

let rebindToView controls ess =
    controls.rebindToViewAgent.Post(protectUI $ rebindToViewAsync controls ess)

let updateDisplayUI controls ess =
    controls.displayData.SelectionChanged.Add(fun _ -> rebindToView controls ess)
    controls.displayView.SelectionChanged.Add(fun _ -> rebindToView controls ess)

let updateFilterUI controls ess sections =
    controls.filterText.TextChanged.Add(fun _ -> rebindToView controls ess)
    controls.filterTextType.SelectionChanged.Add(fun _ -> rebindToView controls ess)
    controls.filterSize.TextChanged.Add(fun _ -> rebindToView controls ess)

    controls.filterSections.SelectionChanged.Add(fun e ->
        if controls.filterSections.SelectedIndex >= 0 then
            controls.filterSections.SelectedIndex <- -1)

    for section in sections do
        let sectionName = if section = "" then "<other>" else section
        let item = CheckBox(Content = section, IsChecked = Nullable<bool>(true), Tag = section)
        item.Unchecked.Add(fun _ -> rebindToView controls ess)
        item.Checked.Add(fun _ -> rebindToView controls ess)
        controls.filterSections.Items.Add(item) |> ignore

    controls.groupPrefix.SelectionChanged.Add(fun _ -> rebindToView controls ess)
    controls.groupTemplates.SelectionChanged.Add(fun _ -> rebindToView controls ess)
    controls.groupLineMerge.TextChanged.Add(fun _ -> rebindToView controls ess)

let updateSelectedSymbol controls (ess: ISymbolSource) (item: obj) =
    match item with
    | :? Symbol as sym ->
        controls.symbolPanel.Tag <- sym
        controls.symbolLocation.Text <- "resolving..."
        controls.symbolLocationLink.Tag <- null

        let pathRemap = getPathRemapFn controls

        async {
            do! AsyncUI.switchToWork ()

            let text, tag =
                match ess.GetFileLine sym.address with
                | Some (file, line) ->
                    let path = pathRemap file
                    sprintf "%s (%d)" path line, (if File.Exists(path) then box (path, line) else null)
                | None ->
                    "unknown", null

            do! AsyncUI.switchToUI ()

            controls.symbolLocation.Text <- text
            controls.symbolLocationLink.Tag <- tag
        } |> controls.updateSymbolLocationAgent.Post
    | _ ->
        controls.symbolPanel.Tag <- null
        controls.symbolLocation.Text <- ""
        controls.symbolLocationLink.Tag <- null

let updatePathRemap controls ess =
    match enum controls.displayData.SelectedIndex, enum controls.displayView.SelectedIndex with
    | DisplayData.Symbols, DisplayView.Tree ->
        updateSelectedSymbol controls ess controls.contentsTree.SelectedItem
    | DisplayData.Symbols, DisplayView.List ->
        updateSelectedSymbol controls ess controls.contentsList.SelectedItem
    | DisplayData.Files, _ ->
        rebindToView controls ess
    | _ -> ()

let jumpToSymbol controls (ess: ISymbolSource) (sym: Symbol) =
    let pathRemap = getPathRemapFn controls

    async {
        do! AsyncUI.switchToWork ()

        let fl =
            match ess.GetFileLine sym.address with
            | Some (file, line) ->
                let path = pathRemap file
                if File.Exists(path) then Some (path, line)
                else None
            | _ -> None

        do! AsyncUI.switchToUI ()

        match fl with
        | Some (file, line) -> (controls.editor ()).Open(file, line)
        | None -> ()
    }

let jumpToFile controls file =
    if File.Exists(file.file) then
        (controls.editor ()).Open(file.file, file.lineBegin, highlightRange = (file.lineBegin, file.lineEnd))

let jumpToItem controls ess (item: obj) =
    match item with
    | :? Symbol as sym -> controls.jumpToAgent.Post(jumpToSymbol controls ess sym)
    | :? FileLineRange as file -> jumpToFile controls file
    | _ -> ()

let updateSymbolUI controls (ess: ISymbolSource) =
    controls.symbolLocationLink.Click.Add(fun _ ->
        match controls.symbolLocationLink.Tag with
        | :? (string * int) as fl -> (controls.editor ()).Open(fst fl, snd fl)
        | _ -> ())

    controls.contentsTree.MouseDoubleClick.Add(fun _ ->
        jumpToItem controls ess controls.contentsTree.SelectedItem)

    controls.contentsList.MouseDoubleClick.Add(fun _ ->
        jumpToItem controls ess controls.contentsList.SelectedItem)

    controls.contentsTree.SelectedItemChanged.Add(fun _ ->
        updateSelectedSymbol controls ess controls.contentsTree.SelectedItem)

    controls.contentsList.SelectionChanged.Add(fun _ ->
        updateSelectedSymbol controls ess controls.contentsList.SelectedItem)

let updatePathUI controls ess =
    controls.pathRemapSource.TextChanged.Add(fun _ -> updatePathRemap controls ess)
    controls.pathRemapTarget.TextChanged.Add(fun _ -> updatePathRemap controls ess)

let bindToViewAsync controls (ess: ISymbolSource) =
    async {
        let sections = ess.Sections

        do! AsyncUI.switchToUI ()

        updateDisplayUI controls ess
        updateFilterUI controls ess sections
        updateSymbolUI controls ess
        updatePathUI controls ess

        do! rebindToViewAsync controls ess
    }
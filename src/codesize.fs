module codesize

open System
open System.Collections.Generic
open System.Drawing
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Documents
open System.Windows.Input
open System.Windows.Interop
open System.Windows.Media
open Microsoft.Win32

open Symbols

let window = Application.LoadComponent(Uri("src/ui/mainwindow.xaml", UriKind.Relative)) :?> Window
let editor = lazy Editor.Window()

module controls =
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

    let displayData = window?DisplayData :?> ComboBox 
    let displayView = window?DisplayView :?> ComboBox
    let filterText = window?FilterText :?> TextBox
    let filterTextType = window?FilterTextType :?> ComboBox 
    let filterSize = window?FilterSize :?> TextBox
    let filterSections = window?FilterSections :?> ComboBox
    let groupTemplates = window?GroupTemplates :?> ComboBox
    let groupPrefix = window?GroupPrefix :?> ComboBox
    let groupLineMerge = window?GroupLineMerge :?> TextBox
    let pathRemapSource = window?PathRemapSource :?> TextBox
    let pathRemapTarget = window?PathRemapTarget :?> TextBox
    let panelLoading = window?PanelLoading :?> Panel
    let labelLoading = window?LabelLoading :?> TextBlock
    let labelStatus = window?LabelStatus :?> TextBlock
    let symbolLocation = window?SymbolLocation :?> TextBox
    let symbolLocationLink = window?SymbolLocationLink :?> Hyperlink
    let symbolPanel = window?SymbolPanel :?> GroupBox
    let contentsTree = window?ContentsTree :?> TreeView
    let contentsList = window?ContentsList :?> ListView
    let welcomePanel = window?WelcomePanel :?> Grid

let getFilterTextFn typ (text: string) =
    let contains (s: string) (p: string) = s.IndexOf(p, StringComparison.InvariantCultureIgnoreCase) >= 0

    match typ with
    | _ when text = "" ->
        fun name -> true
    | controls.FilterTextType.Words ->
            let words = text.Split(null)
            fun name -> words |> Array.forall (fun w -> contains name w)
    | controls.FilterTextType.Phrase ->
        fun name -> contains name text
    | controls.FilterTextType.Regex ->
        try
            let re = Regex(text, RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant)
            fun name -> re.IsMatch(name)
        with _ ->
            fun name -> true
    | _ ->
        failwithf "Unknown type %O" typ

let getFilterSymbolFn () =
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

let getFilterFileFn () =
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

let getGroupSymbolFn () =
    let gt: controls.GroupTemplates = enum controls.groupTemplates.SelectedIndex

    match gt with
    | controls.GroupTemplates.None ->
        id
    | controls.GroupTemplates.MergeAllTypes ->
        templateConvertArgs (fun arg -> "?")
    | controls.GroupTemplates.MergeIncompatibleTypes ->
        templateConvertArgs (fun arg ->
            let at = arg.Trim()
            if at.EndsWith("*") || at.EndsWith("* const") then "?*" else "?")
    | _ ->
        failwithf "Unknown type %O" gt

let getPrefixSymbolFn () =
    let gp: controls.GroupPrefix = enum controls.groupPrefix.SelectedIndex

    match gp with
    | controls.GroupPrefix.Letter ->
        fun (name: string) offset -> if offset < name.Length then 1 else 0
    | controls.GroupPrefix.Word ->
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

let getPathRemapFn () =
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

let rebindToViewSymbolsAsync (ess: ISymbolSource) view switchView =
    async {
        let! token = Async.CancellationToken

        let filter = getFilterSymbolFn ()
        let group = getGroupSymbolFn ()
        let prefix = getPrefixSymbolFn ()

        do! AsyncUI.switchToWork ()

        let syms = ess.Symbols |> Array.filter (fun sym -> token.ThrowIfCancellationRequested(); filter sym)
        let stats = getStatsSymbol syms

        match view with
        | controls.DisplayView.Tree ->
            let items = syms |> Array.map (fun sym -> token.ThrowIfCancellationRequested(); int sym.size, group sym.name, sym)
            let nodes = TreeView.getNodes items prefix

            do! AsyncUI.switchToUI ()

            switchView ()
            controls.contentsTree.ItemsSource <- nodes
        | controls.DisplayView.List ->
            let items = syms |> Array.sortBy (fun sym -> token.ThrowIfCancellationRequested(); ~~~sym.size)

            do! AsyncUI.switchToUI ()

            switchView ()
            controls.contentsList.ItemsSource <- items
        | e -> failwithf "Unknown view %O" e

        controls.labelStatus.Text <- "Total: " + stats
    }

let rebindToViewFilesAsync (ess: ISymbolSource) view switchView =
    async {
        let! token = Async.CancellationToken

        let filter = getFilterFileFn ()
        let prefix = getPrefixSymbolFn ()
        let mergeDistance =
            match Int32.TryParse(controls.groupLineMerge.Text) with
            | true, value -> value
            | _ -> 1
        let pathRemap = getPathRemapFn ()

        do! AsyncUI.switchToWork ()

        let files =
            getLineRanges ess mergeDistance pathRemap
            |> Array.filter (fun file -> token.ThrowIfCancellationRequested(); filter file)
        let stats = getStatsFile files

        match view with
        | controls.DisplayView.Tree ->
            let items = files |> Array.map (fun file -> token.ThrowIfCancellationRequested(); int file.size, file.file, file)
            let nodes = TreeView.getNodes items prefix

            do! AsyncUI.switchToUI ()

            switchView ()
            controls.contentsTree.ItemsSource <- nodes
        | controls.DisplayView.List ->
            let items = files |> Array.sortBy (fun sym -> token.ThrowIfCancellationRequested(); ~~~sym.size)

            do! AsyncUI.switchToUI ()

            switchView ()
            controls.contentsList.ItemsSource <- items
        | e -> failwithf "Unknown view %O" e

        controls.labelStatus.Text <- "Total: " + stats
    }

let rebindToViewAsync (ess: ISymbolSource) =
    async {
        do! AsyncUI.switchToUI ()

        let view = enum controls.displayView.SelectedIndex
        let data = enum controls.displayData.SelectedIndex

        controls.labelStatus.Text <- "Filtering..."

        let switchView () =
            match view with
            | controls.DisplayView.Tree ->
                deactivateView controls.contentsList
                activateView controls.contentsTree
            | controls.DisplayView.List ->
                deactivateView controls.contentsTree
                activateView controls.contentsList
            | e -> failwithf "Unknown view %O" e

        try
            match data with
            | controls.DisplayData.Symbols ->
                do! rebindToViewSymbolsAsync ess view switchView
            | controls.DisplayData.Files ->
                do! rebindToViewFilesAsync ess view switchView
            | e -> failwithf "Unknown data %O" e
        with
        | :? OperationCanceledException -> ()
    }

let protectUI work =
    async {
        try do! work
        with e ->
            do! AsyncUI.switchToUI ()
            UI.Exception.showModal window e
    }
    
let rebindToViewAgent = AsyncUI.SingleUpdateAgent()

let rebindToView ess =
    rebindToViewAgent.Post(protectUI $ rebindToViewAsync ess)

let updateDisplayUI ess =
    controls.displayData.SelectionChanged.Add(fun _ -> rebindToView ess)
    controls.displayView.SelectionChanged.Add(fun _ -> rebindToView ess)

let updateFilterUI ess sections =
    controls.filterText.TextChanged.Add(fun _ -> rebindToView ess)
    controls.filterTextType.SelectionChanged.Add(fun _ -> rebindToView ess)
    controls.filterSize.TextChanged.Add(fun _ -> rebindToView ess)

    controls.filterSections.SelectionChanged.Add(fun e ->
        if controls.filterSections.SelectedIndex >= 0 then
            controls.filterSections.SelectedIndex <- -1)

    for section in sections do
        let sectionName = if section = "" then "<other>" else section
        let item = CheckBox(Content = section, IsChecked = Nullable<bool>(true), Tag = section)
        item.Unchecked.Add(fun _ -> rebindToView ess)
        item.Checked.Add(fun _ -> rebindToView ess)
        controls.filterSections.Items.Add(item) |> ignore

    controls.groupPrefix.SelectionChanged.Add(fun _ -> rebindToView ess)
    controls.groupTemplates.SelectionChanged.Add(fun _ -> rebindToView ess)
    controls.groupLineMerge.TextChanged.Add(fun _ -> rebindToView ess)

let updateSymbolLocationAgent = AsyncUI.SingleUpdateAgent()

let updateSelectedSymbol (ess: ISymbolSource) (item: obj) =
    match item with
    | :? Symbol as sym ->
        controls.symbolPanel.Tag <- sym
        controls.symbolLocation.Text <- "resolving..."
        controls.symbolLocationLink.Tag <- null

        let pathRemap = getPathRemapFn ()

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
        } |> updateSymbolLocationAgent.Post
    | _ ->
        controls.symbolPanel.Tag <- null
        controls.symbolLocation.Text <- ""
        controls.symbolLocationLink.Tag <- null

let updatePathRemap ess =
    match enum controls.displayData.SelectedIndex, enum controls.displayView.SelectedIndex with
    | controls.DisplayData.Symbols, controls.DisplayView.Tree ->
        updateSelectedSymbol ess controls.contentsTree.SelectedItem
    | controls.DisplayData.Symbols, controls.DisplayView.List ->
        updateSelectedSymbol ess controls.contentsList.SelectedItem
    | controls.DisplayData.Files, _ ->
        rebindToView ess
    | _ -> ()

let jumpToAgent = AsyncUI.SingleUpdateAgent()

let jumpToSymbol (ess: ISymbolSource) (sym: Symbol) =
    let pathRemap = getPathRemapFn ()

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
        | Some (file, line) -> editor.Value.Open(file, line)
        | None -> ()
    }

let jumpToFile file =
    if File.Exists(file.file) then
        editor.Value.Open(file.file, file.lineBegin, highlightRange = (file.lineBegin, file.lineEnd))

let jumpToItem ess (item: obj) =
    match item with
    | :? Symbol as sym -> jumpToAgent.Post(jumpToSymbol ess sym)
    | :? FileLineRange as file -> jumpToFile file
    | _ -> ()

let updateSymbolUI (ess: ISymbolSource) =
    controls.symbolLocationLink.Click.Add(fun _ ->
        match controls.symbolLocationLink.Tag with
        | :? (string * int) as fl -> editor.Value.Open(fst fl, snd fl)
        | _ -> ())

    controls.contentsTree.MouseDoubleClick.Add(fun _ ->
        jumpToItem ess controls.contentsTree.SelectedItem)

    controls.contentsList.MouseDoubleClick.Add(fun _ ->
        jumpToItem ess controls.contentsList.SelectedItem)

    controls.contentsTree.SelectedItemChanged.Add(fun _ ->
        updateSelectedSymbol ess controls.contentsTree.SelectedItem)

    controls.contentsList.SelectionChanged.Add(fun _ ->
        updateSelectedSymbol ess controls.contentsList.SelectedItem)

let updatePathUI ess =
    controls.pathRemapSource.TextChanged.Add(fun _ -> updatePathRemap ess)
    controls.pathRemapTarget.TextChanged.Add(fun _ -> updatePathRemap ess)

let bindToViewAsync (ess: ISymbolSource) =
    async {
        let sections = ess.Sections

        do! AsyncUI.switchToUI ()

        updateDisplayUI ess
        updateFilterUI ess sections
        updateSymbolUI ess
        updatePathUI ess

        do! rebindToViewAsync ess
    }

let getSymbolSource path =
    match Path.GetExtension(path).ToLower() with
    | ".elf" ->
        ElfSymbolSource(path) :> ISymbolSource
    | ".self" ->
        SelfSymbolSource(path) :> ISymbolSource
    | ".pdb" ->
        PdbSymbolSource(path) :> ISymbolSource
    | e ->
        failwithf "Unknown extension %s" e

let getRecentFileList () =
    match UI.Settings.current.["RecentFiles"].Value with
    | :? string as s ->
        s.Split([|'*'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.filter (fun path -> File.Exists(path))
    | _ -> [||]

let updateRecentFileList path =
    let list = getRecentFileList ()

    UI.Settings.current.["RecentFiles"].Value <-
        list
        |> Seq.filter (fun p -> Path.GetFullPath(p).ToLower() <> Path.GetFullPath(path).ToLower())
        |> Seq.append [path]
        |> Seq.truncate 10
        |> String.concat "*"

let loadFile path =
    window.IsEnabled <- false
    window.Title <- sprintf "%s - %s" window.Title path
    controls.panelLoading.Visibility <- Visibility.Visible
    controls.labelLoading.Text <- sprintf "Loading %s..." path

    protectUI $ async {
        try
            let ess = getSymbolSource path
            do! bindToViewAsync ess

            controls.welcomePanel.Visibility <- Visibility.Hidden
            updateRecentFileList path
        finally
            async {
                do! AsyncUI.switchToUI ()
                window.IsEnabled <- true
                controls.panelLoading.Visibility <- Visibility.Hidden
            } |> Async.Start
    } |> Async.Start

let getOpenFileName () =
    let dlg = OpenFileDialog(Filter = "Supported files|*.elf;*.self;*.pdb", CheckFileExists = true)
    let res = dlg.ShowDialog(window)
    if res.HasValue && res.Value then
        Some dlg.FileName
    else
        None

type RecentFile(path) =
    member this.FileName = Path.GetFileName(path)
    member this.Path = path

    member this.Icon =
        let ico = Icon.ExtractAssociatedIcon(path)
        let options = Imaging.BitmapSizeOptions.FromEmptyOptions()
        let source = Imaging.CreateBitmapSourceFromHIcon(ico.Handle, Int32Rect.Empty, options)
        source.Freeze()
        source :> ImageSource

window.Loaded.Add(fun _ ->
    if Environment.GetCommandLineArgs().Length > 1 then
        loadFile $ Environment.GetCommandLineArgs().[1])

type MainWindow() =
    inherit Window()

    member this.OpenFileDialog (sender: obj) (e: RoutedEventArgs) =
        match getOpenFileName () with
        | Some path -> loadFile path
        | None -> ()

    member this.RecentFiles =
        getRecentFileList () |> Array.map (fun path -> RecentFile path)

    member this.LoadRecentFile (sender: obj) (e: MouseButtonEventArgs) =
        match (sender :?> FrameworkElement).Tag with
        | :? RecentFile as f -> loadFile f.Path
        | _ -> ()
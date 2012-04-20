open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Windows
open System.Windows.Controls
open System.Windows.Documents
open Microsoft.Win32

open Symbols

[<STAThread>] do ()

let getSymbolSource path =
    match Path.GetExtension(path).ToLower() with
    | ".elf" ->
        ElfSymbolSource(path) :> ISymbolSource
    | ".self" ->
        SelfSymbolSource(path) :> ISymbolSource
    | e ->
        failwithf "Unknown extension %s" e

let app = Application(ShutdownMode = ShutdownMode.OnMainWindowClose)

let window = Application.LoadComponent(Uri("src/ui/mainwindow.xaml", UriKind.Relative)) :?> Window

module controls =
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
    let tabControl = window?TabControl :?> TabControl
    let tabLoading = window?TabLoading :?> TabItem
    let tabContents = window?TabContents :?> TabItem
    let labelLoading = window?LabelLoading :?> TextBlock
    let labelStatus = window?LabelStatus :?> TextBlock
    let symbolName = window?SymbolName :?> TextBox
    let symbolLocation = window?SymbolLocation :?> TextBox
    let symbolLocationLink = window?SymbolLocationLink :?> Hyperlink
    let symbolSize = window?SymbolSize :?> TextBox
    let symbolAddress = window?SymbolAddress :?> TextBox
    let contentsTree = window?ContentsTree :?> TreeView
    let contentsList = window?ContentsList :?> ListView

let treeViewBinding = TreeView.Binding(controls.contentsTree)

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
        templateConvertArgs (fun arg -> "T")
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

let getSymbolText sym =
    sym.name + (if sym.section = "" then "" else " [" + sym.section + "]")

let getStats syms =
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

let rebindToViewAgent = AsyncUI.SingleUpdateAgent()

let deactivateView (view: ItemsControl) =
    view.ItemsSource <- null
    view.Visibility <- Visibility.Hidden

let activateView (view: ItemsControl) =
    view.Visibility <- Visibility.Visible

let rebindToViewAsync (ess: ISymbolSource) =
    async {
        do! AsyncUI.switchToUI ()

        let filter = getFilterSymbolFn ()
        let group = getGroupSymbolFn ()
        let prefix = getPrefixSymbolFn ()
        let view = enum controls.displayView.SelectedIndex

        match view with
        | controls.DisplayView.Tree ->
            deactivateView controls.contentsList
            activateView controls.contentsTree
        | controls.DisplayView.List ->
            deactivateView controls.contentsTree
            activateView controls.contentsList
        | e -> failwithf "Unknown view %O" e

        controls.labelStatus.Text <- "Filtering..."

        try
            do! AsyncUI.switchToWork ()

            let! token = Async.CancellationToken

            let syms = ess.Symbols |> Array.filter (fun sym -> token.ThrowIfCancellationRequested(); filter sym)
            let stats = getStats syms

            match view with
            | controls.DisplayView.Tree ->
                let items = syms |> Array.map (fun sym -> token.ThrowIfCancellationRequested(); int sym.size, group sym.name, sym)

                do! AsyncUI.switchToUI ()
                treeViewBinding.Update(items, getSymbolText, prefix)
            | controls.DisplayView.List ->
                let items = syms |> Array.sortBy (fun sym -> token.ThrowIfCancellationRequested(); ~~~sym.size)

                do! AsyncUI.switchToUI ()
                controls.contentsList.ItemsSource <- items
            | e -> failwithf "Unknown view %O" e

            controls.labelStatus.Text <- "Total: " + stats
        with
        | :? OperationCanceledException -> ()
    }
    
let rebindToView ess =
    rebindToViewAgent.Post(rebindToViewAsync ess)

let updateDisplayUI ess =
    controls.displayView.SelectionChanged.Add(fun _ -> rebindToView ess)

let updateFilterUI ess =
    controls.filterText.TextChanged.Add(fun _ -> rebindToView ess)
    controls.filterTextType.SelectionChanged.Add(fun _ -> rebindToView ess)
    controls.filterSize.TextChanged.Add(fun _ -> rebindToView ess)

    controls.filterSections.SelectionChanged.Add(fun e ->
        if controls.filterSections.SelectedIndex >= 0 then
            controls.filterSections.SelectedIndex <- -1)

    for section in ess.Sections do
        let sectionName = if section = "" then "<other>" else section
        let item = CheckBox(Content = section, IsChecked = Nullable<bool>(true), Tag = section)
        item.Unchecked.Add(fun _ -> rebindToView ess)
        item.Checked.Add(fun _ -> rebindToView ess)
        controls.filterSections.Items.Add(item) |> ignore

    controls.groupPrefix.SelectionChanged.Add(fun _ -> rebindToView ess)
    controls.groupTemplates.SelectionChanged.Add(fun _ -> rebindToView ess)

let updateSymbolLocationAgent = AsyncUI.SingleUpdateAgent()

let editor = lazy Editor.Window()

let updateSelectedSymbol (ess: ISymbolSource) (item: obj) =
    match item with
    | :? Symbol as sym ->
        controls.symbolName.Text <- sym.name
        controls.symbolLocation.Text <- "resolving..."
        controls.symbolLocationLink.Tag <- null
        controls.symbolAddress.Text <- "0x" + sym.address.ToString("x")
        controls.symbolSize.Text <- sym.size.ToString("#,0")

        async {
            do! AsyncUI.switchToWork ()

            let text, tag =
                match ess.GetFileLine sym.address with
                | Some (file, line) ->
                    sprintf "%s (%d)" file line, (if File.Exists(file) then box (file, line) else null)
                | None ->
                    "unknown", null

            do! AsyncUI.switchToUI ()

            controls.symbolLocation.Text <- text
            controls.symbolLocationLink.Tag <- tag
        } |> updateSymbolLocationAgent.Post
    | _ ->
        controls.symbolName.Text <- ""
        controls.symbolLocation.Text <- ""
        controls.symbolLocationLink.Tag <- null
        controls.symbolSize.Text <- ""
        controls.symbolAddress.Text <- ""

let jumpToCurrentFile () =
    match controls.symbolLocationLink.Tag with
    | :? (string * int) as fl -> editor.Value.Open(fst fl, snd fl)
    | _ -> ()

let updateSymbolUI (ess: ISymbolSource) =
    controls.symbolLocationLink.Click.Add(fun _ -> jumpToCurrentFile ())

    // Ideally we should do resolve ourselves here, but it's too much work for now
    controls.contentsTree.MouseDoubleClick.Add(fun _ -> jumpToCurrentFile ())
    controls.contentsList.MouseDoubleClick.Add(fun _ -> jumpToCurrentFile ())

    controls.contentsTree.SelectedItemChanged.Add(fun _ ->
        let item = controls.contentsTree.SelectedItem :?> TreeViewItem
        updateSelectedSymbol ess (if item = null then null else item.Tag))

    controls.contentsList.SelectionChanged.Add(fun _ ->
        updateSelectedSymbol ess controls.contentsList.SelectedItem)

let bindToViewAsync (ess: ISymbolSource) =
    async {
        do! AsyncUI.switchToUI ()

        updateDisplayUI ess
        updateFilterUI ess
        updateSymbolUI ess

        do! rebindToViewAsync ess
    }

window.Loaded.Add(fun _ ->
    let path =
        if Environment.GetCommandLineArgs().Length > 1 then
            Environment.GetCommandLineArgs().[1]
        else
            let dlg = OpenFileDialog(Filter = "Executable files|*.elf;*.self")
            let res = dlg.ShowDialog(window)
            if res.HasValue && res.Value then
                dlg.FileName
            else
                exit 0

    controls.labelLoading.Text <- sprintf "Loading %s..." path

    async {
        let ess = getSymbolSource path
        do! bindToViewAsync ess
        controls.tabContents.IsSelected <- true
    } |> Async.Start)

app.Run(window) |> ignore

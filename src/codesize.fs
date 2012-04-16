open System
open System.Text
open System.Text.RegularExpressions
open System.Windows
open System.Windows.Controls
open System.Windows.Threading
open Microsoft.Win32

open Symbols

[<STAThread>] do ()

let (?) (e: FrameworkElement) (name: string) = e.FindName name

let exepath = AppDomain.CurrentDomain.BaseDirectory
let config =
    ["tools/nm/@path", exepath + @"\nm.exe"
     "tools/demangle/@path", exepath + @"\demangle.exe"
     "tools/objdump/@path", exepath + @"\objdump.exe"]
    |> Map.ofList

let window = Application.LoadComponent(Uri("src/ui/mainwindow.xaml", UriKind.Relative)) :?> Window

module controls =
    type FilterTextType =
    | Words = 0
    | Phrase = 1
    | Regex = 2

    type GroupTemplates =
    | None = 0
    | MergeAllTypes = 1
    | MergeIncompatibleTypes = 2

    let treeView = window?TreeView :?> TreeView
    let filterText = window?FilterText :?> TextBox
    let filterTextType = window?FilterTextType :?> ComboBox 
    let filterSize = window?FilterSize :?> TextBox
    let filterSections = window?FilterSections :?> ComboBox
    let groupTemplates = window?GroupTemplates :?> ComboBox
    let tabControl = window?TabControl :?> TabControl
    let tabLoading = window?TabLoading :?> TabItem
    let tabTreeView = window?TabTreeView :?> TabItem
    let labelLoading = window?LabelLoading :?> Label
    let labelStatus = window?LabelStatus :?> Label

let app = Application()
let context = DispatcherSynchronizationContext(app.Dispatcher)

let treeViewBinding = TreeView.Binding(controls.treeView)

let getFilterTextFn typ (text: string) =
    let contains (s: string) (p: string) = s.IndexOf(p, StringComparison.InvariantCultureIgnoreCase) >= 0

    match typ with
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
        fun name -> true

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
    | controls.GroupTemplates.MergeAllTypes ->
        templateConvertArgs (fun arg -> "T")
    | controls.GroupTemplates.MergeIncompatibleTypes ->
        templateConvertArgs (fun arg ->
            let at = arg.Trim()
            if at.EndsWith("*") || at.EndsWith("* const") then "?*" else "?")
    | _ ->
        id

let rebindToViewToken = ref (new Threading.CancellationTokenSource())

let getSymbolText sym =
    sym.name + (if sym.section = "" then "" else " [" + sym.section + "]")

let rebindToView syms =
    let filter = getFilterSymbolFn ()
    let group = getGroupSymbolFn ()

    rebindToViewToken.Value.Cancel()
    rebindToViewToken := new Threading.CancellationTokenSource()

    controls.labelStatus.Content <- "Filtering..."

    let token = rebindToViewToken.Value.Token

    Async.Start(async {
        try
            let fs =
                syms
                |> Array.filter (fun sym -> token.ThrowIfCancellationRequested(); filter sym)
                |> Array.map (fun sym -> token.ThrowIfCancellationRequested(); int sym.size, group sym.name, sym)

            do! Async.SwitchToContext context
            treeViewBinding.Update(fs, getSymbolText)
            controls.labelStatus.Content <- ""
        with e -> ()
        }, token)
    
let bindToView syms =
    controls.filterText.TextChanged.Add(fun _ -> rebindToView syms)
    controls.filterTextType.SelectionChanged.Add(fun _ -> rebindToView syms)
    controls.filterSize.TextChanged.Add(fun _ -> rebindToView syms)

    controls.filterSections.SelectionChanged.Add(fun e ->
        if controls.filterSections.SelectedIndex >= 0 then
            controls.filterSections.SelectedIndex <- -1)

    for section in syms |> Seq.map (fun sym -> sym.section) |> set do
        let sectionName = if section = "" then "<other>" else section
        let item = CheckBox(Content = section, IsChecked = Nullable<bool>(true), Tag = section)
        item.Unchecked.Add(fun _ -> rebindToView syms)
        item.Checked.Add(fun _ -> rebindToView syms)
        controls.filterSections.Items.Add(item) |> ignore

    controls.groupTemplates.SelectionChanged.Add(fun _ -> rebindToView syms)

    treeViewBinding.Update(syms |> Array.map (fun sym -> int sym.size, sym.name, sym), getSymbolText)

app.Startup.Add(fun _ ->
    let path =
        if Environment.GetCommandLineArgs().Length > 1 then
            Environment.GetCommandLineArgs().[1]
        else
            let dlg = OpenFileDialog(DefaultExt = ".elf", Filter = "ELF files (.elf)|*.elf")
            let res = dlg.ShowDialog(window)
            if res.HasValue && res.Value then
                dlg.FileName
            else
                exit 0

    controls.labelLoading.Content <- sprintf "Loading %s..." path

    async {
        let ess = ElfSymbolSource(path, fun key -> Map.find key config) :> ISymbolSource
        let symbols = ess.Symbols |> Seq.toArray
        do! Async.SwitchToContext context
        bindToView symbols
        controls.tabTreeView.IsSelected <- true
    } |> Async.Start)

app.Run(window) |> ignore

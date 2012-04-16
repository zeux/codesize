open System
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
    ["tools/nm/@path", exepath + @"\nm.exe";
     "tools/demangle/@path", exepath + @"\demangle.exe"]
    |> Map.ofList

let window = Application.LoadComponent(Uri("src/ui/mainwindow.xaml", UriKind.Relative)) :?> Window

module controls =
    let treeView = window?TreeView :?> TreeView
    let filterText = window?FilterText :?> TextBox
    let filterTextType = window?FilterTextType :?> ComboBox 
    let filterSize = window?FilterSize :?> TextBox
    let filterSections = window?FilterSections :?> ComboBox
    let tabControl = window?TabControl :?> TabControl
    let tabLoading = window?TabLoading :?> TabItem
    let tabTreeView = window?TabTreeView :?> TabItem
    let labelLoading = window?LabelLoading :?> Label

let app = Application()
let context = DispatcherSynchronizationContext(app.Dispatcher)

let treeViewBinding = TreeView.Binding(controls.treeView)

let getFilterText typ (text: string) =
    let contains (s: string) (p: string) = s.IndexOf(p, StringComparison.InvariantCultureIgnoreCase) >= 0

    match typ with
    | 0 ->
        let words = text.Split(null)
        fun name -> words |> Array.forall (fun w -> contains name w)
    | 1 ->
        fun name -> contains name text
    | 2 ->
        try
            let re = Regex(text, RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant)
            fun name -> re.IsMatch(name)
        with _ ->
            fun name -> true
    | _ ->
        fun name -> true

let getFilterSymbol () =
    let ft = getFilterText controls.filterTextType.SelectedIndex controls.filterText.Text
    let fs =
        match Int32.TryParse(controls.filterSize.Text) with
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

    fun (size, section, name) -> ft name && fs size && fg section

let rebindToViewToken = ref (new Threading.CancellationTokenSource())

let rebindToView syms =
    let filter = getFilterSymbol ()

    rebindToViewToken.Value.Cancel()
    rebindToViewToken := new Threading.CancellationTokenSource()

    let token = rebindToViewToken.Value.Token

    Async.Start(async {
        try
            let fs = Array.filter (fun s -> token.ThrowIfCancellationRequested(); filter s) syms
            do! Async.SwitchToContext context
            treeViewBinding.ItemsSource <- fs
        with e -> ()
        }, token)
    
let bindToView syms =
    treeViewBinding.ItemsSource <- syms

    controls.filterText.TextChanged.Add(fun _ -> rebindToView syms)
    controls.filterTextType.SelectionChanged.Add(fun _ -> rebindToView syms)
    controls.filterSize.TextChanged.Add(fun _ -> rebindToView syms)

    controls.filterSections.SelectionChanged.Add(fun e ->
        if controls.filterSections.SelectedIndex >= 0 then
            controls.filterSections.SelectedIndex <- -1)

    for section in syms |> Seq.map (fun (_, section, _) -> section) |> set do
        let sectionName = if section = "" then "<other>" else section
        let item = CheckBox(Content = section, IsChecked = Nullable<bool>(true), Tag = section)
        item.Unchecked.Add(fun _ -> rebindToView syms)
        item.Checked.Add(fun _ -> rebindToView syms)
        controls.filterSections.Items.Add(item) |> ignore

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
        let symbols = ess.Symbols |> Seq.map (fun s -> int s.size, s.section, s.name) |> Seq.toArray
        do! Async.SwitchToContext context
        bindToView symbols
        controls.tabTreeView.IsSelected <- true
    } |> Async.Start)

app.Run(window) |> ignore
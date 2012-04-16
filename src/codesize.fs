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

let w = Application.LoadComponent(Uri("src/ui/mainwindow.xaml", UriKind.Relative)) :?> Window

let treeView = w?TreeView :?> TreeView
let filterText = w?FilterText :?> TextBox
let filterTextType = w?FilterTextType :?> ComboBox 
let filterSize = w?FilterSize :?> TextBox
let filterSections = w?FilterSections :?> ComboBox

let app = Application()
let context = DispatcherSynchronizationContext(app.Dispatcher)

let treeViewBinding = TreeView.Binding(treeView)

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
    let ft = getFilterText filterTextType.SelectedIndex filterText.Text
    let fs =
        match Int32.TryParse(filterSize.Text) with
        | true, limit -> fun size -> size >= limit
        | _ -> fun size -> true
    let fg =
        let sections =
            filterSections.Items
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

    filterText.TextChanged.Add(fun _ -> rebindToView syms)
    filterTextType.SelectionChanged.Add(fun _ -> rebindToView syms)
    filterSize.TextChanged.Add(fun _ -> rebindToView syms)

    filterSections.SelectionChanged.Add(fun _ ->
        if filterSections.SelectedIndex >= 0 then
            filterSections.SelectedIndex <- -1)

    for section in syms |> Seq.map (fun (_, section, _) -> section) |> set do
        let sectionName = if section = "" then "<other>" else section
        let item = CheckBox(Content = section, IsChecked = Nullable<bool>(true), Tag = section)
        item.Unchecked.Add(fun _ -> rebindToView syms)
        item.Checked.Add(fun _ -> rebindToView syms)
        filterSections.Items.Add(item) |> ignore

app.Startup.Add(fun _ ->
    let path =
        if Environment.GetCommandLineArgs().Length > 1 then
            Environment.GetCommandLineArgs().[1]
        else
            let dlg = OpenFileDialog(DefaultExt = ".elf", Filter = "ELF files (.elf)|*.elf")
            let res = dlg.ShowDialog(w)
            if res.HasValue && res.Value then
                dlg.FileName
            else
                exit 0

    treeView.ItemsSource <- [sprintf "Loading %s..." path]

    async {
        let ess = ElfSymbolSource(path, fun key -> Map.find key config) :> ISymbolSource
        let symbols = ess.Symbols |> Seq.map (fun s -> int s.size, s.section, s.name) |> Seq.toArray
        do! Async.SwitchToContext context
        bindToView symbols
    } |> Async.Start)

app.Run(w) |> ignore
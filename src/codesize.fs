module codesize

open System
open System.Drawing
open System.Drawing.Imaging
open System.IO
open System.Windows
open System.Windows.Controls
open System.Windows.Documents
open System.Windows.Input
open System.Windows.Interop
open System.Windows.Media
open System.Windows.Media.Imaging
open Microsoft.Win32

open Symbols

let protectUI = UI.Exception.protect

let window = Application.LoadComponent(Uri("src/ui/mainwindow.xaml", UriKind.Relative)) :?> Window
let editor = lazy Editor.Window()

module gcontrols =
    let sessions = window?Sessions :?> TabControl
    let preloadFiles = window?PreloadFiles :?> CheckBox

let getSymbolSource path preload =
    match Path.GetExtension(path).ToLower() with
    | ".elf" ->
        ElfSymbolSource(path, preload) :> ISymbolSource
    | ".self" ->
        SelfSymbolSource(path, preload) :> ISymbolSource
    | ".pdb" ->
        PdbSymbolSource(path, preload) :> ISymbolSource
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
    let preload = gcontrols.preloadFiles.IsChecked.Value

    let tabcontent = Application.LoadComponent(Uri("src/ui/session.xaml", UriKind.Relative)) :?> UserControl
    let tab = TabItem(Header = path, HeaderTemplate = (window.Resources.["TabItemCloseHeaderTemplate"] :?> DataTemplate), Content = tabcontent)

    let controls: UI.Session.Controls =
        { content = tabcontent
          displayData = tabcontent?DisplayData :?> ComboBox 
          displayView = tabcontent?DisplayView :?> ComboBox
          filterText = tabcontent?FilterText :?> TextBox
          filterTextType = tabcontent?FilterTextType :?> ComboBox 
          filterSize = tabcontent?FilterSize :?> TextBox
          filterSections = tabcontent?FilterSections :?> ComboBox
          groupTemplates = tabcontent?GroupTemplates :?> ComboBox
          groupPrefix = tabcontent?GroupPrefix :?> ComboBox
          groupLineMerge = tabcontent?GroupLineMerge :?> TextBox
          pathRemapSource = tabcontent?PathRemapSource :?> TextBox
          pathRemapTarget = tabcontent?PathRemapTarget :?> TextBox
          symbolLocation = tabcontent?SymbolLocation :?> TextBox
          symbolLocationLink = tabcontent?SymbolLocationLink :?> Hyperlink
          symbolPanel = tabcontent?SymbolPanel :?> GroupBox
          contentsTree = tabcontent?ContentsTree :?> TreeView
          contentsList = tabcontent?ContentsList :?> ListView
          editor = editor.Force
          rebindToViewAgent = AsyncUI.SingleUpdateAgent()
          updateSymbolLocationAgent = AsyncUI.SingleUpdateAgent()
          jumpToAgent = AsyncUI.SingleUpdateAgent()
        }

    gcontrols.sessions.Items.Add(tab) |> ignore

    tab.Loaded.Add(fun _ -> tab.IsSelected <- true)

    tabcontent.IsEnabled <- false

    async {
        try
            let ess = getSymbolSource path preload
            do! UI.Session.bindToViewAsync controls ess

            updateRecentFileList path

            tabcontent.IsEnabled <- true
        with e ->
            do! AsyncUI.switchToUI ()
            UI.Exception.uploadException e
            UI.Exception.showModal e

            gcontrols.sessions.Items.Remove(tab)
    } |> Async.Start

let getOpenFileName () =
    let dlg = OpenFileDialog(Filter = "Supported files|*.elf;*.self;*.pdb", CheckFileExists = true)
    let res = dlg.ShowDialog(window)
    if res.HasValue && res.Value then
        Some dlg.FileName
    else
        None

let iconLoaderLock = obj()

type RecentFile(path) =
    member this.FileName = Path.GetFileName(path)
    member this.Path = path
    member this.Icon =
        use icon = lock iconLoaderLock (fun _ -> Icon.ExtractAssociatedIcon(path))

        let options = BitmapSizeOptions.FromEmptyOptions()
        let source = Imaging.CreateBitmapSourceFromHIcon(icon.Handle, Int32Rect.Empty, options)
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

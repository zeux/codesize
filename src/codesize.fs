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

let protectUI = UI.Exception.protect

let window = Application.LoadComponent(Uri("src/ui/mainwindow.xaml", UriKind.Relative)) :?> Window
let editor = lazy Editor.Window()

module gcontrols =
    let panelLoading = window?PanelLoading :?> Panel
    let labelLoading = window?LabelLoading :?> TextBlock
    let welcomePanel = window?WelcomePanel :?> Grid
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
    window.IsEnabled <- false
    window.Title <- sprintf "%s - %s" window.Title path
    gcontrols.panelLoading.Visibility <- Visibility.Visible
    gcontrols.labelLoading.Text <- sprintf "Loading %s..." path
    let preload = gcontrols.preloadFiles.IsChecked.Value

    let controls: UI.Session.Controls =
        { displayData = window?DisplayData :?> ComboBox 
          displayView = window?DisplayView :?> ComboBox
          filterText = window?FilterText :?> TextBox
          filterTextType = window?FilterTextType :?> ComboBox 
          filterSize = window?FilterSize :?> TextBox
          filterSections = window?FilterSections :?> ComboBox
          groupTemplates = window?GroupTemplates :?> ComboBox
          groupPrefix = window?GroupPrefix :?> ComboBox
          groupLineMerge = window?GroupLineMerge :?> TextBox
          pathRemapSource = window?PathRemapSource :?> TextBox
          pathRemapTarget = window?PathRemapTarget :?> TextBox
          labelStatus = window?LabelStatus :?> TextBlock
          symbolLocation = window?SymbolLocation :?> TextBox
          symbolLocationLink = window?SymbolLocationLink :?> Hyperlink
          symbolPanel = window?SymbolPanel :?> GroupBox
          contentsTree = window?ContentsTree :?> TreeView
          contentsList = window?ContentsList :?> ListView
          editor = editor
        }

    protectUI $ async {
        try
            let ess = getSymbolSource path preload
            do! UI.Session.bindToViewAsync controls ess

            gcontrols.welcomePanel.Visibility <- Visibility.Hidden
            updateRecentFileList path
        finally
            async {
                do! AsyncUI.switchToUI ()
                window.IsEnabled <- true
                gcontrols.panelLoading.Visibility <- Visibility.Hidden
            } |> Async.Start
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

        let options = Imaging.BitmapSizeOptions.FromEmptyOptions()
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

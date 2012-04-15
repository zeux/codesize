open System
open System.Windows
open System.Windows.Controls
open System.Windows.Threading
open Microsoft.Win32

open Symbols

[<STAThread>] do ()

let exepath = AppDomain.CurrentDomain.BaseDirectory
let config =
    ["tools/nm/@path", exepath + @"\nm.exe";
     "tools/demangle/@path", exepath + @"\demangle.exe"]
    |> Map.ofList

let t = TreeView()
let w = Window(Content = t)
let app = Application()
let context = DispatcherSynchronizationContext(app.Dispatcher)

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

    t.ItemsSource <- [sprintf "Loading %s..." path]

    async {
        let ess = ElfSymbolSource(path, fun key -> Map.find key config) :> ISymbolSource
        let symbols = ess.Symbols |> Seq.map (fun s -> int s.size, s.section, s.name) |> Seq.toArray
        do! Async.SwitchToContext context
        TreeView.bindToView t symbols
    } |> Async.Start)

app.Run(w) |> ignore
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Threading

open Symbols

[<STAThread>] do ()

let config =
    ["tools/nm/@path", @"D:\work\codesize\tools\nm.exe";
     "tools/c++filt/@path", @"D:\work\codesize\tools\c++filt.exe"]
    |> Map.ofList

let path = @"D:\work\codesizeelf\test.elf"

let t = TreeView(ItemsSource = [sprintf "Loading %s..." path])
let w = Window(Content = t)
let app = Application()
let context = DispatcherSynchronizationContext(app.Dispatcher)

async {
    let ess = ElfSymbolSource(path, fun key -> Map.find key config) :> ISymbolSource
    let symbols = ess.Symbols |> Seq.map (fun s -> int s.size, s.section, s.name) |> Seq.toArray
    do! Async.SwitchToContext context
    TreeView.bindToView t symbols
} |> Async.Start

app.Run(w) |> ignore
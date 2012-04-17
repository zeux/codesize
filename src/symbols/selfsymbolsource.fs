namespace Symbols

open System.IO

type SelfSymbolSource(path, configuration) =
    let offset =
        use inf = File.OpenRead(path)

        inf.Seek(16L, SeekOrigin.Begin) |> ignore
        Array.init 8 (fun _ -> inf.ReadByte() |> int64) |> Array.reduce (fun a b -> (a <<< 8) ||| b)

    let ess = ElfSymbolSource(path, configuration, offset = int offset) :> ISymbolSource

    interface ISymbolSource with
        member this.Symbols = ess.Symbols
        member this.FileLines = ess.FileLines
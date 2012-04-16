namespace Symbols

open System.Runtime.ConstrainedExecution
open System.IO

type SelfSymbolSource(path, configuration) =
    inherit CriticalFinalizerObject()

    let epath = Path.GetTempFileName()

    do
        use inf = File.OpenRead(path)
        use outf = File.OpenWrite(epath)

        inf.Seek(16L, SeekOrigin.Begin) |> ignore
        let offset = Array.init 8 (fun _ -> inf.ReadByte() |> int64) |> Array.reduce (fun a b -> (a <<< 8) ||| b)
        inf.Seek(offset, SeekOrigin.Begin) |> ignore

        inf.CopyTo(outf)

    let ess = ElfSymbolSource(epath, configuration) :> ISymbolSource

    interface ISymbolSource with
        member this.Symbols = ess.Symbols
        member this.FileLines = ess.FileLines

    override this.Finalize() =
        try File.Delete(epath)
        with e -> ()
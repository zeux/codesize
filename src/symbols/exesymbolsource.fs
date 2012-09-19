namespace Symbols

open Dia2Lib

type ExeSymbolSource(path, preload) =
    let pdbpath =
        let source = DiaSymbolSource.CreateSource ()
        do source.loadDataForExe(path, null, null)

        // we can't use the session as is since we have to optimize I/O and/or preload PDB data
        // so let's grab the PDB filename and use it in PdbSymbolSource
        let session = source.openSession()
        session.globalScope.symbolsFileName

    let ss = PdbSymbolSource(pdbpath, preload) :> ISymbolSource

    interface ISymbolSource with
        member this.Sections = ss.Sections
        member this.Symbols = ss.Symbols
        member this.FileLines = ss.FileLines
        member this.GetFileLine address = ss.GetFileLine address

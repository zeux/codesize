namespace Symbols

open Dia2Lib

type ExeSymbolSource(path) =
    let source = DiaSourceClass()
    do source.loadDataForExe(path, null, null)

    let ss = DiaSymbolSource(source) :> ISymbolSource

    interface ISymbolSource with
        member this.Sections = ss.Sections
        member this.Symbols = ss.Symbols
        member this.FileLines = ss.FileLines
        member this.GetFileLine address = ss.GetFileLine address

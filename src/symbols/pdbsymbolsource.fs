namespace Symbols

open Dia2Lib

type PdbSymbolSource(path) =
    let source = DiaSourceClass()
    do source.loadDataFromPdb(path)

    let ss = DiaSymbolSource(source) :> ISymbolSource

    interface ISymbolSource with
        member this.Sections = ss.Sections
        member this.Symbols = ss.Symbols
        member this.FileLines = ss.FileLines
        member this.GetFileLine address = ss.GetFileLine address
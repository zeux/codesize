namespace Symbols

open Dia2Lib

type PdbSymbolSource(path, preload) =
    let source = DiaSymbolSource.CreateSource ()

    do
        if preload then
            let stream = DiaMemoryStream(path)
            source.loadDataFromIStream(stream)
        else
            DiaMemoryStream.PrefetchFile(path)
            source.loadDataFromPdb(path)

    let ss = DiaSymbolSource(source) :> ISymbolSource

    interface ISymbolSource with
        member this.Sections = ss.Sections
        member this.Symbols = ss.Symbols
        member this.FileLines = ss.FileLines
        member this.GetFileLine address = ss.GetFileLine address
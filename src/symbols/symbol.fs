namespace Symbols

type Symbol =
    { address: uint64
      name: string
      size: uint64
      section: string }

type FileLine =
    { address: uint64
      size: uint64
      file: string
      line: int }

[<Interface>]
type ISymbolSource =
    abstract member Symbols: seq<Symbol>
    abstract member FileLines: seq<FileLine>
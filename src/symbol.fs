namespace Symbols

type Symbol =
    { address: uint64
      name: string
      size: uint64
      section: string }

[<Interface>]
type ISymbolSource =
    abstract member Symbols: seq<Symbol>
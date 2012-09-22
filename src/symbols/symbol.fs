namespace Symbols

type Symbol =
    { address: uint64
      size: uint64
      name: string
      section: string }

type FileLine =
    { address: uint64
      size: uint64
      file: string
      lineBegin: int
      lineEnd: int }

type FileLineRange =
    { size: uint64
      file: string
      lineBegin: int
      lineEnd: int }

[<Interface>]
type ISymbolSource =
    abstract member Sections: string[]
    abstract member Symbols: Symbol[] Lazy
    abstract member FileLines: FileLine[] Lazy

    abstract member GetFileLine: uint64 -> (string * int) option
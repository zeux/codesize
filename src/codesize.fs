open Symbols

let config =
    ["tools/nm/@path", @"D:\work\codesize\tools\nm.exe";
     "tools/c++filt/@path", @"D:\work\codesize\tools\c++filt.exe"]
    |> Map.ofList

let ess = ElfSymbolSource(@"D:\work\codesizeelf\test.elf", fun key -> Map.find key config) :> ISymbolSource

ess.Symbols |> Seq.toArray |> ignore
// for s in ess.Symbols do printfn "%d %s" s.size s.name
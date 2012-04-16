namespace Symbols

open System
open System.Collections.Generic
open System.Text.RegularExpressions

module private binutils =
    // parse nm output line
    // 
    // address section name
    // address size section name
    // address and size are 16-symbol hex strings
    // section is a character
    let parseNMLine (line: string) =
        let inthex offset = Convert.ToUInt64(line.Substring(offset, 16), 16)

        if line.Length > 0 && line.[0] = ' ' then
            None
        else if line.Length > 18 && line.[16] = ' ' && line.[18] = ' ' then
            Some (inthex 0, 0UL, line.Substring(16, 1), line.Substring(19))
        else if line.Length > 36 && line.[16] = ' ' && line.[33] = ' ' && line.[35] = ' ' then
            Some (inthex 0, inthex 17, line.Substring(34, 1), line.Substring(36))
        else
            None

    // convert section type to readable name
    let getType typ =
        match typ with
        | "t" | "T" -> "text"
        | "d" | "D" -> "data"
        | "r" | "R" -> "rodata"
        | "b" | "B" -> "bss"
        | _ -> failwithf "Unknown type %s" typ

    // compute symbol sizes
    let fixSizes syms =
        let symbols = syms |> Array.sortBy (fun (addr, _, _, _) -> addr)

        // some symbols don't have size information, so we compute the size as an address difference (this also takes alignment waste into account)
        // the exception is the last symbol, which we should just take the size from
        let result =
            symbols
            |> Seq.pairwise
            |> Seq.map (fun ((addr, size, typ, name), (next, _, _, _)) -> addr, next - addr, typ, name)
            |> Seq.toArray

        Array.append result (if symbols.Length > 0 then [| symbols.[symbols.Length - 1] |] else [||])

    // parse debug_line dump
    let parseDebugLines (lines: string seq) =
        let file = ref ""
        let re = Regex(@"^(.+?)\s+(\d+)\s+0x([a-f0-9]+)$")
        let intern = Dictionary<string, string>()

        seq {
            for l in lines do
                if l.EndsWith(":") then
                    let path = l.Substring(0, l.Length - 1).ToLower()

                    file :=
                        let mutable value = null
                        if intern.TryGetValue(path, &value) then
                            value
                        else
                            intern.Add(path, path)
                            path
                else
                    let m = re.Match(l)
                    if m.Success then
                        yield Convert.ToUInt64(m.Groups.[3].Value, 16), 1UL, !file, int m.Groups.[2].Value
        }

    // fix the address/size pairs to make sure no pair overlaps a symbol
    let fixSizesSymBounds syms data =
        let symtab = syms |> Seq.toArray |> Array.sortBy fst

        data
        |> Seq.map (fun (addr, size, file, line) ->
            let key = (addr, UInt64.MaxValue)
            let index = Array.BinarySearch(symtab, key)
            let (symaddr, symsize) =
                if index >= 0 then symtab.[index]
                else if index <> -1 then symtab.[~~~index - 1]
                else (addr, 0UL)
            addr, min size (symaddr + symsize - addr), file, line)

type ElfSymbolSource(path, configuration) =
    let symbols =
        lazy
        // get symbol data (with mangled names)
        let data =
            Process.popen (configuration "tools/nm/@path") (sprintf "--print-size %s" path)
            |> Seq.choose binutils.parseNMLine
            |> Seq.filter (fun (_, _, typ, name) -> "tTbBdDrR".IndexOf(typ) <> -1 && name.StartsWith(".LANCHOR") = false)
            |> Seq.toArray
            |> binutils.fixSizes

        // demangle names
        let names =
            data
            |> Seq.map (fun (_, _, typ, name) -> name)
            |> Seq.map (fun name ->
                if name.EndsWith("$rodata") then name.Substring(0, name.Length - 7)
                elif name.StartsWith(".") then name.Substring(1)
                else name)
            |> Process.popen2 (configuration "tools/demangle/@path") ""

        // get symbols
        Seq.map2 (fun (addr, size, typ, _) name ->
            { address = addr; name = name; size = size; section = binutils.getType typ }) data names

    let filelines =
        lazy
        // get file/line info
        Process.popen (configuration "tools/objdump/@path") (sprintf "--dwarf=decodedline %s" path)
        |> binutils.parseDebugLines
        |> Seq.toArray
        |> binutils.fixSizes
        |> binutils.fixSizesSymBounds (symbols.Value |> Seq.map (fun sym -> sym.address, sym.size))
        |> Seq.map (fun (addr, size, file, line) -> { address = addr; size = size; file = file; line = line })

    interface ISymbolSource with
        member this.Symbols = symbols.Value
        member this.FileLines = filelines.Value
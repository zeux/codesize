namespace Symbols

#nowarn "9" // StructLayout produces 'Uses of this construct may result in the generation of unverifiable .NET IL code' warning

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Text.RegularExpressions

module private binutils =
    // binutils.dll interface
    type BuFile = nativeint
    type BuSymtab = nativeint

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type BuSymbol =
        val address: uint64
        val size: uint64
        val typ: int
        val name: nativeint

    [<DllImport("binutils")>] extern BuFile buOpen(string path)
    [<DllImport("binutils")>] extern void buClose(BuFile file)
    [<DllImport("binutils")>] extern BuSymtab buSymtabOpen(BuFile file)
    [<DllImport("binutils")>] extern void buSymtabClose(BuSymtab symtab)
    [<DllImport("binutils")>] extern int buSymtabGetData(BuSymtab symtab, [<In; Out>] BuSymbol[] buffer, int bufferSize)

    // auto-closing scope helper
    type Scoped(value, deleter) =
        interface IDisposable with
            member this.Dispose() =
                deleter value

        member this.Value = value

    // convert section type to readable name
    let getSectionName typ =
        match typ with
        | 't' | 'T' -> "text"
        | 'd' | 'D' -> "data"
        | 'r' | 'R' -> "rodata"
        | 'b' | 'B' -> "bss"
        | _ -> failwithf "Unknown type %c" typ

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
        use file = new binutils.Scoped(binutils.buOpen(path), binutils.buClose)
        if file.Value = 0n then failwithf "Error opening file %s" path

        let data =
            use symtab = new binutils.Scoped(binutils.buSymtabOpen(file.Value), binutils.buSymtabClose)
            if symtab.Value = 0n then failwithf "Error reading symbols from file %s" path

            let count = binutils.buSymtabGetData(symtab.Value, null, 0)
            let data = Array.zeroCreate count

            binutils.buSymtabGetData(symtab.Value, data, count) |> ignore

            data
            |> Array.map (fun s -> s.address, s.size, (char s.typ), Marshal.PtrToStringAnsi(s.name))

        data
        |> Array.filter (fun (_, _, typ, name) -> "tTbBdDrR".IndexOf(typ) <> -1 && name.StartsWith("LANCHOR") = false)
        |> binutils.fixSizes
        |> Array.map (fun (address, size, typ, name) -> { address = address; size = size; section = binutils.getSectionName typ; name = name })
        |> Array.toSeq

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
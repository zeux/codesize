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
    type BuLinetab = nativeint

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type BuSymbol =
        val address: uint64
        val size: uint64
        val typ: int
        val name: nativeint

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type BuLine =
        val address: uint64
        val file: int
        val line: int

    [<DllImport("binutils")>] extern BuFile buOpen(string path, int offset)
    [<DllImport("binutils")>] extern void buClose(BuFile file)

    [<DllImport("binutils")>] extern BuSymtab buSymtabOpen(BuFile file)
    [<DllImport("binutils")>] extern void buSymtabClose(BuSymtab symtab)
    [<DllImport("binutils")>] extern int buSymtabGetData(BuSymtab symtab, [<Out>] BuSymbol[] buffer, int bufferSize)

    [<DllImport("binutils")>] extern BuLinetab buLinetabOpen(BuFile file)
    [<DllImport("binutils")>] extern void buLinetabClose(BuLinetab linetab)
    [<DllImport("binutils")>] extern int buLinetabGetFiles(BuLinetab linetab, [<Out>] nativeint[] buffer, int bufferSize)
    [<DllImport("binutils")>] extern int buLinetabGetLines(BuLinetab linetab, [<Out>] BuLine[] buffer, int bufferSize)

    // auto-closing scope helper
    type Scoped(value, deleter) =
        interface IDisposable with
            member this.Dispose() =
                deleter value

        member this.Value = value

    // get variable sized array
    let getArray pred ctx =
        let count = pred(ctx, null, 0)
        let data = Array.zeroCreate count

        let result = pred(ctx, data, count)
        assert (result = count)

        data

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

    // fix the address/size pairs to make sure no pair overlaps a symbol
    let fixSizesSymBounds syms data =
        let symtab = syms |> Array.sortBy fst
        let symaddrs = symtab |> Array.map fst

        data
        |> Array.map (fun (addr, size, file, line) ->
            let index = Array.BinarySearch(symaddrs, addr)
            let (symaddr, symsize) =
                if index >= 0 then symtab.[index]
                else if index <> -1 then symtab.[~~~index - 1]
                else (addr, 0UL)
            addr, min size (symaddr + symsize - addr), file, line)

type ElfSymbolSource(path, ?offset) =
    let symbols =
        lazy
        use file = new binutils.Scoped(binutils.buOpen(path, defaultArg offset 0), binutils.buClose)
        if file.Value = 0n then failwithf "Error opening file %s" path

        let data =
            use symtab = new binutils.Scoped(binutils.buSymtabOpen(file.Value), binutils.buSymtabClose)
            if symtab.Value = 0n then failwithf "Error reading symbols from file %s" path

            binutils.getArray binutils.buSymtabGetData symtab.Value
            |> Array.map (fun s -> s.address, s.size, (char s.typ), Marshal.PtrToStringAnsi(s.name))

        data
        |> Array.filter (fun (_, _, typ, name) -> "tTbBdDrR".IndexOf(typ) <> -1 && name.StartsWith("LANCHOR") = false)
        |> binutils.fixSizes
        |> Array.map (fun (address, size, typ, name) -> { address = address; size = size; section = binutils.getSectionName typ; name = name })

    let filelines =
        lazy
        use file = new binutils.Scoped(binutils.buOpen(path, defaultArg offset 0), binutils.buClose)
        if file.Value = 0n then failwithf "Error opening file %s" path

        let data =
            use linetab = new binutils.Scoped(binutils.buLinetabOpen(file.Value), binutils.buLinetabClose)
            if linetab.Value = 0n then failwithf "Error reading file/line data from file %s" path

            let files =
                binutils.getArray binutils.buLinetabGetFiles linetab.Value
                |> Array.map Marshal.PtrToStringAnsi

            binutils.getArray binutils.buLinetabGetLines linetab.Value
            |> Array.map (fun l -> l.address, 1UL, files.[l.file], l.line)

        // get file/line info
        data
        |> binutils.fixSizes
        |> binutils.fixSizesSymBounds (symbols.Value |> Array.map (fun sym -> sym.address, sym.size))
        |> Array.map (fun (addr, size, file, line) -> { address = addr; size = size; file = file; line = line })

    interface ISymbolSource with
        member this.Symbols = symbols.Value |> Array.toSeq
        member this.FileLines = filelines.Value |> Array.toSeq
        member this.GetFileLine address = None
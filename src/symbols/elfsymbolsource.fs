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

    [<DllImport("binutils")>] extern BuFile buFileOpen(string path, [<MarshalAs(UnmanagedType.U1)>] bool preload, int offset)

    [<DllImport("binutils")>] extern void buFileClose(BuFile file)

    [<DllImport("binutils")>] extern BuSymtab buSymtabOpen(BuFile file)
    [<DllImport("binutils")>] extern void buSymtabClose(BuSymtab symtab)
    [<DllImport("binutils")>] extern int buSymtabGetData(BuSymtab symtab, [<Out>] BuSymbol[] buffer, int bufferSize)

    [<DllImport("binutils")>] extern BuLinetab buLinetabOpen(BuFile file)
    [<DllImport("binutils")>] extern void buLinetabClose(BuLinetab linetab)
    [<DllImport("binutils")>] extern int buLinetabGetFiles(BuLinetab linetab, [<Out>] nativeint[] buffer, int bufferSize)
    [<DllImport("binutils")>] extern int buLinetabGetLines(BuLinetab linetab, [<Out>] BuLine[] buffer, int bufferSize)

    [<DllImport("binutils")>] extern [<MarshalAs(UnmanagedType.U1)>] bool buGetFileLine(BuFile file, uint64 address, string& filename, int& line)

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

    // valid section list
    let sectionList = "tTdDrRbB"

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
        // sort symbols by address, but keep empty symbols before non-empty
        // this makes sure that if an empty and non-empty symbols share an address, padded size is correctly attributed
        let symbols = syms |> Array.sortBy (fun (addr, size, _, _) -> addr * 2UL + (if size = 0UL then 0UL else 1UL))

        // some symbols don't have size information, so we compute the size as an address difference
        // (this also takes alignment waste into account)
        // the exception is the last symbol, which we should just take the size from
        symbols
        |> Array.mapi (fun i (addr, size, typ, name) ->
            if i + 1 < symbols.Length then
                let (next, _, _, _) = symbols.[i + 1]
                addr, next - addr, typ, name
            else
                addr, size, typ, name)

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

type ElfSymbolSource(path, preload, ?offset) =
    let file = binutils.buFileOpen(path, preload, defaultArg offset 0)
    do if file = 0n then failwithf "Error opening file %s" path

    let symbols =
        lazy
            use symtab = new binutils.Scoped(binutils.buSymtabOpen(file), binutils.buSymtabClose)
            if symtab.Value = 0n then failwithf "Error reading symbols from file %s" path

            binutils.getArray binutils.buSymtabGetData symtab.Value
            |> Array.filter (fun s -> binutils.sectionList.IndexOf(char s.typ) <> -1)
            |> Array.map (fun s ->
                { address = s.address
                  size = s.size
                  section = binutils.getSectionName (char s.typ)
                  name = Marshal.PtrToStringAnsi(s.name) })

    let lines =
        lazy
        let data =
            use linetab = new binutils.Scoped(binutils.buLinetabOpen(file), binutils.buLinetabClose)
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
        |> Array.map (fun (addr, size, file, line) -> { address = addr; size = size; file = file; lineBegin = line; lineEnd = line })

    let fileLineLock = obj()

    override this.Finalize() =
        binutils.buFileClose file

    interface ISymbolSource with
        member this.Sections = binutils.sectionList |> Seq.map binutils.getSectionName |> set |> Set.toArray
        member this.Symbols = symbols
        member this.FileLines = lines

        member this.GetFileLine address =
            // buGetFileLine is not thread-safe
            lock fileLineLock $ fun _ ->

            let mutable filename = null
            let mutable line = 0

            if binutils.buGetFileLine(file, address, &filename, &line) && filename <> null && line > 0 then
                Some (filename, line)
            else
                None
namespace Symbols

open Dia2Lib

module PDBDump =
    // helper to convert IDiaEnumSymbols to IDiaSymbol[]
    let syms (s: IDiaEnumSymbols) = seq { for o in s -> o :?> IDiaSymbol }

    // get section names from session
    let getSectionNames (session: IDiaSession) =
        // get debug stream data with header info
        let streams = session.getEnumDebugStreams()
        let headers = Array.init streams.count (fun i -> streams.[i]) |> Array.pick (fun s -> if s.name = "SECTIONHEADERS" then Some s else None)

        // get SCNHDR for each section, first 8 bytes correspond to section name
        Array.init headers.count (fun i ->
            // get data size
            let (size, _) = headers.[uint32 i, 0u]

            // get data
            let data = Array.zeroCreate (int size)
            headers.[uint32 i, size, ref size, &data.[0]]

            // get section name (first 8 bytes, null-terminated if length < 8)
            let bytes = data |> Seq.take 8 |> Seq.takeWhile ((<>) 0uy) |> Seq.map char |> Seq.toArray

            System.String(bytes))

    // get symbol information from file
    let getSymbols (session: IDiaSession) =
        // get section names
        let sections = getSectionNames session

        // get all symbols from PDB
        let all_symbols = session.findChildren(session.globalScope, SymTagEnum.SymTagNull, null, 0u) |> syms
        
        // get symbol info
        all_symbols
        |> Seq.filter (fun s -> s.locationType = 1u && s.length > 0UL) // LocIsStatic
        |> Seq.distinctBy (fun s -> s.relativeVirtualAddress)
        |> Seq.map (fun s ->
            let undname = s.get_undecoratedNameEx(0x1000u) // UNDNAME_NAME_ONLY
            let section = int s.addressSection
            let section_name = if section <= sections.Length then sections.[section - 1] else "" 
            s.length, s.addressOffset, section_name, if undname <> null then undname else s.name)
        |> Seq.toArray

type DiaSymbolSource(source: IDiaDataSource) =
    let session = source.openSession()

    let symbols =
        lazy
        PDBDump.getSymbols session
        |> Array.map (fun (size, address, section, name) ->
            { address = uint64 address; size = uint64 size; section = section; name = name })

    interface ISymbolSource with
        member this.Sections = PDBDump.getSectionNames session
        member this.Symbols = symbols.Value
        member this.FileLines = [||]
        member this.GetFileLine address = None
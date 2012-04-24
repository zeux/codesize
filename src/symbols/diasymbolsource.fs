namespace Symbols

open System
open System.Collections

open Dia2Lib

module private DIA =
    type LocationType =
    | LocIsStatic = 1

    [<Flags>]
    type UNDNAME =
    | NAME_ONLY = 0x1000

    // helpers to iterate IDiaEnum* objects
    let inline toSeq (o: ^T) =
        let e = { new IEnumerable with member this.GetEnumerator () = (^T: (member GetEnumerator: unit -> _) (o)) }
        if true then
            Seq.cast e
        else
            // Dummy expression to constrain return type to that of o.Item
            seq [| (^T: (member Item: _ -> _) (o, Unchecked.defaultof<_>)) |]

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

    // get symbol size
    let getSymbolSize (sym: IDiaSymbol) =
        match enum $ int sym.symTag with
        | SymTagEnum.SymTagFunction -> sym.length
        | SymTagEnum.SymTagData -> sym.``type``.length
        | t -> failwithf "Unknown symbol tag %O" t

    // get symbol information from file
    let getSymbols (session: IDiaSession) =
        // get section names
        let sections = getSectionNames session

        // get symbol info
        toSeq $ session.findChildren(session.globalScope, SymTagEnum.SymTagFunction, null, 0u)
        |> Seq.append (toSeq $ session.findChildren(session.globalScope, SymTagEnum.SymTagData, null, 0u))
        |> Seq.filter (fun s -> s.locationType = uint32 LocationType.LocIsStatic)
        |> Seq.toArray
        |> Array.map (fun s ->
            let size = getSymbolSize s
            let undname = s.get_undecoratedNameEx(uint32 UNDNAME.NAME_ONLY)
            let section = int s.addressSection
            let section_name = if section > 0 && section <= sections.Length then sections.[section - 1] else "" 

            { address = uint64 s.relativeVirtualAddress
              size = size
              section = section_name
              name = if undname <> null then undname else s.name })

    // get line information from file
    let getLines (session: IDiaSession) =
        let paths = Cache(fun id -> session.findFileById(id).fileName)

        toSeq $ session.findLinesByRVA(0u, ~~~0u)
        |> Seq.toArray
        |> Array.map (fun line ->
            let path = paths.[line.sourceFileId]

            { address = uint64 line.relativeVirtualAddress
              size = uint64 line.length
              file = path
              lineBegin = int line.lineNumber
              lineEnd = int line.lineNumberEnd} )


type DiaSymbolSource(source: IDiaDataSource) =
    let session = source.openSession()

    let symbols = lazy DIA.getSymbols session
    let lines = lazy DIA.getLines session

    interface ISymbolSource with
        member this.Sections = DIA.getSectionNames session
        member this.Symbols = symbols.Value
        member this.FileLines = lines.Value
        member this.GetFileLine address =
            let lines = session.findLinesByRVA(uint32 address, 0u)
            match lines.Next(1u) with
            | line, 1u -> Some (line.sourceFile.fileName, int line.lineNumber)
            | _ -> None

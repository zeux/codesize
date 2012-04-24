namespace Symbols

open System
open System.Collections

open Dia2Lib

module private DIA =
    type LocationType =
    | LocIsStatic = 1

    [<Flags>]
    type UNDNAME =
    | COMPLETE               = 0x0000
    | NO_LEADING_UNDERSCORES = 0x0001
    | NO_MS_KEYWORDS         = 0x0002
    | NO_FUNCTION_RETURNS    = 0x0004
    | NO_ALLOCATION_MODEL    = 0x0008
    | NO_ALLOCATION_LANGUAGE = 0x0010
    | NO_MS_THISTYPE         = 0x0020
    | NO_CV_THISTYPE         = 0x0040
    | NO_THISTYPE            = 0x0060
    | NO_ACCESS_SPECIFIERS   = 0x0080
    | NO_THROW_SIGNATURES    = 0x0100
    | NO_MEMBER_TYPE         = 0x0200
    | NO_RETURN_UDT_MODEL    = 0x0400
    | _32_BIT_DECODE         = 0x0800
    | NAME_ONLY              = 0x1000
    | NO_ARGUMENTS           = 0x2000
    | NO_SPECIAL_SYMS        = 0x4000

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

    // get symbol name
    let getSymbolName (sym: IDiaSymbol) =
        let undflags =
            UNDNAME.NO_MS_KEYWORDS |||
            UNDNAME.NO_FUNCTION_RETURNS |||
            UNDNAME.NO_ALLOCATION_MODEL |||
            UNDNAME.NO_ALLOCATION_LANGUAGE |||
            UNDNAME.NO_ACCESS_SPECIFIERS |||
            UNDNAME.NO_MEMBER_TYPE

        let undname = sym.get_undecoratedNameEx(uint32 undflags)
        let name = if undname = null then sym.name else undname
        name.Replace("`", "")

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
            let section = int s.addressSection
            let section_name = if section > 0 && section <= sections.Length then sections.[section - 1] else "" 

            { address = uint64 s.relativeVirtualAddress
              size = getSymbolSize s
              section = section_name
              name = getSymbolName s })

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

namespace Symbols

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.InteropServices

open Dia2Lib

module private DIACreate =
    [<Literal>]
    let GUID = UnmanagedType.LPStruct

    [<DllImport("msdia110")>]
    extern int DllGetClassObject( [<MarshalAs(GUID)>] Guid clsid, [<MarshalAs(GUID)>] Guid iid, [<Out>] nativeint& ppv)

    [<ComVisible(false)>]
    [<ComImport; InterfaceType(ComInterfaceType.InterfaceIsIUnknown); Guid("00000001-0000-0000-C000-000000000046")>]
    [<Interface>]
    type IClassFactory =
        abstract member CreateInstance: outer:nativeint * [<MarshalAs(GUID)>] iid:Guid * punk:byref<nativeint> -> unit
        abstract member LockServer: bool -> unit

    let create<'I> (clsid: Guid) =
        // get class factory
        let mutable punk = 0n
        Marshal.ThrowExceptionForHR(DllGetClassObject(clsid, Marshal.GenerateGuidForType(typeof<IClassFactory>), &punk))
        let factory = Marshal.GetTypedObjectForIUnknown(punk, typeof<IClassFactory>) :?> IClassFactory

        // create class instance
        let mutable punki = 0n
        factory.CreateInstance(0n, Marshal.GenerateGuidForType(typeof<'I>), &punki)
        Marshal.GetTypedObjectForIUnknown(punki, typeof<'I>) :?> 'I

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
        let headers = Array.init streams.count (fun i -> streams.[i]) |> Array.tryPick (fun s -> if s.name = "SECTIONHEADERS" then Some s else None)

        match headers with
        | Some stream ->
            // get SCNHDR for each section, first 8 bytes correspond to section name
            Array.init stream.count (fun i ->
                // get data size
                let (size, _) = stream.[uint32 i, 0u]

                // get data
                let data = Array.zeroCreate (int size)
                stream.[uint32 i, size, ref size, &data.[0]]

                // get section name (first 8 bytes, null-terminated if length < 8)
                let bytes = data |> Seq.take 8 |> Seq.takeWhile ((<>) 0uy) |> Seq.map char |> Seq.toArray

                System.String(bytes))
        | None -> [||]

    // get symbol name
    let getSymbolName (sym: IDiaSymbol) =
        let undflags =
            if sym.``function`` = 0 then
                UNDNAME.NAME_ONLY
            else
                UNDNAME.NO_MS_KEYWORDS |||
                UNDNAME.NO_FUNCTION_RETURNS |||
                UNDNAME.NO_ALLOCATION_MODEL |||
                UNDNAME.NO_ALLOCATION_LANGUAGE |||
                UNDNAME.NO_ACCESS_SPECIFIERS |||
                UNDNAME.NO_MEMBER_TYPE

        let undname = sym.get_undecoratedNameEx(uint32 undflags)
        let name = if undname = null then sym.name else undname

        if name = "`string'" then
            "string'" + sym.name
        elif name.[0] = '`' then
            name.[1..]
        else
            name

    let excludeSymbolsByVA (lhs : IDiaSymbol seq) (rhs: IDiaSymbol seq) =
        let va = HashSet<_>(rhs |> Seq.map (fun s -> s.virtualAddress))

        lhs |> Seq.filter (fun s -> not (va.Contains(s.virtualAddress)))

    // get symbol size
    let getSymbolSize (sym: IDiaSymbol) =
        match enum $ int sym.symTag with
        | SymTagEnum.SymTagFunction -> sym.length
        | SymTagEnum.SymTagData -> sym.``type``.length
        | SymTagEnum.SymTagPublicSymbol -> sym.length
        | t -> failwithf "Unknown symbol tag %O" t

    // get symbol information from file
    let getSymbols (session: IDiaSession) =
        // get section names
        let sections = getSectionNames session

        // get function/data symbols
        let fdsyms =
            toSeq $ session.findChildren(session.globalScope, SymTagEnum.SymTagFunction, null, 0u)
            |> Seq.append (toSeq $ session.findChildren(session.globalScope, SymTagEnum.SymTagData, null, 0u))
            |> Seq.distinctBy (fun s -> s.virtualAddress)
            |> Seq.toArray

        // get public symbols that are not listed in function/data symbols
        let psyms =
            toSeq $ session.findChildren(session.globalScope, SymTagEnum.SymTagPublicSymbol, null, 0u)
            |> Seq.distinctBy (fun s -> s.virtualAddress)

        let pfsyms = excludeSymbolsByVA psyms fdsyms |> Seq.toArray
        
        // do we have to filter by static here?
        let syms =
            Array.append pfsyms fdsyms
            |> Array.filter (fun s -> s.locationType = uint32 LocationType.LocIsStatic)

        // get symbol info
        syms
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

type DiaException(message, innerException: exn) =
    inherit Exception(message, innerException)

    override this.StackTrace =
        innerException.StackTrace

    static member TranslateIn code =
        try
            code ()
        with
        :? COMException as e ->
            raise $ DiaException.Translate e

    static member Translate (e: COMException) =
        let text = 
            match e.ErrorCode with
            | 0x806d0001 -> "No error"
            | 0x806d0002 -> "Invalid usage"
            | 0x806d0003 -> "Out of memory"
            | 0x806d0004 -> "File system error"
            | 0x806d0005 -> "File not found or has invalid format"
            | 0x806d0006 -> "Signature does not match"
            | 0x806d0007 -> "Age does not match"
            | 0x806d0008 -> "E_PDB_PRECOMP_REQUIRED"
            | 0x806d0009 -> "E_PDB_OUT_OF_TI"
            | 0x806d000a -> "Not implemented"
            | 0x806d000b -> "E_PDB_V1_PDB"
            | 0x806d000c -> "Obsolete format"
            | 0x806d000d -> "E_PDB_LIMIT"
            | 0x806d000e -> "File is corrupt"
            | 0x806d000f -> "E_PDB_TI16"
            | 0x806d0010 -> "Access denied"
            | 0x806d0011 -> "E_PDB_ILLEGAL_TYPE_EDIT"
            | 0x806d0012 -> "Invalid executable"
            | 0x806d0013 -> "Debug file not found"
            | 0x806d0014 -> "No debug info found"
            | 0x806d0015 -> "Executable timestamp does not match"
            | 0x806d0016 -> "E_PDB_RESERVED"
            | 0x806d0017 -> "Debug info is not in PDB"
            | 0x806d0018 -> "Symbol server cache path is invalid"
            | 0x806d0019 -> "Symbol server cache is full"
            | _ -> ""

        let message =
            if text = "" then e.Message
            else sprintf "%s (%08X)" text e.ErrorCode

        DiaException(message, e)

type DiaSymbolSource(source: IDiaDataSource) =
    let session = source.openSession()

    let symbols = lazy DIA.getSymbols session
    let lines = lazy DIA.getLines session

    interface ISymbolSource with
        member this.Sections = DIA.getSectionNames session
        member this.Symbols = symbols
        member this.FileLines = lines
        member this.GetFileLine address =
            let lines = session.findLinesByRVA(uint32 address, 0u)
            match lines.Next(1u) with
            | line, 1u -> Some (line.sourceFile.fileName, int line.lineNumber)
            | _ -> None

    static member CreateSource() =
        DIACreate.create<IDiaDataSource> $ Guid("{761D3BCD-1304-41D5-94E8-EAC54E4AC172}")

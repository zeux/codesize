namespace Symbols

// We use native pointers for COM interop, disable "unverifiable IL" warnings
#nowarn "9"
#nowarn "51"

open System
open System.IO
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

open Dia2Lib

type DiaMemoryStream(path) =
    let data = File.ReadAllBytes(path)
    let mutable position = 0L

    member this.Unimplemented() = raise $ NotImplementedException()

    interface ISequentialStream with
        member this.RemoteRead(pv, cb, pcbRead) = this.Unimplemented()
        member this.RemoteWrite(pv, cb, pcbWritten) = this.Unimplemented()

    interface IStream with
        member this.RemoteWrite(pv, cb, pcbWritten) = this.Unimplemented()
        member this.SetSize(libNewSize) = this.Unimplemented()
        member this.RemoteCopyTo(pstm, cb, pcbRead, pcbWritten) = this.Unimplemented()
        member this.Commit(grfCommitFlags) = this.Unimplemented()
        member this.Revert() = this.Unimplemented()
        member this.LockRegion(libOffset, cb, dwLockType) = this.Unimplemented()
        member this.UnlockRegion(libOffset, cb, dwLockType) = this.Unimplemented()
        member this.Clone(ppstm) = this.Unimplemented()

        member this.RemoteRead(pv, cb, pcbRead) =
            let count = min cb $ uint32 (data.LongLength - position)
            Marshal.Copy(data, int position, NativePtr.toNativeInt &&pv, int count)
            pcbRead <- count
            
        member this.RemoteSeek(dlibMove, dwOrigin, plibNewPosition) =
            match enum $ int dwOrigin with
            | SeekOrigin.Begin ->
                position <- dlibMove.QuadPart
            | SeekOrigin.Current ->
                position <- position + dlibMove.QuadPart
            | SeekOrigin.End ->
                position <- data.LongLength - dlibMove.QuadPart
            | _ ->
                raise $ ArgumentException()

            position <- max 0L (min position data.LongLength)
            plibNewPosition <- _ULARGE_INTEGER(QuadPart = uint64 position)

        member this.Stat(pstatstg, grfStatFlag) =
            pstatstg <- tagSTATSTG(cbSize = _ULARGE_INTEGER(QuadPart = uint64 data.LongLength))

    static member PrefetchFile path =
        try
            use file = IO.File.OpenRead(path)
            let buffer = Array.zeroCreate 65536
            while file.Read(buffer, 0, buffer.Length) > 0 do ()
        with _ -> ()
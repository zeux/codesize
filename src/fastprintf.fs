[<AutoOpen>]
module FastPrintf

// Configuration defines:
// FASTPRINTF_COMPAT_FS2 - enables F# 2.0 compatibility (no padding for %c)
// FASTPRINTF_COMPAT_FX3 - enables .NET 3.x compatibility (no ConcurrentDictionary)

open System
open System.Globalization
open System.Reflection
open System.Text
open System.IO
open Microsoft.FSharp.Core.Printf

#if FASTPRINTF_COMPAT_FX3
open System.Collections.Generic
#else
open System.Collections.Concurrent
#endif

module private PrintfImpl =
    [<Flags>]
    type FormatFlags =
    | None = 0
    | ZeroFill = 1
    | LeftJustify = 2
    | AddSignIfPositive = 4
    | AddSpaceIfPositive = 8

    type FormatElement =
        { flags: FormatFlags
          width: int
          precision: int
          typ: char
          postfix: string }

    let inline hasFlag e f = (e &&& f) = f

    let parseFormatFlag ch =
        match ch with
        | '0' -> FormatFlags.ZeroFill
        | '-' -> FormatFlags.LeftJustify
        | '+' -> FormatFlags.AddSignIfPositive
        | ' ' -> FormatFlags.AddSpaceIfPositive
        | _ -> FormatFlags.None

    let rec parseFormatFlags (fmt: string) i acc =
        match parseFormatFlag fmt.[i] with
        | FormatFlags.None -> i, acc
        | x -> parseFormatFlags fmt (i + 1) (acc ||| x)

    let rec parseNumber (fmt: string) i acc =
        match fmt.[i] with
        | ch when ch >= '0' && ch <= '9' -> parseNumber fmt (i + 1) (acc * 10 + int ch - int '0')
        | _ -> i, acc

    let rec parseString (fmt: string) i =
        if i >= fmt.Length || fmt.[i] = '%' then i
        else parseString fmt (i + 1)

    let parsePrecision (fmt: string) i def =
        if fmt.[i] = '.' then parseNumber fmt (i + 1) 0
        else i, def

    let parseElement (fmt: string) i =
        let i, flags = parseFormatFlags fmt i FormatFlags.None
        let i, width = parseNumber fmt i 0
        let i, precision = parsePrecision fmt i -1
        i + 1, { new FormatElement with flags = flags and width = width and precision = precision and typ = fmt.[i] and postfix = "" }

    let parseFormatString (fmt: string) =
        let rec loop i acc =
            if i >= fmt.Length then acc
            else
                assert (fmt.[i] = '%')
                let i, el = parseElement fmt (i + 1)
                let e = parseString fmt i
                loop e ({el with postfix = fmt.Substring(i, e - i)} :: acc)

        let prefix = parseString fmt 0
        let elements = loop prefix []
        let prefixs = fmt.Substring(0, prefix)

        let rec mergeRev (els: FormatElement list) acc =
            match els with
            | x :: y :: xs when x.typ = '%' -> mergeRev ({ y with postfix = y.postfix + "%" + x.postfix } :: xs) acc
            | x :: xs -> mergeRev xs (x :: acc)
            | [] -> acc

        match mergeRev elements [] with
        | x :: xs when x.typ = '%' -> prefixs + "%" + x.postfix, xs
        | xs -> prefixs, xs

    let addPadding (e: FormatElement) (conv: 'a -> string) =
        if e.width = 0 then
            conv
        else
            let ch = if hasFlag e.flags FormatFlags.ZeroFill then '0' else ' '
            if hasFlag e.flags FormatFlags.LeftJustify then
                fun x -> (conv x).PadRight(e.width, ch)
            else
                fun x -> (conv x).PadLeft(e.width, ch)

    let formatChar c = 
        match c with 
        | '\'' -> "\\\'"
        | '\\' -> "\\\\"
        | '\b' -> "\\b"
        | _ when System.Char.IsControl(c) -> 
             let d1 = (int c / 100) % 10 
             let d2 = (int c / 10) % 10 
             let d3 = int c % 10 
             "\\" + d1.ToString() + d2.ToString() + d3.ToString()
        | _ -> c.ToString()
                
    let genericPrintOpt (o: obj) =
        match o with 
        | null -> "<null>"
        | :? double as d -> 
            let s = d.ToString("g10", CultureInfo.InvariantCulture)
            if System.Double.IsNaN(d) then "nan"
            elif System.Double.IsNegativeInfinity(d) then "-infinity"
            elif System.Double.IsPositiveInfinity(d) then "infinity"
            elif String.forall(fun c -> System.Char.IsDigit(c) || c = '-')  s
            then s + ".0" 
            else s
        | :? single as d -> 
            (if System.Single.IsNaN(d) then "nan"
             elif System.Single.IsNegativeInfinity(d) then "-infinity"
             elif System.Single.IsPositiveInfinity(d) then "infinity"
             elif float32(System.Int32.MinValue) < d && d < float32(System.Int32.MaxValue) && float32(int32(d)) = d 
             then (System.Convert.ToInt32 d).ToString(CultureInfo.InvariantCulture) + ".0"
             else d.ToString("g10", CultureInfo.InvariantCulture)) 
            + "f"
        | :? System.Decimal as d -> d.ToString("g", CultureInfo.InvariantCulture) + "M"
        | :? uint64 as d -> d.ToString(CultureInfo.InvariantCulture) + "UL"
        | :? int64  as d -> d.ToString(CultureInfo.InvariantCulture) + "L"
        | :? int32  as d -> d.ToString(CultureInfo.InvariantCulture)
        | :? uint32 as d -> d.ToString(CultureInfo.InvariantCulture) + "u"
        | :? int16  as d -> d.ToString(CultureInfo.InvariantCulture) + "s"
        | :? uint16 as d -> d.ToString(CultureInfo.InvariantCulture) + "us"
        | :? sbyte  as d -> d.ToString(CultureInfo.InvariantCulture) + "y"
        | :? byte   as d -> d.ToString(CultureInfo.InvariantCulture) + "uy"
        | :? nativeint as d -> d.ToString() + "n"
        | :? unativeint  as d -> d.ToString() + "un"
        | :? bool   as b -> (if b then "true" else "false")
        | :? char   as c -> "\'" + formatChar c + "\'"
        | :? string as s -> "\"" + s + "\""
        | :? Enum as e -> e.ToString()
        | _ -> null

    [<AllowNullLiteral>]
    type FormatContext<'Result> =
        abstract Apply: (obj -> obj) * FormatElement -> unit
        abstract Append: string * FormatElement -> unit
        abstract Finish: unit -> 'Result

    type FormatTransformer<'Result> = FormatContext<'Result> -> FormatContext<'Result>

    type FormatFactoryString<'Result> =
        static member Next<'T, 'Cont> (e: FormatElement) (func: 'T -> string) (next: FormatTransformer<'Result> -> 'Cont) =
            fun (state: FormatTransformer<'Result>) ->
                fun (v: 'T) ->
                    let state' acc =
                        let r = state acc
                        r.Append(func v, e)
                        r
                    next state'

        static member Opt1<'A0> e0 f0 =
            fun (state: FormatTransformer<'Result>) ->
                fun (a0: 'A0) ->
                    let s = state null
                    s.Append(f0 a0, e0)
                    s.Finish()

        static member Opt2<'A0, 'A1> e0 e1 f0 f1 =
            fun (state: FormatTransformer<'Result>) ->
                fun (a0: 'A0) (a1: 'A1) ->
                    let s = state null
                    s.Append(f0 a0, e0)
                    s.Append(f1 a1, e1)
                    s.Finish()

        static member Opt3<'A0, 'A1, 'A2> e0 e1 e2 f0 f1 f2 =
            fun (state: FormatTransformer<'Result>) ->
                fun (a0: 'A0) (a1: 'A1) (a2: 'A2) ->
                    let s = state null
                    s.Append(f0 a0, e0)
                    s.Append(f1 a1, e1)
                    s.Append(f2 a2, e2)
                    s.Finish()

        static member Opt4<'A0, 'A1, 'A2, 'A3> e0 e1 e2 e3 f0 f1 f2 f3 =
            fun (state: FormatTransformer<'Result>) ->
                fun (a0: 'A0) (a1: 'A1) (a2: 'A2) (a3: 'A3) ->
                    let s = state null
                    s.Append(f0 a0, e0)
                    s.Append(f1 a1, e1)
                    s.Append(f2 a2, e2)
                    s.Append(f3 a3, e3)
                    s.Finish()

        static member Opt5<'A0, 'A1, 'A2, 'A3, 'A4> e0 e1 e2 e3 e4 f0 f1 f2 f3 f4 =
            fun (state: FormatTransformer<'Result>) ->
                fun (a0: 'A0) (a1: 'A1) (a2: 'A2) (a3: 'A3) (a4: 'A4) ->
                    let s = state null
                    s.Append(f0 a0, e0)
                    s.Append(f1 a1, e1)
                    s.Append(f2 a2, e2)
                    s.Append(f3 a3, e3)
                    s.Append(f4 a4, e4)
                    s.Finish()

    type FormatFactoryGeneric<'State, 'Residue, 'Result> =
        static member CreateT<'Cont> (e: FormatElement) (next: FormatTransformer<'Result> -> 'Cont) =
            fun (state: FormatTransformer<'Result>) ->
                fun (f: 'State -> 'Residue) ->
                    let state' acc =
                        let r = state acc
                        r.Apply((fun s -> f (unbox s) |> box), e)
                        r
                    next state'

        static member CreateA<'T, 'Cont> (e: FormatElement) (next: FormatTransformer<'Result> -> 'Cont) =
            fun (state: FormatTransformer<'Result>) ->
                fun (f: 'State -> 'T -> 'Residue) (v: 'T) ->
                    let state' acc =
                        let r = state acc
                        r.Apply((fun s -> f (unbox s) v |> box), e)
                        r
                    next state'

    type Factory =
        static member CreateBoxString<'T> (e: FormatElement) =
            let basic = fun (o: 'T) -> if Object.ReferenceEquals(o, null) then "<null>" else o.ToString()
            basic |> addPadding e

        static member CreateGenericString<'T> (e: FormatElement) =
            let fmt =
                String.Concat("%",
                    (if hasFlag e.flags FormatFlags.AddSignIfPositive then "+" else ""),
                    (if hasFlag e.flags FormatFlags.ZeroFill then "0" else if e.width > 0 then string e.width else ""),
                    (if e.precision >= 0 then "." + string e.precision else ""),
                    "A")
            if fmt = "%A" then
                fun (o: 'T) ->
                    let s = genericPrintOpt (box o)
                    if s <> null then s
                    else sprintf (StringFormat<'T -> string>(fmt)) o
            else
                fun (o: 'T) -> sprintf (StringFormat<'T -> string>(fmt)) o

    let bindingFlags = BindingFlags.Static ||| BindingFlags.NonPublic

    let getStringFormatterNext<'Result> e func next arg cont =
        typeof<FormatFactoryString<'Result>>.GetMethod("Next", bindingFlags).MakeGenericMethod([|arg; cont|]).Invoke(null, [|box e; box func; box next|])

    let getStringFormatterOptN<'Result> n types args =
        assert (n >= 1 && n <= 5)
        typeof<FormatFactoryString<'Result>>.GetMethod("Opt" + n.ToString(), bindingFlags).MakeGenericMethod(types).Invoke(null, args)

    let getGenericFormatterT<'State, 'Residue, 'Result> e next cont =
        typeof<FormatFactoryGeneric<'State, 'Residue, 'Result>>.GetMethod("CreateT", bindingFlags).MakeGenericMethod([|cont|]).Invoke(null, [|box e; box next|])

    let getGenericFormatterA<'State, 'Residue, 'Result> e next arg cont =
        typeof<FormatFactoryGeneric<'State, 'Residue, 'Result>>.GetMethod("CreateA", bindingFlags).MakeGenericMethod([|arg; cont|]).Invoke(null, [|box e; box next|])

    let getBoxStringFunction (e: FormatElement) (typ: Type) =
        typeof<Factory>.GetMethod("CreateBoxString", bindingFlags).MakeGenericMethod([|typ|]).Invoke(null, [|box e|])

    let getGenericStringFunction (e: FormatElement) (typ: Type) =
        typeof<Factory>.GetMethod("CreateGenericString", bindingFlags).MakeGenericMethod([|typ|]).Invoke(null, [|box e|])

    let inline toStringInvariant (x: 'T) =
        (^T: (member ToString: IFormatProvider -> string) (x, CultureInfo.InvariantCulture))

    let inline toStringFormatInvariant (x: 'T) format =
        (^T: (member ToString: string -> IFormatProvider -> string) (x, format, CultureInfo.InvariantCulture))

    let inline toStringIntegerBasic (e: FormatElement) unsigned : 'T -> string =
        match e.typ with
        | 'd' | 'i' -> fun (x: 'T) -> toStringInvariant x
        | 'u' -> fun (x: 'T) -> toStringInvariant (x |> unsigned)
        | 'x' -> fun (x: 'T) -> toStringFormatInvariant x "x"
        | 'X' -> fun (x: 'T) -> toStringFormatInvariant x "X"
        | 'o' -> fun (x: 'T) -> Convert.ToString(x |> unsigned |> int64, 8)
        | _ -> failwithf "Unrecognized integer type specifier '%c'" e.typ

    let inline toStringInteger (e: FormatElement) unsigned : 'T -> string =
        let basic = toStringIntegerBasic e unsigned

        if e.width = 0 && e.flags = FormatFlags.None then
            basic
        else fun (x: 'T) ->
            let mutable s = basic x
            let mutable sign = false

            if s.[0] = '-' then
                sign <- true
            else if hasFlag e.flags FormatFlags.AddSignIfPositive || hasFlag e.flags FormatFlags.AddSpaceIfPositive then
                s <- (if hasFlag e.flags FormatFlags.AddSignIfPositive then "+" else " ") + s
                sign <- true

            if e.width = 0 then
                s
            else if hasFlag e.flags FormatFlags.LeftJustify then
                s.PadRight(e.width, ' ')
            else if hasFlag e.flags FormatFlags.ZeroFill then
                if sign then
                    if e.width <= s.Length then s
                    else
                        s.Insert(1, String('0', e.width - s.Length))
                else s.PadLeft(e.width, '0')
            else
                s.PadLeft(e.width, ' ')

    let getFloatFormat (e: FormatElement) =
        e.typ.ToString() + (if e.precision < 0 then "6" else (max (min e.precision 99) 0).ToString())

    let getFloatSign (e: FormatElement) =
        if hasFlag e.flags FormatFlags.AddSpaceIfPositive then " " else "+"

    let inline toStringFloatBasic fmt (e: FormatElement) : 'T -> string =
        if hasFlag e.flags FormatFlags.AddSignIfPositive || hasFlag e.flags FormatFlags.AddSpaceIfPositive then
            fun (x: 'T) ->
                let s = toStringFormatInvariant x fmt
                if s.[0] <> '-' && s.[0] <> 'N' then
                    getFloatSign e + s
                else
                    s
        else
            fun (x: 'T) -> toStringFormatInvariant x fmt

    let inline floatIsFinite (x: 'T) =
        not (^T: (static member IsPositiveInfinity: 'T -> bool) x) &&
        not (^T: (static member IsNegativeInfinity: 'T -> bool) x) &&
        not (^T: (static member IsNaN: 'T -> bool) x)

    let inline toStringFloat (e: FormatElement) : 'T -> string =
        let basic = toStringFloatBasic (getFloatFormat e) e

        if e.width = 0 && e.flags = FormatFlags.None then
            basic
        else fun (x: 'T) ->
            let mutable s = basic x

            let ch = if hasFlag e.flags FormatFlags.ZeroFill && floatIsFinite x then '0' else ' '
            if hasFlag e.flags FormatFlags.LeftJustify then s.PadRight(e.width, ch)
            else s.PadLeft(e.width, ch)

    let toString (e: FormatElement) (typ: Type) =
        match e.typ with
        | 'b' -> (fun x -> if x then "true" else "false") |> addPadding e |> box
        | 'c' ->
            (fun (x: char) -> x.ToString())
        #if FASTPRINTF_COMPAT_FS2
        #else
            |> addPadding e
        #endif
            |> box
        | 'd' | 'i' | 'u' | 'x' | 'X' | 'o' ->
            if typ = typeof<int8> then toStringInteger e uint8 |> box<int8 -> string>
            else if typ = typeof<uint8> then toStringInteger e uint8 |> box<uint8 -> string>
            else if typ = typeof<int16> then toStringInteger e uint16 |> box<int16 -> string>
            else if typ = typeof<uint16> then toStringInteger e uint16 |> box<uint16 -> string>
            else if typ = typeof<int32> then toStringInteger e uint32 |> box<int32 -> string>
            else if typ = typeof<uint32> then toStringInteger e uint32 |> box<uint32 -> string>
            else if typ = typeof<int64> then toStringInteger e uint64 |> box<int64 -> string>
            else if typ = typeof<uint64> then toStringInteger e uint64 |> box<uint64 -> string>
            else if typ = typeof<nativeint> then
                match sizeof<nativeint> with
                | 4 -> (fun x -> int32 x |> toStringInteger e uint32) |> box<nativeint -> string>
                | 8 -> (fun x -> int64 x |> toStringInteger e uint64) |> box<nativeint -> string>
                | x -> failwith "Unexpected size for nativeint: %d" x
            else if typ = typeof<unativeint> then
                match sizeof<unativeint> with
                | 4 -> (fun x -> uint32 x |> toStringInteger e uint32) |> box<unativeint -> string>
                | 8 -> (fun x -> uint64 x |> toStringInteger e uint64) |> box<unativeint -> string>
                | x -> failwith "Unexpected size for unativeint: %d" x
            else failwithf "Unrecognized type %A" typ
        | 'e' | 'E' | 'f' | 'F' | 'g' | 'G' ->
            if typ = typeof<float32> then toStringFloat e |> box<float32 -> string>
            else if typ = typeof<float> then toStringFloat e |> box<float -> string>
            else if typ = typeof<decimal> then toStringFloatBasic (getFloatFormat e) e |> addPadding e |> box<decimal -> string>
            else failwithf "Unrecognized type %A" typ
        | 'M' ->
            if typ = typeof<decimal> then toStringFloatBasic "G" e |> addPadding e |> box<decimal -> string>
            else failwithf "Unrecognized type %A" typ
        | 's' ->
            if typ = typeof<string> then (fun (x: string) -> if x = null then "" else x) |> addPadding e |> box
            else failwithf "Unrecognized type %A" typ
        | 'O' -> getBoxStringFunction e typ
        | 'A' -> getGenericStringFunction e typ
        | _ -> failwithf "Unrecognized format type %c" e.typ

    let getFunctionElements (typ: Type) =
        match typ.GetGenericArguments() with
        | [|car; cdr|] -> car, cdr
        | _ -> failwithf "Type %A is not a function type" typ

    let rec unpackFunctionArguments (typ: Type) =
        match typ.GetGenericArguments() with
        | [|car; cdr|] -> car :: unpackFunctionArguments cdr
        | _ -> []

    let rec getFormatter<'State, 'Residue, 'Result> (els: FormatElement list) (typ: Type) =
        match els with
        | [] ->
            fun (state: FormatTransformer<'Result>) -> (state null).Finish()
            |> box
        | _ when els.Length <= 5 && List.forall (fun e -> e.typ <> 't' && e.typ <> 'a') els ->
            let args = unpackFunctionArguments typ |> List.toArray
            let els = els |> List.toArray
            getStringFormatterOptN<'Result> args.Length args (Array.append (els |> Array.map box) (Array.map2 toString els args))
        | x :: xs ->
            let arg, cont = getFunctionElements typ 

            match x.typ with
            | 't' ->
                let next = getFormatter<'State, 'Residue, 'Result> xs cont

                getGenericFormatterT<'State, 'Residue, 'Result> x next cont
            | 'a' ->
                let arg, cont = getFunctionElements cont
                let next = getFormatter<'State, 'Residue, 'Result> xs cont

                getGenericFormatterA<'State, 'Residue, 'Result> x next arg cont
            | _ ->
                let str = toString x arg
                let next = getFormatter<'State, 'Residue, 'Result> xs cont

                getStringFormatterNext<'Result> x str next arg cont

    type StringConcatFormatContext<'Result>(prefix: string, finish) =
        let mutable state = prefix

        interface FormatContext<'Result> with
            member this.Apply(f, e) = (this :> FormatContext<'Result>).Append(f null :?> string, e)
            member this.Append(s, e) = state <- String.Concat(state, s, e.postfix)
            member this.Finish() = finish state

    type StringJoinFormatContext<'Result>(prefix: string, count, finish) =
        let state = Array.zeroCreate (count * 2 + 1)
        do state.[0] <- prefix

        let mutable index = 1

        interface FormatContext<'Result> with
            member this.Apply(f, e) = (this :> FormatContext<'Result>).Append(f null :?> string, e)
            member this.Append(s, e) =
                state.[index] <- s
                state.[index + 1] <- e.postfix
                index <- index + 2
            member this.Finish() =
                assert (index = state.Length)
                finish (String.Join("", state))

    type TextWriterFormatContext<'Result>(writer: TextWriter, prefix: string, finish) =
        do writer.Write(prefix)

        interface FormatContext<'Result> with
            member this.Apply(f, e) = f (box writer) |> ignore; writer.Write(e.postfix)
            member this.Append(s, e) = writer.Write(s); writer.Write(e.postfix)
            member this.Finish() = finish ()

    type StringBuilderFormatContext<'Result>(builder: StringBuilder, prefix: string, finish) =
        do builder.Append(prefix) |> ignore

        interface FormatContext<'Result> with
            member this.Apply(f, e) = f (box builder) |> ignore; builder.Append(e.postfix) |> ignore
            member this.Append(s, e) = builder.Append(s).Append(e.postfix) |> ignore
            member this.Finish() = finish ()

    let inline getStringFormatContext prefix count (formatter: FormatTransformer<'Result> -> 'Printer) cont =
        if count <= 1 then
            formatter (fun _ -> StringConcatFormatContext<'Result>(prefix, cont) :> FormatContext<'Result>)
        else
            formatter (fun _ -> StringJoinFormatContext<'Result>(prefix, count, cont) :> FormatContext<'Result>)

#if FASTPRINTF_COMPAT_FX3
    type Cache<'K, 'V when 'K : equality>(generator) =
        let data = Dictionary<'K, 'V>()

        member this.Item key =
            lock data (fun _ ->
                let mutable value = Unchecked.defaultof<'V>
                if data.TryGetValue(key, &value) then value
                else
                    let value = generator key
                    data.Add(key, value)
                    value)
#else
    type Cache<'K, 'V when 'K : equality>(generator) =
        let data = ConcurrentDictionary<'K, 'V>()
        let creator = Func<'K, 'V>(generator)

        member this.Item key =
            data.GetOrAdd(key, creator)
#endif

    type LocalCache<'K, 'V when 'K: equality>() =
        let mutable key = Unchecked.defaultof<'K>
        let mutable value = Unchecked.defaultof<'V>

        member this.Item k (cache: Cache<'K, 'V>) =
            if Object.ReferenceEquals(key, k) then
                value
            else
                let v = cache.Item k
                key <- k
                value <- v
                v

    type PrintfCache<'Printer, 'State, 'Residue, 'Result>() =
        static let cache = Cache<string, _>(fun fmt ->
            let prefix, els = parseFormatString fmt
            let count = els.Length
            let formatter = getFormatter<'State, 'Residue, 'Result> els typeof<'Printer> :?> FormatTransformer<'Result> -> 'Printer
            prefix, count, formatter)

        [<ThreadStatic; DefaultValue>]
        static val mutable private local: LocalCache<string, string * int * (FormatTransformer<'Result> -> 'Printer)>

        static member Item fmt =
            let lc = PrintfCache<'Printer, 'State, 'Residue, 'Result>.local
            if Object.ReferenceEquals(lc, null) then
                let lc = LocalCache<_, _>()
                PrintfCache<'Printer, 'State, 'Residue, 'Result>.local <- lc
                lc.Item fmt cache
            else
                lc.Item fmt cache

    let gprintf (fmt: #Format<'Printer, 'State, 'Residue, 'Result>) =
        PrintfCache<'Printer, 'State, 'Residue, 'Result>.Item fmt.Value

    let idString: string -> string = id
    let idUnit : unit -> unit = id

    type PrintfCacheString<'Printer>() =
        static let cache = Cache<string, _>(fun fmt ->
            let prefix, count, formatter = PrintfCache<'Printer, unit, string, string>.Item fmt
            getStringFormatContext prefix count formatter idString)

        [<ThreadStatic; DefaultValue>]
        static val mutable private local: LocalCache<string, 'Printer>

        static member Item fmt =
            let lc = PrintfCacheString<'Printer>.local
            if Object.ReferenceEquals(lc, null) then
                let lc = LocalCache<_, _>()
                PrintfCacheString<'Printer>.local <- lc
                lc.Item fmt cache
            else
                lc.Item fmt cache

module Printf =
    open PrintfImpl

    let kbprintf (cont: unit -> 'Result) (builder: StringBuilder) (fmt: #BuilderFormat<'Printer, 'Result>): 'Printer =
        let prefix, count, formatter = gprintf fmt
        formatter (fun _ -> StringBuilderFormatContext<'Result>(builder, prefix, cont) :> FormatContext<'Result>)

    let kfprintf (cont: unit -> 'Result) (writer: TextWriter) (fmt: #TextWriterFormat<'Printer, 'Result>): 'Printer =
        let prefix, count, formatter = gprintf fmt
        formatter (fun _ -> TextWriterFormatContext<'Result>(writer, prefix, cont) :> FormatContext<'Result>)

    let ksprintf (cont: string -> 'Result) (fmt: #StringFormat<'Printer, 'Result>): 'Printer =
        let prefix, count, formatter = gprintf fmt
        getStringFormatContext prefix count formatter cont

    let bprintf (builder: StringBuilder) (fmt: #BuilderFormat<'Printer>): 'Printer = kbprintf idUnit builder fmt

    let fprintf (writer: TextWriter) (fmt: #TextWriterFormat<'Printer>): 'Printer = kfprintf idUnit writer fmt
    let fprintfn (writer: TextWriter) (fmt: #TextWriterFormat<'Printer>): 'Printer = kfprintf (fun _ -> writer.WriteLine()) writer fmt

    let printf (fmt: #TextWriterFormat<'Printer>): 'Printer = fprintf Console.Out fmt
    let printfn (fmt: #TextWriterFormat<'Printer>): 'Printer = fprintfn Console.Out fmt

    let eprintf (fmt: #TextWriterFormat<'Printer>): 'Printer = fprintf Console.Error fmt
    let eprintfn (fmt: #TextWriterFormat<'Printer>): 'Printer = fprintfn Console.Error fmt

    let sprintf (fmt: #StringFormat<'Printer>): 'Printer =
        PrintfCacheString<'Printer>.Item fmt.Value

    let kprintf (cont: string -> 'Result) (fmt: #StringFormat<'Printer, 'Result>): 'Printer = ksprintf cont fmt

    let failwithf (fmt: #StringFormat<'Printer, 'Result>): 'Printer = ksprintf failwith fmt

let printf (fmt: #TextWriterFormat<'Printer>): 'Printer = Printf.printf fmt
let printfn (fmt: #TextWriterFormat<'Printer>): 'Printer = Printf.printfn fmt

let eprintf (fmt: #TextWriterFormat<'Printer>): 'Printer = Printf.eprintf fmt
let eprintfn (fmt: #TextWriterFormat<'Printer>): 'Printer = Printf.eprintfn fmt

let sprintf (fmt: #StringFormat<'Printer>): 'Printer = Printf.sprintf fmt

let failwithf (fmt: #StringFormat<'Printer, 'Result>): 'Printer = Printf.failwithf fmt

let fprintf (writer: TextWriter) (fmt: #TextWriterFormat<'Printer>): 'Printer = Printf.fprintf writer fmt
let fprintfn (writer: TextWriter) (fmt: #TextWriterFormat<'Printer>): 'Printer = Printf.fprintfn writer fmt

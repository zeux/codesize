namespace Converters

open System.Windows.Markup
open System.Windows.Data

open Symbols

type BaseConverter() =
    inherit MarkupExtension()

    override this.ProvideValue(sp) = box this

    interface IValueConverter with
        member this.Convert(value, targetType, parameter, culture) = failwith "Not implemented"
        member this.ConvertBack(value, targetType, parameter, culture) = failwith "Not implemented"

type FnConverter<'T, 'U>(fn: 'T -> 'U) =
    inherit BaseConverter()

    interface IValueConverter with
        member this.Convert(value, targetType, parameter, culture) = box (fn (value :?> 'T))

type IsNotNull() =
    inherit FnConverter<obj, bool>(fun v -> v <> null)

module Impl =
    let getSymbolText sym =
        sym.name + (if sym.section = "" then "" else " [" + sym.section + "]")

    let getFileText file =
        if file.lineBegin = file.lineEnd then
            sprintf "%s:%d" file.file file.lineBegin
        else
            sprintf "%s:%d-%d" file.file file.lineBegin file.lineEnd

type ListItem() =
    inherit FnConverter<obj, string>(fun item ->
        match item with
        | :? Symbol as sym -> sym.size.ToString("#,0 ") + Impl.getSymbolText sym
        | :? FileLineRange as file -> file.size.ToString("#,0 ") + Impl.getFileText file
        | o -> failwithf "Unsupported type %O" $ o.GetType())

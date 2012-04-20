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

type SymbolListItem() =
    inherit FnConverter<Symbol, string>(fun sym ->
        sym.size.ToString("#,0 ") + sym.name + (if sym.section = "" then "" else " [" + sym.section + "]"))
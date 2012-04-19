namespace Converters

open System.Windows.Markup
open System.Windows.Data

type BaseConverter() =
    inherit MarkupExtension()

    override this.ProvideValue(sp) = box this

    interface IValueConverter with
        member this.Convert(value, targetType, parameter, culture) = failwith "Not implemented"
        member this.ConvertBack(value, targetType, parameter, culture) = failwith "Not implemented"

type IsNotNull() =
    inherit BaseConverter()

    interface IValueConverter with
        member this.Convert(value, targetType, parameter, culture) = box (value <> null)
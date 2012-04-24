namespace UI

open System.Windows
open System.Windows.Markup
open System.Windows.Data

open Symbols

type BaseConverter() =
    inherit MarkupExtension()

    override this.ProvideValue(sp) = box this

type FnConverter<'T, 'U>(fn: 'T -> 'U) =
    inherit BaseConverter()

    interface IValueConverter with
        member this.Convert(value, targetType, parameter, culture) = box (fn (value :?> 'T))
        member this.ConvertBack(value, targetType, parameter, culture) = failwith "Not implemented"

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
        | o -> string o)

type TreeNodeHeader() =
    inherit FnConverter<obj, string>(fun item ->
        match item with
        | :? Symbol as sym -> Impl.getSymbolText sym + sym.size.ToString(" (#,0)")
        | :? FileLineRange as file -> Impl.getFileText file + file.size.ToString(" (#,0)")
        | :? TreeView.Group as group -> group.Prefix + group.Size.ToString(" (#,0)")
        | o -> string o)

type TreeNodeItems() =
    inherit BaseConverter()

    let dummy = [|obj()|]
    let empty = [||]

    interface IMultiValueConverter with
        member this.Convert(values, targetType, parameter, culture) =
            match values with
            | [|:? bool as expanded; :? TreeView.Group as group|] ->
                if expanded then group.Items else dummy
            | _ -> empty
            |> box

        member this.ConvertBack(values, targetType, parameter, culture) = failwith "Not implemented"

type BoolToVisibility() =
    inherit FnConverter<bool, Visibility>(fun item ->
        if item then Visibility.Visible else Visibility.Hidden)

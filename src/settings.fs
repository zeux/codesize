namespace UI

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.ComponentModel
open System.IO
open System.Windows
open System.Windows.Data
open System.Windows.Markup
open System.Xml

module Settings =
    type Item() =
        let mutable value = null
        let event = Event<_, _>()

        member this.Value
            with get () = value
             and set v =
                value <- v
                event.Trigger(this, PropertyChangedEventArgs("Value"))

        interface INotifyPropertyChanged with
            [<CLIEvent>]
            member this.PropertyChanged = event.Publish

    type Group() =
        let data = ConcurrentDictionary<string, Item>()

        member this.Item key = data.GetOrAdd(key, fun key -> Item())

        member this.Load (path: string) =
            let doc = XmlDocument()
            doc.Load(path)

            doc.SelectNodes("settings/item")
            |> Seq.cast<XmlElement>
            |> Seq.map (fun node -> node.GetAttribute("name"), Type.GetType(node.GetAttribute("type")), node.GetAttribute("value"))
            |> Seq.map (fun (name, typ, value) -> name, TypeDescriptor.GetConverter(typ).ConvertFromInvariantString(value))
            |> Seq.iter (fun (name, value) -> this.[name].Value <- value)

        member this.Save (path: string) =
            let doc = XmlDocument()
            let root = doc.AppendChild(doc.CreateElement("settings"))

            data.ToArray()
            |> Array.map (fun p -> p.Key, p.Value.Value)
            |> Array.iter (fun (name, value) ->
                let item = doc.CreateElement("item")
                item.SetAttribute("name", name)
                item.SetAttribute("type", value.GetType().AssemblyQualifiedName)
                item.SetAttribute("value", TypeDescriptor.GetConverter(value).ConvertToInvariantString(value))
                root.AppendChild(item) |> ignore)

            Directory.CreateDirectory(Path.GetDirectoryName(path)) |> ignore
            doc.Save(path)

    let current = Group()

type AutoSave(defaultValue) =
    inherit MarkupExtension()

    override this.ProvideValue(sp) =
        match sp.GetService(typeof<IProvideValueTarget>) with
        | :? IProvideValueTarget as target ->
            let tobj = target.TargetObject :?> DependencyObject
            let tprop = target.TargetProperty :?> DependencyProperty

            let name = tobj.GetType().GetProperty("Name").GetValue(tobj, [||]) :?> string

            if String.IsNullOrEmpty(name) then
                failwith "AutoSave properties require parent element to have a name"

            let item = Settings.current.[sprintf "%s/%s" name tprop.Name]

            if item.Value = null then
                item.Value <- TypeDescriptor.GetConverter(tprop.PropertyType).ConvertFromInvariantString(defaultValue)

            Binding(Source = item, Path = PropertyPath("Value"), Mode = BindingMode.TwoWay).ProvideValue(sp)
        | _ -> null
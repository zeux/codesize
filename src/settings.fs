namespace UI

#nowarn "9" // StructLayout produces 'Uses of this construct may result in the generation of unverifiable .NET IL code' warning

open System
open System.Collections.Concurrent
open System.ComponentModel
open System.IO
open System.Runtime.InteropServices
open System.Windows
open System.Windows.Data
open System.Windows.Markup
open System.Windows.Interop
open System.Xml

module private Win32 =
    let SW_SHOWNORMAL = 1
    let SW_SHOWMINIMIZED = 2

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type WINDOWPLACEMENT =
        val mutable length: int
        val mutable flags: int
        val mutable showCmd: int
        val mutable minPositionX: int
        val mutable minPositionY: int
        val mutable maxPositionX: int
        val mutable maxPositionY: int
        val mutable normalPositionLeft: int
        val mutable normalPositionTop: int
        val mutable normalPositionRight: int
        val mutable normalPositionBottom: int

    [<DllImport("user32")>] extern bool SetWindowPlacement(nativeint handle, [<In>] WINDOWPLACEMENT& placement)
    [<DllImport("user32")>] extern bool GetWindowPlacement(nativeint handle, WINDOWPLACEMENT& placement)

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

type Window() =
    static let loadHandler = EventHandler(fun e args ->
        let window = e :?> Windows.Window
        let handle = WindowInteropHelper(window).Handle
        let placement = Settings.current.[sprintf "%s/Placement" window.Name].Value :?> string
        let pldata = if placement = null then [||] else placement.Split(null) |> Array.map (Int32.TryParse) |> Array.choose (fun (r, v) -> if r then Some v else None)

        match pldata with
        | [|flags; showCmd; minPositionX; minPositionY; maxPositionX; maxPositionY; normalPositionLeft; normalPositionTop; normalPositionRight; normalPositionBottom|] ->
            let showCmdFix = if showCmd = Win32.SW_SHOWMINIMIZED then Win32.SW_SHOWNORMAL else showCmd
            let mutable pl =
                Win32.WINDOWPLACEMENT(
                    length = Marshal.SizeOf(typeof<Win32.WINDOWPLACEMENT>),
                    flags = flags, showCmd = showCmdFix,
                    minPositionX = minPositionX, minPositionY = minPositionY,
                    maxPositionX = maxPositionX, maxPositionY = maxPositionY,
                    normalPositionLeft = normalPositionLeft, normalPositionTop = normalPositionTop,
                    normalPositionRight = normalPositionRight, normalPositionBottom = normalPositionBottom)
            Win32.SetWindowPlacement(handle, &pl) |> ignore
        | _ -> ())
        
    static let saveHandler = CancelEventHandler(fun e args ->
        let window = e :?> Windows.Window
        let handle = WindowInteropHelper(window).Handle
        let mutable pl = Win32.WINDOWPLACEMENT(length = Marshal.SizeOf(typeof<Win32.WINDOWPLACEMENT>))
        if Win32.GetWindowPlacement(handle, &pl) then
            Settings.current.[sprintf "%s/Placement" window.Name].Value <-
                sprintf "%d %d %d %d %d %d %d %d %d %d" pl.flags pl.showCmd pl.minPositionX pl.minPositionY pl.maxPositionX pl.maxPositionY
                    pl.normalPositionLeft pl.normalPositionTop pl.normalPositionRight pl.normalPositionBottom)
        
    static let autoSavePlacement =
        DependencyProperty.RegisterAttached("AutoSavePlacement",
            typeof<bool>, typeof<Windows.Window>, PropertyMetadata(false))

    static member GetAutoSavePlacement(window: Windows.Window) =
        window.GetValue(autoSavePlacement) :?> bool

    static member SetAutoSavePlacement(window: Windows.Window, value: bool) =
        if value <> Window.GetAutoSavePlacement window then
            if value then
                if String.IsNullOrEmpty(window.Name) then
                    failwith "AutoSave properties require parent element to have a name"

                window.SourceInitialized.AddHandler(loadHandler)
                window.Closing.AddHandler(saveHandler)
            else
                window.SourceInitialized.RemoveHandler(loadHandler)
                window.Closing.RemoveHandler(saveHandler)

            window.SetValue(autoSavePlacement, value)
namespace UI

open System
open System.Diagnostics
open System.Reflection
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Navigation
open System.Windows.Markup
open System.Xaml

type TextBox() =
    static let handler = 
        MouseButtonEventHandler(fun e args -> (args.Source :?> Controls.TextBox).SelectAll())

    static let doubleClickSelectsAll =
        DependencyProperty.RegisterAttached("DoubleClickSelectsAll",
            typeof<bool>, typeof<Controls.TextBox>, PropertyMetadata(false))

    static member GetDoubleClickSelectsAll(element: Controls.TextBox) =
        element.GetValue(doubleClickSelectsAll) :?> bool

    static member SetDoubleClickSelectsAll(element: Controls.TextBox, value: bool) =
        if value <> TextBox.GetDoubleClickSelectsAll element then
            if value then
                element.MouseDoubleClick.AddHandler(handler)
            else
                element.MouseDoubleClick.RemoveHandler(handler)

            element.SetValue(doubleClickSelectsAll, value)

type Hyperlink() =
    static let navigateToUri =
        DependencyProperty.RegisterAttached("NavigateToUri",
            typeof<bool>, typeof<Documents.Hyperlink>, PropertyMetadata(false))

    static let handler =
        RequestNavigateEventHandler(fun e args ->
            Process.Start(args.Uri.ToString()) |> ignore)

    static member GetNavigateToUri(element: Documents.Hyperlink) =
        element.GetValue(navigateToUri) :?> bool

    static member SetNavigateToUri(element: Documents.Hyperlink, value: bool) =
        if value <> Hyperlink.GetNavigateToUri element then
            if value then
                element.RequestNavigate.AddHandler(handler)
            else
                element.RequestNavigate.RemoveHandler(handler)

            element.SetValue(navigateToUri, value)

type Method(name: string) =
    inherit MarkupExtension()

    override this.ProvideValue(sp) =
        let root =
            match sp.GetService(typeof<IRootObjectProvider>) with
            | :? IRootObjectProvider as p -> p.RootObject
            | _ -> null

        match sp.GetService(typeof<IProvideValueTarget>) with
        | :? IProvideValueTarget as target ->
            let tobj = target.TargetObject :?> DependencyObject
            let tprop = target.TargetProperty :?> EventInfo
            box $ Delegate.CreateDelegate(tprop.EventHandlerType, root, name)
        | _ -> null

type RootObject() =
    inherit MarkupExtension()

    override this.ProvideValue(sp) =
        match sp.GetService(typeof<IRootObjectProvider>) with
        | :? IRootObjectProvider as p -> p.RootObject
        | _ -> null

type TabItemCloseCommand() =
    inherit MarkupExtension()

    override this.ProvideValue(sp) =
        { new ICommand with
          member this.CanExecute(arg) = true
          member this.add_CanExecuteChanged(h) = ()
          member this.remove_CanExecuteChanged(h) = ()

          member this.Execute(arg) =
            let tabitem = arg :?> TabItem
            let tabcontrol = tabitem.Parent :?> TabControl
            tabcontrol.Items.Remove(tabitem) |> ignore
         } |> box
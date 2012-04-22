namespace Behaviors

open System.Windows
open System.Windows.Controls
open System.Windows.Input

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

module UI.Exception

open System
open System.Windows
open System.Windows.Controls

let showModal owner (e: exn) =
    let window = Application.LoadComponent(Uri("src/ui/exception.xaml", UriKind.Relative)) :?> Window
    let buttonCopy = window?ButtonCopy :?> Button
    let buttonClose = window?ButtonClose :?> Button

    window.Owner <- owner
    window.Tag <- e

    buttonCopy.Click.Add(fun _ ->
        let t = e.ToString()

        Clipboard.SetText(t))

    buttonClose.Click.Add(fun _ ->
        window.Close())

    window.ShowDialog() |> ignore

let showMessage (e: exn) =
    MessageBox.Show(e.ToString(), "Exception", MessageBoxButton.OK, MessageBoxImage.Error) |> ignore
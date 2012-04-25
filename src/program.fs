namespace UI

open System
open System.Windows

type MainWindow() =
    inherit codesize.MainWindow()

module Program =
    [<STAThread>] do ()

    let app = Application(ShutdownMode = ShutdownMode.OnMainWindowClose)
    let window = codesize.window

    app.DispatcherUnhandledException.Add(fun args ->
        UI.Exception.showModal window args.Exception
        args.Handled <- true)

    AppDomain.CurrentDomain.UnhandledException.Add(fun args ->
        UI.Exception.showMessage (unbox args.ExceptionObject))

    app.Run(window) |> ignore
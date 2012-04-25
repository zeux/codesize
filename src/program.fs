namespace UI

open System
open System.IO
open System.Windows

type MainWindow() =
    inherit codesize.MainWindow()

module Program =
    [<STAThread>] do ()

    let app = Application(ShutdownMode = ShutdownMode.OnMainWindowClose)

    let settingsPath = sprintf "%s\\codesize\\settings.xml" $ Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)

    if File.Exists(settingsPath) then try UI.Settings.current.Load settingsPath with _ -> ()
    app.Exit.Add(fun _ -> try UI.Settings.current.Save settingsPath with _ -> ())

    let window = codesize.window

    app.DispatcherUnhandledException.Add(fun args ->
        UI.Exception.showModal window args.Exception
        args.Handled <- true)

    AppDomain.CurrentDomain.UnhandledException.Add(fun args ->
        UI.Exception.showMessage (unbox args.ExceptionObject))

    app.Run(window) |> ignore
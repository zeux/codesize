module UI.Exception

open System
open System.Collections.Specialized
open System.Net
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

let uploadReport (e: exn) =
    let wc = new WebClient()
    wc.Headers.Add("Content-Type", "application/x-www-form-urlencoded")

    let data = NameValueCollection()
    data.Add("Description", e.ToString())
    data.Add("ScoutUserName", "CrashReporter")
    data.Add("ScoutProject", "codesize")
    data.Add("ScoutArea", "Crash")
    data.Add("ForceNewBug", "0")

    try wc.UploadValues("https://codesize.fogbugz.com/scoutSubmit.asp", "POST", data) |> ignore
    with :? WebException -> ()

let reportUploader =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () = async {
            let! (msg: obj) = inbox.Receive()
            match msg with
            | :? exn as e -> uploadReport e
            | :? AsyncReplyChannel<unit> as ch -> ch.Reply()
            | _ -> ()
            return! loop () }

        loop ())

let uploadReportAsync (e: exn) =
    reportUploader.Post(box e)

let uploadReportWait timeout =
    try reportUploader.PostAndReply((fun ch -> box ch), timeout)
    with :? TimeoutException -> ()

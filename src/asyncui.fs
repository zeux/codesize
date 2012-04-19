module AsyncUI

open System.Threading
open System.Threading.Tasks
open System.Windows
open System.Windows.Threading

type SingleUpdateAgent() =
    let mutable cancel = new CancellationTokenSource()

    member this.Post item =
        // cancel any existing task
        cancel.Cancel()

        // launch a new task
        // note: this does not give us ordering guarantees yet; I'm still figuring it out
        cancel <- new CancellationTokenSource()
        Async.Start(item, cancel.Token)

let private uiContext = lazy DispatcherSynchronizationContext(Application.Current.Dispatcher)

let switchToUI () =
    Async.SwitchToContext uiContext.Value

let switchToWork () =
    Async.SwitchToThreadPool ()

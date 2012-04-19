[<AutoOpen>]
module Common

let ($) a b = a b
let (?) (e: System.Windows.FrameworkElement) (name: string) = e.FindName name

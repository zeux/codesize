module Editor

open System
open System.Windows
open System.Windows.Input
open System.Windows.Media

open ICSharpCode.AvalonEdit
open ICSharpCode.AvalonEdit.Rendering
open ICSharpCode.AvalonEdit.Search

type HighlightLineBackgroundRenderer(editor: TextEditor) as this =
    let mutable lineRange = 0, 0

    do editor.TextArea.TextView.BackgroundRenderers.Add(this :> IBackgroundRenderer)

    member this.LineRange
        with get () = lineRange
         and set v = lineRange <- v
    
    interface IBackgroundRenderer with
        member this.Layer = KnownLayer.Background
        member this.Draw(textView, drawingContext) =
            textView.EnsureVisualLines()

            for line in fst lineRange .. snd lineRange do
                if line > 0 && line < editor.Document.LineCount then
                    let cline = editor.Document.GetLineByNumber(line)
                    let rects = BackgroundGeometryBuilder.GetRectsForSegment(textView, cline)

                    for rect in rects do
                        drawingContext.DrawRectangle(Brushes.LightSteelBlue, null,
                            Rect(rect.Location, Size(textView.ActualWidth, rect.Height)))

type ActionCommand(action) =
    interface ICommand with 
        member this.add_CanExecuteChanged h = ()
        member this.remove_CanExecuteChanged h = ()
        member this.CanExecute(o) = true
        member this.Execute(o) = action o

type Window() as this =
    let window = Application.LoadComponent(Uri("src/ui/editor.xaml", UriKind.Relative)) :?> System.Windows.Window
    let editor = window?TextEditor :?> TextEditor

    let hlRenderer = HighlightLineBackgroundRenderer(editor)

    do
        editor.TextArea.DefaultInputHandler.NestedInputHandlers.Add(SearchInputHandler(editor.TextArea))

        let escBinding = InputBinding(ActionCommand(fun _ -> this.Close()), KeyGesture(Key.Escape))
        window.InputBindings.Add(escBinding) |> ignore
        editor.InputBindings.Add(escBinding) |> ignore
        editor.TextArea.InputBindings.Add(escBinding) |> ignore

        window.Closing.Add(fun args ->
            this.Close()
            args.Cancel <- true)

    member this.Open(file, line, ?highlightRange) =
        window.Title <- file

        editor.Load(file)

        if editor.IsLoaded then
            editor.ScrollToLine(line)
        else
            editor.Loaded.Add(fun _ -> editor.ScrollToLine(line))

        hlRenderer.LineRange <- defaultArg highlightRange (line, line)

        window.Show()
        window.Focus() |> ignore

    member this.Close() =
        editor.Clear()
        window.Hide()
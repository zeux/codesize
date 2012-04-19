module Editor

open System
open System.Windows
open System.Windows.Input
open System.Windows.Media

open ICSharpCode.AvalonEdit
open ICSharpCode.AvalonEdit.Rendering

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


type Window() as this =
    let window = Application.LoadComponent(Uri("src/ui/editor.xaml", UriKind.Relative)) :?> System.Windows.Window
    let editor = window?TextEditor :?> TextEditor

    let hlRenderer = HighlightLineBackgroundRenderer(editor)

    let scrollLine = ref 1

    do
        window.KeyDown.Add(fun args ->
            if args.Key = Key.Escape then this.Close())

        window.Closing.Add(fun args ->
            this.Close()
            args.Cancel <- true)

        editor.Loaded.Add(fun _ ->
            editor.ScrollToLine(!scrollLine))

        editor.TextChanged.Add(fun _ ->
            editor.ScrollToLine(!scrollLine))

    member this.Open(file, line, ?highlightRange) =
        window.Title <- file
        scrollLine := line

        editor.Load(file)
        editor.ScrollToLine(line)

        hlRenderer.LineRange <- defaultArg highlightRange (line, line)

        window.Show()

    member this.Close() =
        editor.Clear()
        window.Hide()
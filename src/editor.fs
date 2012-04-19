module Editor

open System
open System.Windows
open System.Windows.Input
open System.Windows.Media

open ICSharpCode.AvalonEdit
open ICSharpCode.AvalonEdit.Rendering

type HighlightLineBackgroundRenderer(editor: TextEditor, lineRange) =
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

        let hlRenderer = HighlightLineBackgroundRenderer(editor, defaultArg highlightRange (line, line))
        editor.TextArea.TextView.BackgroundRenderers.Clear()
        editor.TextArea.TextView.BackgroundRenderers.Add(hlRenderer :> IBackgroundRenderer)

        window.Show()

    member this.Close() =
        editor.Clear()
        window.Hide()
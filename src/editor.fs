module Editor

open System
open System.Windows
open System.Windows.Media

open ICSharpCode.AvalonEdit
open ICSharpCode.AvalonEdit.Rendering

type HighlightLineBackgroundRenderer(editor: TextEditor, lineRange) =
    interface IBackgroundRenderer with
        member this.Layer = KnownLayer.Background
        member this.Draw(textView, drawingContext) =
            textView.EnsureVisualLines()

            for line in fst lineRange .. snd lineRange do
                let cline = editor.Document.GetLineByNumber(line)
                let rects = BackgroundGeometryBuilder.GetRectsForSegment(textView, cline)

                for rect in rects do
                    drawingContext.DrawRectangle(Brushes.LightSteelBlue, null,
                        Rect(rect.Location, Size(textView.ActualWidth, rect.Height)))


type Window () =
    let window = Application.LoadComponent(Uri("src/ui/editor.xaml", UriKind.Relative)) :?> System.Windows.Window
    let editor = window?TextEditor :?> TextEditor

    member this.Load(file, line, ?highlightRange) =
        window.Title <- file
        editor.Load(file)
        editor.Loaded.Add(fun _ -> editor.ScrollToLine(line))

        let hlRenderer = HighlightLineBackgroundRenderer(editor, defaultArg highlightRange (line, line))
        editor.TextArea.TextView.BackgroundRenderers.Clear()
        editor.TextArea.TextView.BackgroundRenderers.Add(hlRenderer :> IBackgroundRenderer)

    member this.Window = window
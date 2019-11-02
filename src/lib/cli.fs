namespace Fons

open Components
open System
open System.Text

module cli = 
    /// Types for parsing keys returned from the console using active patterns
    module KeyPatterns =
        type Direction = | Up | Down | Left | Right
        let (|Esc|_|) (cki:ConsoleKeyInfo) = if cki.Key = ConsoleKey.Escape then Some Esc else None
        let (|CR|_|) (cki:ConsoleKeyInfo)  = if (int cki.KeyChar) = 10 || (int cki.KeyChar) = 13 then Some CR else None
        let (|BS|_|) (cki:ConsoleKeyInfo) =  if cki.Key = ConsoleKey.Backspace then Some BS else None
        let (|Printable|_|) (cki:ConsoleKeyInfo) = 
            let ci = int cki.KeyChar
            if 32 <= ci && ci <= 126 
            then Some(Printable cki.KeyChar) 
            else None
                
        let (|Arrow|_|) (cki:ConsoleKeyInfo) =
            match cki.Key with
            | ConsoleKey.LeftArrow ->  Some <| Arrow Left
            | ConsoleKey.RightArrow -> Some <| Arrow Right
            | ConsoleKey.UpArrow ->    Some <| Arrow Up
            | ConsoleKey.DownArrow ->  Some <| Arrow Down
            | _ -> None

        let (|Page|_|) (cki:ConsoleKeyInfo) =
            match cki.Key with
            | ConsoleKey.PageUp ->     Some <| Page Up
            | ConsoleKey.PageDown ->   Some <| Page Down
            | _ -> None

    open KeyPatterns
    open Internal

    /// Defines the result of processing the input from the command line. Each time the user pressed enter and submits
    /// a command, the cmdProcssor is called and should return this result, indicating if the command line should continue,
    /// and if it is to continue, passing the render state which would account for rendering done by the cmdProcessor.
    type CmdProcessorResult = {
        UpdatedState: RenderState
        ExitCLI: bool
    }

    let getPos () = Console.CursorTop, Console.CursorLeft

    /// Main command loop. Accepts functions for returning the current prompt as a RenderCmd, a command processor
    /// which accepts the user input, parses & processes it, and returns a CmdProcessorResult indicating what
    /// to do next, and the current renderState (in case the command processor has rendered to the TUI). 
    /// It returns updated renderState when the command loop is exited.
    let cmdLine getPrompt cmdProcessor syntaxHighligher renderState = 
        let rec readKeyLoop state idx (sb:StringBuilder) =
            let syntax = syntaxHighligher |> Option.defaultValue (fun s -> write s)
            let currentLineContent = 
                let r,c = getPos ()
                /// a field consisting of a label and value
                let field lbl txt = block [ write lbl; text [fg 200 200 0; bold] txt ]
                block [
                    saveExcursion
                    // write the "status bar", then move the cursor back to where it was
                    div [bg 0x08 0x79 0xc9; fg 200 200 200] [
                        pos 1 1
                        clrLineToEnd
                        write (new string(' ', Console.WindowWidth))
                        pos 1 1
                        (field " row:" (sprintf "%03i" r))
                        (field " col:" (sprintf "%03i" c))
                        (field " len:" (sprintf "%03i" sb.Length))
                        write " | climode --INSERT--"
                    ]
                    restoreExcursion
                    // clear the current line, move back to the start by moving left col# times, then
                    // write the prompt and the curent text value
                    clrLine
                    left c
                    (getPrompt())
                    (syntax (sb.ToString()))
                ]
            let idxAdj = block [ if sb.Length = idx then () else yield left (sb.Length - idx)]
            let state' = render [currentLineContent; idxAdj] state
            let ki = Console.ReadKey true
            
            match ki with
            | Esc -> None
            | CR ->
                let s = string sb
                sb.Clear() |> ignore
                Some s
            | Printable c -> readKeyLoop state' (idx + 1) (sb.Insert(idx,c))
            | Arrow Left ->  readKeyLoop state' (max 0 (idx - 1)) sb
            | Arrow Right -> readKeyLoop state' (min (sb.Length) (idx + 1)) sb

            | BS ->
                let idx' =
                    if idx > 0 then
                        sb.Remove(idx - 1, 1) |> ignore
                        idx - 1
                    else
                        idx
                readKeyLoop state' idx' sb
            | _ -> 
                readKeyLoop state' idx sb
        
        let sb = StringBuilder(1000)
        let rec responseProcessor state =
            match readKeyLoop state 0 sb with
            | Some s when s.Length > 0 -> 
                let rsp = cmdProcessor s state
                if not rsp.ExitCLI then
                    sb.Clear() |> ignore 
                    responseProcessor rsp.UpdatedState
                else
                    rsp.UpdatedState
            | Some _ ->
                responseProcessor <| render [br] state
            | None ->
                state
        
        let updatedRenderState = responseProcessor renderState
        render [br] updatedRenderState

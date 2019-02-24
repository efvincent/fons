namespace Fons

open Components
open System
open System.Text

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

module cli = 
    open KeyPatterns

    let getPos () = Console.CursorTop, Console.CursorLeft

    let cmdLine getPrompt state = 
        
        let rec loop state idx (sb:StringBuilder) =
            let content = 
                let r,c = getPos ()
                block [
                    saveExcursion
                    div [bg 0 0x65 0xb3] [
                        pos 1 1
                        clrLineToEnd
                        write (new String(' ', Console.WindowWidth))
                        pos 1 1
                        text [fg 255 255 255] " row:"
                        text [fg 255 255 0; bold] (sprintf "%03i" r)
                        text [fg 255 255 255] " col:"
                        text [fg 255 255 0; bold] (sprintf "%03i" c)
                        text [fg 255 255 255] " len:"
                        text [fg 255 128 0; bold] (sprintf "%03i" sb.Length)
                        write " | climode --INSERT--"
                    ]
                    restoreExcursion
                    clrLine
                    left c
                    (getPrompt())
                    text [] (sb.ToString())
                ]
            let idxAdj = block [ if sb.Length = idx then () else yield left (sb.Length - idx)]
            let state' = render [content; idxAdj] state
            let ki = Console.ReadKey true
            
            match ki with
            | Esc -> None
            | CR ->
                let s = string sb
                sb.Clear() |> ignore
                Some s
            | Printable c -> loop state' (idx + 1) (sb.Insert(idx,c))
            | Arrow Left ->  loop state' (max 0 (idx - 1)) sb
            | Arrow Right -> loop state' (min (sb.Length) (idx + 1)) sb

            | BS ->
                let idx' =
                    if idx > 0 then
                        sb.Remove(idx - 1, 1) |> ignore
                        idx - 1
                    else
                        idx
                loop state' idx' sb
            | _ -> 
                loop state' idx sb
        
        let echoPrompt =
            block
                [
                    space
                    text [fg 0x7a 0x6f 0xfa; bold] "echo"
                    space
                    text [fg 0xf0 0xff 0x20; bold] ">>>"
                    space
                ]
        let sb = new StringBuilder(1000)
        let rec echoLoop state =
            match loop state 0 sb with
            | Some s when s.Length > 0 -> 
                let state' = render [block [br; br; echoPrompt; text [(fg 180 180 20)] s; br; br]] state
                sb.Clear() |> ignore 
                echoLoop state'
            | Some _ ->
                echoLoop <| render [br] state
            | None ->
                state
        
        let state' = echoLoop state
        render [block [br; text [] "Done..."; br]] state'

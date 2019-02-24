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

    let cmdLine state = 
        let prompt = 
            block
                [
                    space
                    text [fg 0 0 255] "FlightDeck"
                    text [fg 220 220 20] "@"
                    text [fg 250 100 100] "PROD"
                    space
                    text [fg 100 100 100] "$"
                    space
                ]
        let idxoffset = 19

        let rec loop state idx (sb:StringBuilder) =
            let content = 
                block
                    [
                        clrLine
                        left 1000
                        prompt
                        text [] (sb.ToString())
                        left 1000
                        right (idx + idxoffset)
                    ]
            let state' = render [content] state
            let ki = Console.ReadKey true
            
            match ki with
            | Esc -> None
            | CR ->
                let s = string sb
                sb.Clear() |> ignore
                Some s
            | Printable c -> loop state (idx + 1) (sb.Insert(idx,c))
            | Arrow Left ->  loop state (max 0 (idx - 1)) sb
            | Arrow Right -> loop state (min (sb.Length) (idx + 1)) sb

            | BS ->
                let idx' =
                    if idx > 0 then
                        sb.Remove(idx - 1, 1) |> ignore
                        idx - 1
                    else
                        idx
                loop state idx' sb
            | _ -> 
                loop state idx sb
        
        let echoPrompt =
            block
                [
                    space
                    text [fg 0x7a 0x6f 0xfa; bold] "echo"
                    space
                    text [fg 0xf0 0xff 0x20; bold] ">>>"
                    space
                ]
        let rec echoLoop state =
            let sb = new StringBuilder(1000)
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

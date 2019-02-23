open System
open Fons

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

open Components
open KeyPatterns
open System.Text

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


let prog () =    

    let styleTest = 
        [
            write "plain text "
            div [bg 255 255 0; fg 0 0 0] [ 
                write "black on yellow " 
                text [fg 50 50 255; bold] "\nblue and bold on yellow"
                div [uline] [
                    textln [bg 255 0 255] "\nblue and bold on purple underlined"
                    writeln "blue, bold on yellow, uline"
                ]
                text [] " black on yellow "
            ]
            text [fg 255 120 120] "red on black "
            write "plain text"
            br
        ] 

    let many count cmd =
        let rec loop acc n =
            if n < count then 
                loop (cmd::acc) (n+1)
            else
                acc
        block (loop [] 0)

    let t1 =
        [
            pos 1 1
            write "HOME"; left 4
            right 20
            write "20"; left 2
            down 20
            write "20"; left 2
            left 20 
            write "20"; left 2
            pos 1 35
            writeln "Done..."
        ]

    let t2 = 
        [
            write "Movement test 2"
            block ([1..20] |> List.map (fun n -> 
                block [ pos (9+n) 20; write (sprintf "%i" n)]))
            br
            writeln "Done"
        ]

    let t3 =
        let line = 
            List.concat [
                [block ([0..6] |> List.map (fun _ -> 
                    write (string "1234567890"))
                )]
                [writeln ""]
            ]
        [
            pos 0 0
            block ([1..40] |> List.map (fun _ -> block line))
            
            block [ pos 1 1; text [bg 220 220 0] " "]
            block [ pos 30 1; text [bg 220 220 0] " "]
            block [ pos 1 30; text [bg 220 220 0] " "]
            block [ pos 30 30; text [bg 220 220 0] " "]

            block [ pos 2 2; text [bg 0 220 220] " "; left 1]
            block [ right 29; text [bg 0 220 220] " "; left 1]
            block [ down 29; text [bg 0 220 220] " "; left 1]
            block [ left 29; text [bg 0 220 220] " "; left 1]

            pos 41 1
            writeln "Done..."
        ]

    render [clrScreen; block t1] initialRenderState 
    |> cmdLine

[<EntryPoint>]
let main argv =
    prog () |> ignore
    0

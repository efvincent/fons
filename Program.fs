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

(* let cmdLine () = async {
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
    let rec loop idx (sb:StringBuilder) = async {
        let content = 
            block
                [
                    clrLine
                    moveLeft 1000
                    prompt
                    text [] (sb.ToString())
                    moveLeft 1000
                    moveRight (idx + idxoffset)
                ]
        do! render content
        let ki = Console.ReadKey true
        
        match ki with
        | Esc -> return None
        | CR ->
            let s = string sb
            sb.Clear() |> ignore
            return Some s
        | Printable c -> return! loop (idx + 1) (sb.Insert(idx,c))
        | Arrow Left ->  return! loop (max 0 (idx - 1)) sb
        | Arrow Right -> return! loop (min (sb.Length) (idx + 1)) sb

        | BS ->
            let idx' =
                if idx > 0 then
                    sb.Remove(idx - 1, 1) |> ignore
                    idx - 1
                else
                    idx
            return! loop idx' sb
        | _ -> 
            return! loop idx sb
    }
    let echoPrompt =
        block
            [
                space
                text [fg 0x7a 0x6f 0xfa; bold] "echo"
                space
                text [fg 0xf0 0xff 0x20; bold] ">>>"
                space
            ]
    let rec echoLoop () = async {
        let sb = new StringBuilder(1000)
        match! loop 0 sb with
        | Some s when s.Length > 0 -> 
            do! render (block [cr; cr; echoPrompt; text [(fg 180 180 20)] s; cr; cr])
            sb.Clear() |> ignore 
            do! echoLoop()
        | Some _ ->
            do! render cr
            do! echoLoop()
        | None ->
            ()
    }
    do! echoLoop ()
    do! render (block [cr; text [] "Done..."; cr])
} *)

let prog () =    
(*    do! render <| block [ setAlt; clrScreen; home ]
    let option n s =
        block
            [
                text [(fg 0xff 0xb9 0x31); bold] " => "
                text [(fg 0 0x95 0xff); uline] "Option" 
                space
                text [(bg 0x80 0x20 0x50); bold; (fg 0xff 0xff 0)] (sprintf "%i:" n); space 
                text [(fg 0x5f 0xba 0x7d); (bg 25 15 85)] (sprintf "%s" s); cr        
            ] 

    do! render <|
        block  
            [
                option 1 "Standalone monolyth"
                option 2 "Web Farm deployed locally"
                option 3 "Microservices in Nomad Cluster"            
            ] 

    // do! ProgressBar.loading()
    
    do! render <|
        div [bg 0 150 10] 
            [
                cr
                text [] "this sentence will have "
                text [] "made up of two parts before the special formatting "
                text [fg 0 0 0] "some gray, bold words "
                text [] "right in the middle"
                cr
            ]
    do! cmdLine()
    do! render <| block [ setNoAlt ]
    *)

    let renderState = 
        render initialRenderState [
            write "plain text "
            div [bg 255 255 0; fg 0 0 0] [ 
                write "black on yellow " 
                text [fg 50 50 255; bold] "\nblue and bold on yellow"
                div [uline] [
                    textLn [bg 255 0 255] "\nblue and bold on purple underlined"
                    writeLn "blue, bold on yellow, uline"
                ]
                text [] " black on yellow "
            ]
            text [fg 255 120 120] "red on black "
            write "plain text"
            br
        ] 
    renderState |> ignore

[<EntryPoint>]
let main argv =
    prog () |> ignore
    0

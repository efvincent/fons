// Learn more about F# at http://fsharp.org

open System
open System

let strToBytes (s:string) =
    System.Text.Encoding.UTF8.GetBytes s

let newLine = strToBytes("\n")

[<RequireQualifiedAccess>]
module Color =
    let private d = [|0; 95; 135; 175; 215; 255|]
    let private a = 
        [|0..239|]
        |> Array.map (fun n ->
            if n < 216 then [|d.[n / 36]; d.[(n % 36) / 6]; d.[n % 6]|]
            else 
                let v = n * 10 - 2152
                [|v; v; v|])

    let convToXTerm r g b = 
      a 
      |> Array.map (Array.zip [|r;g;b|])
      |> Array.map (fun tups -> tups |> Array.map(fun (v1,v2) -> abs(v1-v2)) |> Array.reduce (+))
      |> Array.fold 
        (fun (idxMin,vMin,idxCur) vCur -> if vCur <= vMin then (idxCur,vCur,idxCur+1) else (idxMin,vMin,idxCur+1)) 
        (-1,257,0)
      |> fun (idxMin,_,_) -> (idxMin + 16)

(*
 Flight Deck colors:
 Blue: #0879c9
 Orange: #dfad0b
 Teal: #4db6ac 
 Red: #e74c3c
*)

[<RequireQualifiedAccess>]
module Hex =
    let Tab = 0x09uy
    let Esc = 0x1Buy
    let LSqBracket = (strToBytes "[").[0]

[<RequireQualifiedAccess>]
module enc =
    let code c = Array.concat [| [| Hex.Esc; Hex.LSqBracket |]; strToBytes c |]

[<RequireQualifiedAccessAttribute>]
module write =
    let bytes (b:byte[]) = async {
        use so = Console.OpenStandardOutput ()
        do! so.AsyncWrite b
    }    

    let byte (b:byte) = bytes [| b |]

    let code c = bytes <| Array.concat [| [| Hex.Esc; Hex.LSqBracket |]; strToBytes c |]
            
    let str s = bytes (strToBytes s) 

    let newLine = bytes (strToBytes "\n")
    

module Components =

    type attrTags = {
        openTag: byte array option
        closeTag:  byte array option
    }

    type Comp = attrTags array -> byte array list -> byte array

    let text attrs (contents:string) = 
        let openings = attrs |> List.choose (fun a -> a.openTag) 
        let closings = attrs |> List.choose (fun a -> a.closeTag) |> List.rev
        Array.concat [|
            (openings |> Array.concat)
            (strToBytes contents)
            (closings |> Array.concat) 
        |] 

    let fgXTerm n = {
        openTag = Some (enc.code (sprintf "38;5;%im" n))
        closeTag = Some (enc.code "0m")
    }

    let fg r g b = fgXTerm (Color.convToXTerm r g b)

    let bgXTerm n = {
        openTag = Some (enc.code (sprintf "48;5;%im" n))
        closeTag = Some (enc.code "0m")
    }

    let bg r g b = bgXTerm (Color.convToXTerm r g b)

    let bold = {
        openTag = Some (enc.code "1m")
        closeTag = Some (enc.code "0m")
    }

    let clrLine = enc.code "2K"
    let clrLineToStart = enc.code "1K"
    let clrLineToEnd = enc.code "0K"

    let clrScreen = enc.code "2J"
    let clrScreenToStart = enc.code "1J"
    let clrScreenToEnd = enc.code "0J"

    /// unformatted space
    let space = strToBytes " "

    let cr = strToBytes "\n"

    let moveUp    count = enc.code (sprintf "%iA" count)
    let moveDown  count = enc.code (sprintf "%iB" count)
    let moveRight count = enc.code (sprintf "%iC" count)
    let moveLeft  count = enc.code (sprintf "%iD" count)

    let uline = {
        openTag = Some (enc.code (sprintf "4m"))
        closeTag = Some (enc.code "0m")
    }

    let div (contents:byte [] list) =
        Array.concat [|
           (contents |> Array.ofList)
        |] |> Array.concat 

    let render (comps: byte []) = async {
        do! write.bytes comps
    }

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
open System
open System.Threading

let loading () = async {
    do! render (text [(fg 225 225 30)] "Loading...\n")
    let rec loop pct = async {
        let width = (pct + 1) / 4
        let content =
            div
                [
                    moveLeft 1000
                    text [(fg 200 0 0)] "["
                    text [(bg 255 80 20)] (new String(' ', width))
                    text [] (new String(' ', 25 - width))
                    text [(fg 200 0 0)] "]"
                ]
        do! render content
        do! Async.Sleep 5
        if pct < 100 then
            do! loop (pct + 1)
    }    
    do! loop 0
    do! render cr
}

let cmdLine () = async {
    let prompt = 
        div
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
            div
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
        div
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
            do! render (div [cr; cr; echoPrompt; text [(fg 180 180 20)] s; cr; cr])
            sb.Clear() |> ignore 
            do! echoLoop()
        | Some _ ->
            do! render cr
            do! echoLoop()
        | None ->
            ()
    }
    do! echoLoop ()
    do! render (div [cr; text [] "Done..."; cr])
}

let prog () = async {   

    let option n s =
        div
            [
                text [(fg 0xff 0xb9 0x31); bold] " => "
                text [(fg 0 0x95 0xff); uline] "Option"; space
                text [(bg 0x80 0x20 0x50); bold; (fg 0xff 0xff 0)] (sprintf "%i:" n); space 
                text [(fg 0x5f 0xba 0x7d); (bg 25 15 85)] (sprintf "%s" s); cr        
            ] 

    let content =
        div  
            [
                option 1 "Standalone monolyth"
                option 2 "Web Farm deployed locally"
                option 3 "Microservices in Nomad Cluster"            
            ] 
    do! render content
    do! loading()
    do! cmdLine()
    do! render cr
}

[<EntryPoint>]
let main argv =
    prog () |> Async.RunSynchronously
    0
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

    let StrComp attrs (contents:string) = 
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

    let writeComps (comps: byte []) = async {
        do! write.bytes comps
    }

open Components

let loading () = async {
    do! writeComps (StrComp [(fg 225 225 30)] "Loading...\n")
    let rec loop pct = async {
        let width = (pct + 1) / 4
        let content =
            div
                [
                    moveLeft 1000
                    StrComp [(fg 200 0 0)] "["
                    StrComp [(bg 255 80 20)] (new String(' ', width))
                    StrComp [] (new String(' ', 25 - width))
                    StrComp [(fg 200 0 0)] "]"
                ]
        do! writeComps content
        do! Async.Sleep 5
        if pct < 100 then
            do! loop (pct + 1)
    }    
    do! loop 0
    do! writeComps cr
}

let cmdLine () = async {
    use inStream = Console.OpenStandardInput()
    let rec loop () = async {
        let ki = Console.ReadKey true
        if ki.Key <> ConsoleKey.Escape then
            let keyCode = int ki.KeyChar
            if 32 <= keyCode && keyCode <= 126 then
                let content = 
                    div
                        [
                            StrComp [] (sprintf "%c" ki.KeyChar)
                        ]
                do! writeComps content
            else
                ()
            do! loop ()
    }
    do! loop ()
    do! writeComps (div [cr; StrComp [] "Done..."; cr])
}

let prog () = async {   

    let option n s =
        div
            [
                StrComp [(fg 0xff 0xb9 0x31); bold] " => "
                StrComp [(fg 0 0x95 0xff); uline] "Option"; space
                StrComp [(bg 0x80 0x20 0x50); bold; (fg 0xff 0xff 0)] (sprintf "%i:" n); space 
                StrComp [(fg 0x5f 0xba 0x7d); (bg 25 15 85)] (sprintf "%s" s); cr        
            ] 

    let content =
        div  
            [
                option 1 "Standalone monolyth"
                option 2 "Web Farm deployed locally"
                option 3 "Microservices in Nomad Cluster"            
            ] 
    do! writeComps content
    do! loading()
    do! cmdLine()
    do! writeComps cr
}

[<EntryPoint>]
let main argv =
    prog () |> Async.RunSynchronously
    0
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
        closeTag = None // Some (enc.code "0m")
    }

    let fg r g b = fgXTerm (Color.convToXTerm r g b)

    let bgXTerm n = {
        openTag = Some (enc.code (sprintf "48;5;%im" n))
        closeTag = Some (enc.code "0m")
    }

    let bg r g b = bgXTerm (Color.convToXTerm r g b)

    let div attrs (contents:byte [] list) =
        let openings = attrs |> Array.ofList |> Array.choose (fun a -> a.openTag) 
        let closings = attrs |> Array.ofList |> Array.choose (fun a -> a.closeTag) |> Array.rev        
        Array.concat [|
            openings; (contents |> Array.ofList); closings
        |] |> Array.concat

    let writeComps (comps: byte []) = async {
        do! write.bytes comps
    }

open Components

let prog () = async {   

    let fg1 = Color.convToXTerm 0x00 0x95 0xff
    
    let option n s =
        div []
            [
                StrComp [(fg 0xff 0xb9 0x31)] " => "
                StrComp [(fg 0 0x95 0xff)] "Option "
                StrComp [(fg 0xff 0 0)] (sprintf "%i: " n) 
                StrComp [(fg 0x5f 0xba 0x7d)] (sprintf "%s\n" s)        
            ] 

    let content =
        div [] 
            [
                option 1 "Find him and talk to him"
                option 2 "Threaten family"
                option 3 "Assasinate from a distance"            
            ] 
    do! writeComps content
}

[<EntryPoint>]
let main argv =
    prog () |> Async.RunSynchronously
    0
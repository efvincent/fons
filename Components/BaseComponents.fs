namespace Fons

module Components =

    open LowLevel

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

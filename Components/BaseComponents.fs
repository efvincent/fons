namespace Fons

module Components =

    open LowLevel

    /// Style stack is used to support nested styles. Example: you have a div that indicates white on blue.
    /// Within that is a plain text element, an element that implements yellow foreground, and another
    /// plain element. What should render is the first element as white on blue, the second as yellow on blue
    /// (the blue is from the div), and the third element is white on blue again. The behavior is that when
    /// an element's scope is complete, the style reverts to what it was before the element. And this effect
    /// can be nested. Terminal by itself cannot do this.
    let private styleStack = new System.Collections.Generic.Stack<byte[]>()

    (*
        CONTAINERS / CURRENT STYLE
    *)

    /// Accepts a list of byte arrays and concats into a single byte
    /// array. Use it like you'd use a div in react, to group many
    /// other controls into a single control
    let block (contents:byte [] list) =
        Array.concat [|
           (contents |> Array.ofList)
        |] |> Array.concat 

    /// Push new styles onto the style stack. Anything after this point will default to the
    /// new style. Old style is saved, and can be returned to with a popStyle command
    let pushStyle attrs =
        let styles = attrs |> Array.concat
        if styles.Length > 0 then styleStack.Push styles
        styles

    /// calculates the combined previous style, which is a clear command, then a re-application of the stacked
    /// up styles, then pops off the last style, and applies the calculated old style. If there's nothing
    /// in the stack, it simply resets the current style
    let popStyle () =
        if styleStack.Count > 0 then
            styleStack.Pop() |> ignore
            [| (enc.code "0m"); styleStack.ToArray() |> Array.concat |] |> Array.concat
        else enc.code "0m"

    /// A div is like a block with attributes
    let div attrs (contents:byte [] list) =
        let hasStyle = attrs |> Seq.length > 0
        // This must be done as a sequential separate steps because the style stack is a side
        // effect, and is mutated on pre and post
        let pre = if hasStyle then pushStyle attrs else Array.empty
        let content = contents |> Array.ofList |> Array.concat
        let post  = if hasStyle then popStyle () else Array.empty
        [|pre; content; post|] |> Array.concat
    (*
        MOTION
    *)

    let moveUp    count = enc.code (sprintf "%iA" count)
    let moveDown  count = enc.code (sprintf "%iB" count)
    let moveRight count = enc.code (sprintf "%iC" count)
    let moveLeft  count = enc.code (sprintf "%iD" count)

    /// Move cursor to the upper left hand corner of the screen (0,0)
    let home = enc.code "H"
    /// Move the cursor to specified position (line, column) zero based
    let moveTo line col = enc.code (sprintf "%i;%iH" line col)
    /// Move to the next line
    let moveNextLine = enc.code "E"
    /// Scroll window up one line
    let scrollUp = enc.code "D"
    /// Scroll window down one line
    let scrollDown = enc.code "M"

    (*
        CLEARING / ALT SCREEN / SAVE CURSOR ATTRIBUTES
    *)
    
    /// Clear the entire line. Does not move the cursor
    let clrLine = enc.code "2K"
    /// Clear from current cursor postioin to the start of the line. Does not move the cursor
    let clrLineToStart = enc.code "1K"
    /// Clear from current cursor position to the end of the line. Does not move the cursor
    let clrLineToEnd = enc.code "0K"

    /// Clear the entire screen. Does not move the cursor
    let clrScreen = enc.code "2J"
    /// Clear from current cursor position to the start of the screen. Does not move the cursor
    let clrScreenToStart = enc.code "1J"
    /// Clear from current cursor position to the end of the screen. Does not move the cursor
    let clrScreenToEnd = enc.code "0J"

    /// switch to alt screen
    let setAlt = enc.code "?47h"
    /// switch to normal screen
    let setNoAlt = enc.code "?47l"

    /// Save cursor attributes
    let saveCursor = enc.code "s"
    /// Restor cursor attributes
    let restoreCursor = enc.code "u"

    /// Get the current cursor position. Look for current position in the
    /// input stream as EscLine;Column;Row
    let getCursorPos = enc.code "6n" 

    (*
        ATTRIBUTES
    *)

    /// Foreground color as XTerm code number
    let fgXTerm n = (enc.code (sprintf "38;5;%im" n))

    /// Foreground as RGB number, 0-255
    /// For hex, prefix with 0x, as in 0x6F
    let fg r g b = fgXTerm (Color.convToXTerm r g b)

    /// Set background color as XTerm code number
    let bgXTerm n = enc.code (sprintf "48;5;%im" n)

    /// Set background color as RGB number, 0-255
    let bg r g b = bgXTerm (Color.convToXTerm r g b)

    /// Turn on bold characters
    let bold = enc.code "1m"

    /// Turn on underlining
    let uline = enc.code (sprintf "4m")
    
    /// Turn off highlighting
    let clear = enc.code "0m"

    (*
        WIDGETS
    *)

    /// Write the text at the current cursor position with the given attributes
    let text (attrs:seq<byte []>) (contents:string) = 
        let hasStyle = attrs |> Seq.length > 0
        let pre = if hasStyle then pushStyle attrs else Array.empty
        let content = strToBytes contents 
        let post = if hasStyle then popStyle () else Array.empty
        Array.concat [| pre; content; post |]

    /// unformatted space
    let space = strToBytes " "

    let cr = strToBytes "\n"

    (*
        RENDERING
    *)

    /// Renders a byte array
    let render (comps: byte []) = async {
        do! write.bytes comps
    }

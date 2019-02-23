namespace Fons
open System
open System.Reflection.Metadata.Ecma335

module OutCommands = 

    open LowLevel

    type ColorValue =
    | RGB of R:int * G:int * B:int
    | XTerm256 of int
    with 
        member x.ToCode = 
            match x with
            | RGB(r,g,b) -> Color.convToXTerm r g b
            | XTerm256 n -> n

    type StyleSetting =
    | Fg of ColorValue
    | Bg of ColorValue
    | Bold 
    | Underline 
    with 
        static member ToBytes x = 
            match x with
            | Fg cv -> enc.code (sprintf "38;5;%im" cv.ToCode)
            | Bg cv -> enc.code (sprintf "48;5;%im" cv.ToCode)
            | Bold -> enc.code "1m"
            | Underline -> enc.code "4m"

    type MovementCmd = 
    | PageHome  // upper left of page
    | PageEnd   // lower right of page
    | Home      // start of line
    | End       // end of line
    | Left of int       // Move left n postitions
    | Right of int      // Move right n positions
    | Up of int         // Move up n positions
    | Down of int       // Move down n positions


    [<RequireQualifiedAccess>]
    type ClearCmd =
    | ToStartOfLine
    | ToEndOfLine
    | Line
    | ToStartOfScreen
    | ToEndOfScreen
    | Screen

    [<RequireQualifiedAccess>]
    type RenderCmd =
    | Style of StyleSetting list
    | Clear of ClearCmd
    | Move of MovementCmd
    | Text of string
    | Container of StyleSetting list * RenderCmd list
    | CR 


module Internal =
    open OutCommands
    open LowLevel

    type RenderState = {
        StyleStack: StyleSetting list list
        /// TODO:
        /// * cursor location
        /// * screen size
        /// * FUTURE: content state... Use for retained mode. Each position needs all information, style, and content
        ///   typically there'd be some optimization to provide for large areas (blocks) of characters
    }
    with
        static member init = {
            StyleStack = []
        }


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

open Internal
open System.Diagnostics
open LowLevel
open LowLevel
open System.Threading
open System.Threading
open LowLevel

module Rendering =

    open LowLevel
    open OutCommands
    open Internal
    
    let private cvTo256 = function
    | RGB(r,g,b) -> Color.convToXTerm r g b
    | XTerm256 n -> n

    let writeStyles (styles:StyleSetting list) =
        styles
        |> List.map (StyleSetting.ToBytes)
        |> Array.concat
        |> write.buffer

    let render (initialState: RenderState) (commands: RenderCmd list) : RenderState =

        let rec renderStep state cmd =
            match cmd with
            | RenderCmd.Clear clr -> state
            | RenderCmd.Style styles ->
                // we need to merge current styles with previous styles, listing the previous
                // styles first. Then apply them and pushed the merged styles. An optimization
                // would be to clean duplicates after merging. Also, there's a problem with 
                // turning off bolds/uline with this approach
                let combined = 
                    match state.StyleStack with
                    | [] -> styles
                    | prevHead::_ ->
                        List.append prevHead styles 
                writeStyles combined
                { state with StyleStack = combined::state.StyleStack }
            | RenderCmd.Move move -> state
            | RenderCmd.Container (styles, content) ->
                let weHadStyles = styles |> List.length > 0
                let stateAfterStyles = 
                    if weHadStyles then
                        renderStep state (RenderCmd.Style styles)
                    else state
                let stateAfterContent = content |> List.fold(renderStep) stateAfterStyles 
                // after rendering, check to see if we should restore a style
                match stateAfterContent.StyleStack with
                | _::prev::rest when weHadStyles ->
                    // there's a style to restore, and we also applied a style. So restore
                    // the style to restore, and we can pop ours off the stack. Restoring
                    // means clearing the current style and applying the new style.
                    write.buffer clear
                    if prev <> [] then writeStyles prev
                    { stateAfterContent with StyleStack = prev::rest }
                | _::[] when weHadStyles -> 
                    // there was nothing before ours. We can return empty and restore 
                    // a blanks style
                    write.buffer clear
                    { stateAfterContent with StyleStack = [] }
                | _ ->
                    // In any other case, we can just return the new state. We haven't modified
                    // the style stack so it should stay as is
                    stateAfterContent
            | RenderCmd.Text text ->
                write.buffer (strToBytes text)
                state
            | RenderCmd.CR -> 
                write.buffer (strToBytes "\n")
                state
            
        let newState = commands |> List.fold renderStep initialState
        newState 
    
module Components =

    open OutCommands

    let initialRenderState = Internal.RenderState.init
    let render = Rendering.render
    
    let fg r g b = Fg(RGB(r,g,b))
    let bg r g b = Bg(RGB(r,g,b))

    let bold = Bold 
    let uline = Underline 
    
    let text styles content =
        RenderCmd.Container(styles, [RenderCmd.Text content])
        
    let textLn styles content = 
        text styles (content + "\n")

    let write content = text [] content

    let writeLn content = textLn [] content

    let br = textLn [] ""

    let block content = RenderCmd.Container([], content)

    let div styles content = RenderCmd.Container(styles, content)

    (*
        MOTION
    *)

(*     let moveUp    count = enc.code (sprintf "%iA" count)
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
    let scrollDown = enc.code "M" *)

    (*
        CLEARING / ALT SCREEN / SAVE CURSOR ATTRIBUTES
    *)
    
(*     /// Clear the entire line. Does not move the cursor
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
    let getCursorPos = enc.code "6n"  *)

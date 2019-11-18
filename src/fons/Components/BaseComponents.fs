namespace Fons

module OutCommands = 

    open LowLevel

    /// Represents a character color
    type ColorValue =
    | RGB of R:int * G:int * B:int
    | XTerm256 of int
    with 
        member x.ToCode = 
            match x with
            | RGB(r,g,b) -> Color.convToXTerm r g b
            | XTerm256 n -> n

    /// Commands that change the color, bold, or underline of the text
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

    /// Commands that move the cursor
    type MovementCmd = 
    | PageHome          // upper left of page -     [H
    | NextLine          // move to the next line -  [E
    | Pos of row:int * col:int // move to abs position row height - [line;colH 
    | ScrollUp          // Scroll window up -       D
    | ScrollDown        // Scroll window down -     M
    | Up of int         // Move up n positions -    [A
    | Down of int       // Move down n positions  - [B
    | Right of int      // Move right n positions - [C
    | Left of int       // Move left n postitions - [D

    /// Commands that delete characters
    type ClearCmd =
    | ToStartOfLine
    | ToEndOfLine
    | Line
    | ToStartOfScreen
    | ToEndOfScreen
    | Screen

    /// Terminal level commands
    type TerminalCmd =
    | SwitchToAltBuffer
    | SwitchToMainBuffer
    | SaveExcursion
    | RestoreExcursion
    
    [<RequireQualifiedAccess>]
    /// The differnet types of render command that can be composed together and
    /// sent to the renderer.
    type RenderCmd =
    | Term of TerminalCmd
    | Style of StyleSetting list
    | Clear of ClearCmd
    | Move of MovementCmd
    | Text of string
    | Container of StyleSetting list * RenderCmd list
    | CR 


module Internal =
    open OutCommands

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

module Rendering =

    open LowLevel
    open Internal
    open OutCommands
    
    let private cvTo256 = function
    | RGB(r,g,b) -> Color.convToXTerm r g b
    | XTerm256 n -> n

    let writeStyles (styles:StyleSetting list) =
        styles
        |> List.map (StyleSetting.ToBytes)
        |> Array.concat
        |> write.buffer

    let render (commands: RenderCmd list) (initialState: RenderState) : RenderState =

        let rec renderStep state cmd =
            match cmd with
            | RenderCmd.Term SwitchToAltBuffer ->
                write.buffer <| enc.code "?47h"
                state
            | RenderCmd.Term SwitchToMainBuffer ->
                write.buffer <| enc.code "?47l"
                state
            | RenderCmd.Term SaveExcursion ->
                write.buffer <| enc.codeNoBracket "7" 
                state
            | RenderCmd.Term RestoreExcursion ->
                write.buffer <| enc.codeNoBracket "8"
                state

            | RenderCmd.Clear ToStartOfLine ->
                write.buffer <| enc.code "1K"
                state
            | RenderCmd.Clear ToEndOfLine ->
                write.buffer <| enc.code "0K"
                state
            | RenderCmd.Clear Line ->
                write.buffer <| enc.code "2K"
                state
            | RenderCmd.Clear ToStartOfScreen ->
                write.buffer <| enc.code "1J"
                state
            | RenderCmd.Clear ToEndOfScreen ->
                write.buffer <| enc.code "0J"
                state
            | RenderCmd.Clear Screen ->
                write.buffer <| enc.code "2J"
                state

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

            | RenderCmd.Move (Up n) ->
                write.buffer <| enc.code (sprintf "%iA" n)
                state
            | RenderCmd.Move (Down n) ->
                write.buffer <| enc.code (sprintf "%iB" n)
                state
            | RenderCmd.Move (Right n) ->
                write.buffer <| enc.code (sprintf "%iC" n)
                state
            | RenderCmd.Move (Left n) ->
                write.buffer <| enc.code (sprintf "%iD" n)
                state
            | RenderCmd.Move NextLine ->
                write.buffer <| enc.code "E"
                state
            | RenderCmd.Move PageHome ->
                write.buffer <| enc.code "H"
                state
            | RenderCmd.Move (Pos(row,col)) ->
                write.buffer <| enc.code (sprintf "%i;%iH" row col)
                state
            | RenderCmd.Move ScrollUp ->
                write.buffer <| enc.codeNoBracket "D"
                state
            | RenderCmd.Move ScrollDown ->
                write.buffer <| enc.codeNoBracket "M"
                state

            | RenderCmd.Container (styles, content) ->
                let weHadStyles = not (List.isEmpty styles)
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
                    write.buffer <| enc.code "0m"
                    if prev <> [] then writeStyles prev
                    { stateAfterContent with StyleStack = prev::rest }
                | [_] when weHadStyles -> 
                    // there was nothing before ours. We can return empty and restore 
                    // a blanks style
                    write.buffer <| enc.code "0m"
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
    
    let text styles content =RenderCmd.Container(styles, [RenderCmd.Text content])
        
    let textln styles content = 
        text styles (content + "\n")

    let write content = text [] content

    let writeln content = textln [] content

    let br = writeln ""

    let space = write " "

    let block content = RenderCmd.Container([], content)

    let div styles content = RenderCmd.Container(styles, content)

    (*
        MOTION
    *)
    let home = RenderCmd.Move PageHome

    let nextLine = RenderCmd.Move NextLine

    let pos row col = RenderCmd.Move <| Pos(row,col)

    let scrollUp = RenderCmd.Move ScrollUp

    let scrollDown = RenderCmd.Move ScrollDown

    let up n = RenderCmd.Move<| Up n

    let down n =  RenderCmd.Move<| Down n

    let left n = RenderCmd.Move <| Left n

    let right n = RenderCmd.Move <| Right n
    (*
        CLEARING / ALT SCREEN / SAVE CURSOR ATTRIBUTES
    *)

    let clrLine = RenderCmd.Clear Line
   
    let clrLineToStart = RenderCmd.Clear ToStartOfLine

    let clrLineToEnd = RenderCmd.Clear ToEndOfLine

    let clrScreen = RenderCmd.Clear Screen

    let clrScreenToEnd = RenderCmd.Clear ToEndOfScreen

    let clrScreenToStart = RenderCmd.Clear ToStartOfScreen

    let switchToAlt = RenderCmd.Term SwitchToAltBuffer

    let switchToMain = RenderCmd.Term SwitchToMainBuffer

    let saveExcursion = RenderCmd.Term SaveExcursion

    let restoreExcursion = RenderCmd.Term RestoreExcursion

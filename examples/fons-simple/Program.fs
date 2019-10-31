open Fons
open Components
open Fons.cli
open System

let prog () =    
    let many count cmd =
        let rec loop acc n =
            if n < count then 
                loop (cmd::acc) (n+1)
            else
                acc
        block (loop [] 0)

    let getCmdLinePrompt () =
        let curTime = DateTime.Now
        block
            [
                space
                text [fg 0 0 255] "FlightDeck"
                text [fg 220 220 20] "["
                text [fg 250 100 100] (sprintf "%02i:%02i:%02i" curTime.Hour curTime.Minute curTime.Second)
                text [fg 220 220 20] "]"
                space
                text [fg 100 100 100] "$"
                space
            ]

    let syntaxHighlighter (s:string) =
        let intersperse sep ls =
            List.foldBack (fun x -> function
                | [] -> [x]
                | xs -> x::sep::xs) ls []
        // hrmm... there must be some established patters which I'll look for later rather than 
        // reinventing the wheel. For now, I'll break it up by spaces and assemble them back together as 
        // a block, highlighting each block as needed as an initial test. This breaks if the person types > 1
        // space inbetween words (I know ... it's temporary)
        let keywords = ["exit"; "cat"; "ll"; "ls"; "token"; "script"; "export"; "send"; "help"] |> Set.ofList
        let parts = 
            s.Split(' ')
            |> Array.map (fun word ->
                if keywords |> Set.contains (word.ToLower()) 
                then text [fg 0x80 0x80 0xff; bold] (word.ToLower())
                elif word.StartsWith ("--") then 
                    block [ text [fg 0x100 0x255 0x200] "--"; text [fg 0xdf 0xab 0x0b] (word.Substring(2))]
                else write word)
        block (parts |> List.ofArray |> intersperse space)

    let cmdProcessor (input:string) renderState =
        let echoPrompt =
            block
                [
                    space
                    text [fg 0x7a 0x6f 0xfa; bold] "echo"
                    space
                    text [fg 0xf0 0xff 0x20; bold] ">>>"
                    space
                ]

        if (input.ToLower() = "exit") then
            {
                UpdatedState = renderState
                ExitCLI = true
            }
        else
            {
                UpdatedState = render [block [br; br; echoPrompt; text [fg 180 180 20] input; br; br]] renderState
                ExitCLI = false
            }

    render [switchToAlt; clrScreen; pos 3 1] initialRenderState
    |> cmdLine getCmdLinePrompt cmdProcessor (Some syntaxHighlighter)
    |> render [switchToMain]

[<EntryPoint>]
let main argv =
    prog () |> ignore
    0

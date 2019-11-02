namespace Fons

module Interact =
    open Components
    open cli
    
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
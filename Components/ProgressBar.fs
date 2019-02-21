namespace Fons

module ProgressBar =
    open System
    open Components 

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
namespace Fons

module Examples =
    open Components
    open System

    let styleTest = 
        [
            write "plain text "
            div [bg 255 255 0; fg 0 0 0] [ 
                write "black on yellow " 
                text [fg 50 50 255; bold] "\nblue and bold on yellow"
                div [uline] [
                    textln [bg 255 0 255] "\nblue and bold on purple underlined"
                    writeln "blue, bold on yellow, uline"
                ]
                text [] " black on yellow "
            ]
            text [fg 255 120 120] "red on black "
            write "plain text"
            br
        ]     

    let t1 =
        [
            pos 1 1
            write "HOME"; left 4
            right 20
            write "20"; left 2
            down 20
            write "20"; left 2
            left 20 
            write "20"; left 2
            pos 1 35
            writeln "Done..."
        ]

    let t2 = 
        [
            write "Movement test 2"
            block ([1..20] |> List.map (fun n -> 
                block [ pos (9+n) 20; write (sprintf "%i" n)]))
            br
            writeln "Done"
        ]

    let t3 =
        let line = 
            List.concat [
                [block ([0..6] |> List.map (fun _ -> 
                    write (string "1234567890"))
                )]
                [writeln ""]
            ]
        [
            pos 0 0
            block ([1..40] |> List.map (fun _ -> block line))
            
            block [ pos 1 1; text [bg 220 220 0] " "]
            block [ pos 30 1; text [bg 220 220 0] " "]
            block [ pos 1 30; text [bg 220 220 0] " "]
            block [ pos 30 30; text [bg 220 220 0] " "]

            block [ pos 2 2; text [bg 0 220 220] " "; left 1]
            block [ right 29; text [bg 0 220 220] " "; left 1]
            block [ down 29; text [bg 0 220 220] " "; left 1]
            block [ left 29; text [bg 0 220 220] " "; left 1]

            pos 41 1
            writeln "Done..."
        ]

    let box styles fillStyles r c h w =
        let fillLine r' = block [pos (r'+r) (c+1); write (new String(' ', (w-2)))]

        div styles [
            // top & bottom of the box
            block (
                [r; r+h] |> List.map (fun n -> (block [pos n c; write (new String(' ', w))]))
            )

            // the two sides
            block (
                [1..h] |> List.map (fun n -> (block [ pos (r+n) c; write " "; pos (r+n) (c+w-1); write " "]))
            )

            div fillStyles (
                [1..h-1] |> List.map (fun n -> (block [pos (r+n) c; (fillLine n)]))
            )
        ]

    let boxes = 
        let sW = Console.WindowWidth
        let sH = Console.WindowHeight
        [
            box [bg 80 80 0] [bg 0 50 50] 1 1 (sH-2) sW   
            box [bg 120 120 120] [bg 0 80 80] 10 20 10 50
            box [bg 220 220 220] [bg 80 0 80] 15 25 15 60
            pos sH 1
        ]
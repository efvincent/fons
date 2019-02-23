namespace Fons

module LowLevel =

    open System

    let strToBytes (s:string) =
        System.Text.Encoding.UTF8.GetBytes s

    [<RequireQualifiedAccess>]
    module Hex =
        let Tab = 0x09uy
        let Esc = 0x1Buy
        let LSqBracket = (strToBytes "[").[0]

    [<RequireQualifiedAccess>]
    module enc =
        let code c = Array.concat [| [| Hex.Esc; Hex.LSqBracket |]; strToBytes c |]
        let codeNoBracket c = Array.concat [| [| Hex.Esc; |]; strToBytes c |]

    [<RequireQualifiedAccessAttribute>]
    module write =
        
        let buffer (b:byte[]) =
            use so = Console.OpenStandardOutput()
            so.Write(b,0,b.Length)

        let byte (b:byte) = buffer [| b |]

        let code c = buffer <| Array.concat [| [| Hex.Esc; Hex.LSqBracket |]; strToBytes c |]
                
        let str s = buffer (strToBytes s) 

        let newLine = buffer (strToBytes "\n")
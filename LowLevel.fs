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
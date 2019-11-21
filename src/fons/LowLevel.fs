namespace Fons

/// Modules for writing to standard out, converting strings to byte arrays, and commonly
/// used characters in  building escape sequences
module LowLevel =

    open System

    /// Converts a string to a byte array, uses UTF8 encoding
    let strToBytes (s:string) =
        System.Text.Encoding.UTF8.GetBytes s

    [<RequireQualifiedAccess>]
    /// Hex codes for commonly used characters in terminal control: the
    /// tab, esc, and left square bracket characters
    module Hex =
        let Tab = 0x09uy
        let Esc = 0x1Buy
        let LSqBracket = (strToBytes "[").[0]

    [<RequireQualifiedAccess>]
    module enc =
        let code c = Array.concat [| [| Hex.Esc; Hex.LSqBracket |]; strToBytes c |]
        let codeNoBracket c = Array.concat [| [| Hex.Esc; |]; strToBytes c |]

    [<RequireQualifiedAccessAttribute>]
    /// Used to write butes out to the standard output
    module write =
        
        /// Writes the buffer (a byte array) out the standard output
        let buffer (b:byte[]) =
            use so = Console.OpenStandardOutput()
            so.Write(b,0,b.Length)

        /// writes a single byte out to the standard buffer
        let byte (b:byte) = buffer [| b |]

        /// Writes a CLI code out to the buffer by appending the hex codes for escape
        /// and left square bracket before the string.
        let code c = buffer <| Array.concat [| [| Hex.Esc; Hex.LSqBracket |]; strToBytes c |]

        /// writes a string out to the standard buffer by converting the string to bytes
        /// and passing it to the write.buffer function   
        let str s = buffer (strToBytes s) 

        /// writes a newline to the standard output
        let newLine = buffer (strToBytes "\n")
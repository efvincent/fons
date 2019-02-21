namespace Fons


[<RequireQualifiedAccess>]
module Color =
    let private d = [|0; 95; 135; 175; 215; 255|]
    let private a = 
        [|0..239|]
        |> Array.map (fun n ->
            if n < 216 then [|d.[n / 36]; d.[(n % 36) / 6]; d.[n % 6]|]
            else 
                let v = n * 10 - 2152
                [|v; v; v|])

    let convToXTerm r g b = 
      a 
      |> Array.map (Array.zip [|r;g;b|])
      |> Array.map (fun tups -> tups |> Array.map(fun (v1,v2) -> abs(v1-v2)) |> Array.reduce (+))
      |> Array.fold 
        (fun (idxMin,vMin,idxCur) vCur -> if vCur <= vMin then (idxCur,vCur,idxCur+1) else (idxMin,vMin,idxCur+1)) 
        (-1,257,0)
      |> fun (idxMin,_,_) -> (idxMin + 16)

(*
 Flight Deck colors:
 Blue: #0879c9
 Orange: #dfad0b
 Teal: #4db6ac 
 Red: #e74c3c
*)
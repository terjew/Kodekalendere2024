open Utilities
open System.IO

let checkPos pos matrix expected =
    match Matrix.isInside matrix pos with
    | false -> false
    | true -> Matrix.get matrix pos |> (fun v -> v = expected)

let checkDirection pos matrix direction expected = 
    let calculatepos = Vector.offsetWith pos direction
    let check (pos, expected) = checkPos pos matrix expected

    let mismatch = expected
                    |> Seq.map (fun (a,c) -> (calculatepos a, c))
                    |> Seq.map check 
                    |> Seq.contains false 

    match mismatch with
    | false -> 1
    | true -> 0

let checkDirectionXMAS pos matrix direction = 
    seq {(1,'M'); (2,'A'); (3,'S')} |> checkDirection pos matrix direction
   
let checkForXmas pos matrix =
    Direction.cardinalAndOrdinal |> Seq.sumBy (checkDirectionXMAS pos matrix)

let checkDirectionMAS pos matrix direction = 
    seq {(-1,'M'); (1,'S')} |> checkDirection pos matrix direction
   
let checkForMas pos matrix =
    let count = Direction.ordinal 
                |> Seq.sumBy (checkDirectionMAS pos matrix)
    if count = 2 then 1 else 0

let matrix = File.ReadLines("input.txt") |> Matrix.fromStrings 

matrix |> Matrix.findAll 'X'
|> Seq.sumBy (fun p -> checkForXmas p matrix)
|> printfn "Part 1: %A"

matrix |> Matrix.findAll 'A'
|> Seq.sumBy (fun p -> checkForMas p matrix)
|> printfn "Part 1: %A"

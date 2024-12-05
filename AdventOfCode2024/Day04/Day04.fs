open Utilities
open System.IO

let checkPos pos matrix expected =
    match Matrix.isInside matrix pos with
    | false -> false
    | true -> Matrix.get matrix pos |> (fun v -> v = expected)

let checkDirectionXMAS pos matrix direction = 
    let offset = Coordinate.offsetWith pos direction
    let check (pos, expected) = checkPos pos matrix expected

    let isXmas = seq {(1,'M'); (2,'A'); (3,'S')}
                |> Seq.map (fun (a,c) -> (offset a, c))
                |> Seq.map check 
                |> Seq.contains false 
                |> not

    if isXmas then 1 else 0
   
let checkForXmas pos matrix =
    Direction.cardinalAndOrdinal |> Seq.sumBy (checkDirectionXMAS pos matrix)

let checkDirectionMAS pos matrix direction = 
    let offset = Coordinate.offsetWith pos direction
    let check (pos, expected) = checkPos pos matrix expected

    let isXmas = seq {(-1,'M'); (1,'S')}
                |> Seq.map (fun (a,c) -> (offset a, c))
                |> Seq.map check 
                |> Seq.contains false 
                |> not

    if isXmas then 1 else 0
   
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

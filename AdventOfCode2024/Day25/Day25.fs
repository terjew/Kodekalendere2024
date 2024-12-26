open System.IO
open Utilities

let getHeights matrix =
    matrix 
    |> Matrix.columnIndices 
    |> Seq.map (Matrix.column matrix)
    |> Seq.map (fun column -> (column |> Seq.filter ((=) '#')) |> Seq.length)
    |> Seq.map (fun l -> l - 1)
    |> List.ofSeq

type Entry =
    | Lock of (Matrix * int list)
    | Key of (Matrix * int list)
        
    static member create matrix =
        match (Matrix.row matrix 0) with
        | "#####" -> Lock (matrix, getHeights matrix)
        | "....." -> Key (matrix, getHeights matrix)
        | any -> failwithf "Unkwown entry starting with %s" any

let parseEntry lines =
    lines 
    |> Matrix.fromStrings
    |> Entry.create

let parseFile filename = 
    File.ReadAllLines filename
    |> Seq.toList
    |> SequenceHelper.splitByValue ""
    |> Seq.map parseEntry

let fits (_,key) (_,lock) =
    key 
    |> List.zip lock 
    |> List.forall (fun (k,l) -> (k+l) <= 5)

let entries = parseFile "input.txt"
let keys = entries |> Seq.choose (function |Key k -> Some k | _ -> None) |> Seq.toList
let locks = entries |> Seq.choose (function |Lock k -> Some k | _ -> None) |> Seq.toList

let checkForFit keys locks =
    seq [
        for key in keys do
            for lock in locks do
                yield fits key lock
    ]

checkForFit keys locks |> Seq.filter id |> Seq.length |> printfn "Part 1: %d"
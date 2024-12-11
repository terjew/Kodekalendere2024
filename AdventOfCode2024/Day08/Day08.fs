open System.IO
open Utilities

let getAntiNodes min max (a,b) =
    let diff = Vector.subtract a b 
    [min..max]
    |> Seq.map (Vector.mul diff)
    |> Seq.map (Vector.add a)

let findAntinodes map min max frequency =
    map 
    |> Matrix.findAll frequency
    |> List.ofSeq
    |> SequenceHelper.getPerms 2
    |> Seq.map (fun list -> (Seq.head list,Seq.last list))
    |> Seq.collect (getAntiNodes min max)
    |> Seq.filter (Matrix.isInside map)

let getFrequencies map =
    map
    |> Matrix.allPos
    |> Seq.map (Matrix.get map)
    |> Seq.except ['.']
    |> Seq.distinct

let distinctAntinodes min max map =
    map 
    |> getFrequencies
    |> Seq.map (findAntinodes map min max)
    |> Seq.collect id
    |> Seq.distinct

let map = File.ReadLines("input.txt") |> Matrix.fromStrings        

map |> distinctAntinodes 1 1 |> Seq.length |> printfn "Part 1: %A"
map |> distinctAntinodes 0 (Matrix.maxDim map) |> Seq.length |> printfn "Part 2: %A"

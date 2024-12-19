open System.IO
open Utilities

let parse filename =
    File.ReadAllLines filename
    |> Seq.map (Regex.combineGroups "(\d+),(\d+)" ValueCollection.asIntVector2)
    
let corrupt map pos =
    map |> Matrix.withValueAt pos '#'

let solveAStar start goal map =
    let h pos next = 
        (float (Vector.manhattanDistance pos next))
    let g _ _ = 
        1.0
    let neighbors pos = 
        Matrix.neighborsWithValues map pos
        |> Seq.filter (fun kvp -> '#' <> kvp.Value)
        |> Seq.map (fun kvp -> kvp.Key)

    AStar.search start goal {
        fCost = h
        gCost = g
        neighbours = neighbors
        maxIterations = None
    }

let part1 size count filename =
    let map = Matrix.create size size '.'
    parse filename
    |> Seq.take count
    |> Seq.fold corrupt map
    |> solveAStar (0,0) (size - 1,size - 1)
    |> Option.defaultValue Seq.empty
    |> Seq.skip 1
    |> Seq.length 

let isSolvable map =
    match solveAStar (0,0) (map.SizeX - 1, map.SizeY - 1) map with
    | Some _ -> true
    | None -> false

let isSolvableAfter map coords i =
    coords 
    |> List.take i
    |> Seq.fold corrupt map
    |> isSolvable

let bisect isValidFunc (arr: 'a array) =
    let rec recurse lastValid firstInvalid =
        if lastValid > firstInvalid then
            None
        else if lastValid = firstInvalid - 1 then
            Some lastValid
        else
            let midPoint = (lastValid + firstInvalid) / 2
            let valid = isValidFunc arr[midPoint]

            if not valid then
                recurse lastValid midPoint
            else 
                recurse midPoint firstInvalid

    recurse 0 (Array.length arr - 1)

let part2 size filename = 
    let map = Matrix.create size size '.'
    let coords = parse filename |> Seq.toList
    let f = isSolvableAfter map coords
    let lastKnown = bisect f [|0..coords.Length|]
    coords[lastKnown.Value]

let stopWatch = System.Diagnostics.Stopwatch.StartNew()
part1 71 1024 "input.txt" |> printfn "Part 1: %A"
part2 71 "input.txt" |> printfn "Part 2: %A"
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
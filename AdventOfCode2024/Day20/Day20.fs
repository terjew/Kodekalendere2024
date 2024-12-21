open Utilities

let map = Matrix.fromFile "input.txt"
let start = map |> Matrix.find 'S'
let goal = map |> Matrix.find 'E'
let solution = map |> Matrix.solveAStar start goal |> Option.get |> Seq.toList

map |> (Matrix.printColored Matrix.defaultColormap) |> ignore

let getCheatsFromPosition map solution distance minValue position =
    let startIndex = List.findIndex ((=) position) solution
    position 
    |> Matrix.allWithinManhattanDistance map distance
    |> Seq.filter (fun pos -> List.contains pos solution)
    |> Seq.map (fun pos -> (pos, List.findIndex ((=) pos) solution))
    |> Seq.map (fun (pos,index) -> index - startIndex - (Vector.manhattanDistance pos position))
    |> Seq.filter (fun value -> value >= minValue)

let getCheats map (solution:(int*int) list) distance minValue = 
    solution 
    |> Seq.map (getCheatsFromPosition map solution distance minValue)
    |> Seq.sumBy Seq.length
        
getCheats map solution 2 100 |> printfn "Part 1: %A"
getCheats map solution 20 100 |> printfn "Part 2: %A"

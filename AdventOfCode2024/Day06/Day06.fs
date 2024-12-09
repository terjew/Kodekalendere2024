open System.IO
open Utilities
open FSharp.Collections.ParallelSeq

type State = 
    {
        Position : int * int
        Heading : Direction
        Map : Matrix
    }

type SearchStatus =
    | Running of State
    | BoardExited of State
    | LoopFound

let northChar = Direction.directionsToChar [Direction.North]

let createState pos heading map =
    {
        Position = pos;
        Heading = heading;
        Map = map;
    }

let findStart matrix =
    matrix |> Matrix.find northChar

let rotate state =
    let newHeading = Direction.next state.Heading 2
    { state with
        Heading = newHeading;
    }    

let move state newPos =
    { state with 
        Position = newPos
    }

let writePosition state =
    let cellHistory = Matrix.get state.Map state.Position |> Direction.directionsFromChar
    match (cellHistory |> Seq.contains state.Heading) with
    | true -> LoopFound
    | false ->  let newCellValue = Direction.directionsToChar (state.Heading :: cellHistory)
                let updatedMap = state.Map |> Matrix.withValueAt state.Position newCellValue
                Running { state with 
                            Map = updatedMap
                        }

let moveOrRotate state =
    let newPos = Vector.offsetWith state.Position state.Heading 1
    match Matrix.tryGet state.Map newPos with
    | None -> BoardExited state
    | Some c when "#O".Contains c -> rotate state |> writePosition
    | Some _ -> move state newPos |> writePosition

let rec solve state =
    //printfn "%O" state.Map
    match moveOrRotate state with
    | BoardExited finalState -> finalState
    | LoopFound -> failwith "Unexpected error"
    | Running newState -> solve newState

let rec detectLoop state =
    match moveOrRotate state with
    | BoardExited _ -> false
    | LoopFound -> true
    | Running newState -> detectLoop newState

let placeObstacle state pos =
    { state with
        Map = Matrix.withValueAt pos 'O' state.Map
    }

printfn "%s" Direction.cardinalSymbolCombinations

let map = 
    File.ReadLines("input.txt")
    |> Seq.map (fun str -> str.Replace('.',' '))
    |> Seq.map (fun str -> str.Replace('^', northChar))
    |> Matrix.fromStrings

let start = findStart map
let state = createState start Direction.North map
let solved = state |> solve
let visitedCells = solved.Map |> Matrix.findAllOf Direction.cardinalSymbolCombinations 

Matrix.print solved.Map

visitedCells
|> Seq.length
|> printfn "Part 1: %A"

visitedCells
|> Seq.map (placeObstacle state)
|> PSeq.filter detectLoop
|> Seq.length
|> printfn "Part 2: %d"

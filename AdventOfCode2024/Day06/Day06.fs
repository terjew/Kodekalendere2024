open System.IO
open Utilities

type State = 
    {
        Position : int * int
        Heading : Direction
        Map : Matrix
        LineSymbol : char
        Trail : ((int * int) * Direction) list
    }

let createState pos heading map =
    {
        Position = pos;
        Heading = heading;
        Map = map;
        LineSymbol = Direction.lineSymbol heading;
        Trail = List.empty
    }

let findStart matrix =
    matrix |> Matrix.find '^'

let rotate state =
    let newHeading = Direction.next state.Heading 2
    { state with
        Heading = newHeading;
        LineSymbol = (Direction.lineSymbol newHeading)
        Map = state.Map |> Matrix.withValueAt state.Position (Direction.symbol newHeading)
    }    

let move state newPos =
    let oldSymbol = Matrix.get state.Map newPos
    let newSymbol = match oldSymbol with
                    | '|' -> '+'
                    | '-' -> '+'
                    | '^' -> '+'
                    | _ -> state.LineSymbol
    { state with 
        Map = state.Map |> Matrix.withValueAt newPos newSymbol
        Position = newPos
        Trail = (state.Position, state.Heading) :: state.Trail
    }

let moveOrRotate state =
    let newPos = Vector.offsetWith state.Position state.Heading 1
    match Matrix.tryGet state.Map newPos with
    | Some c when c = '#' -> rotate state |> Some
    | Some _ -> move state newPos |> Some
    | None -> None

let rec solve state =
    match moveOrRotate state with
    | Some newState -> solve newState
    | None -> state

let rec solveForLoops state count =
    match moveOrRotate state with
    | Some newState -> solveForLoops newState count
    | None -> state

let map = File.ReadLines("input.txt") |> Matrix.fromStrings        
let start = findStart map
let state = createState start Direction.North map

let solved = state |> solve
Matrix.print solved.Map
solved.Trail |> List.distinctBy (fun (pos, dir) -> pos) |>  List.length |> printf "Trail: %A"

solved.Map 
|> Matrix.findAllOf "^v<>|-+" 
|> Seq.length
|> printf "Part 1: %A"

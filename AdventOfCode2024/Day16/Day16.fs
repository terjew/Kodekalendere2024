open System.IO
open Utilities
open System

type World =
    {
        map : Matrix
        start : int * int
        goal : int * int
    }

type Move =
    | Forward
    | Rotate of bool

type PlayerState = 
    {
        pos : int * int
        heading : Direction
    }

let getWorld filename = 
    let map = File.ReadAllLines(filename) |> Matrix.fromStrings
    { 
        map = map
        start = map |> Matrix.find 'S'
        goal = map |> Matrix.find 'E'
    }

let solveAStar world =
    let start = { pos = world.start; heading = Direction.East}
    let goal = { pos = world.goal; heading = Direction.North}
    let isPassable pos =
        (pos |> (Matrix.get world.map)) <> '#'

    let apply move state =
        match move with
        | Forward -> 
            let forwardPos = Vector.neighbor state.pos state.heading
            if goal.pos = forwardPos then 
                goal
            else 
                {state with pos = Vector.neighbor state.pos state.heading}
        | Rotate cw -> {state with heading = (Direction.next state.heading (match cw with |true -> 2 | false -> -2))}

    let neighbors (state:PlayerState) = 
        seq [
            let forward = state |> apply Forward
            if isPassable forward.pos then yield forward
            yield state |> apply (Rotate false)
            yield state |> apply (Rotate true)
        ]

    let fCost state next = 
        //next is always the goal state
        //We ignore the heading, but instead use a heuristic that determines how many minimum heading changes are needed to get there
        1.0

    //cost of actually moving from state to next (must be a neighbor)
    let gCost state next = 
        match next with 
        | goalState when goalState = goal -> 
            1.0 //the last step into goal always costs 1.0 if it is really a neighbor (is in front)
        | _ ->
            match state.heading = next.heading with
            | true -> 
                1.0
            | false -> 
                1000.0

    AStar.search start goal {
        neighbours = neighbors
        fCost = fCost
        gCost = gCost
        maxIterations = None
    }

let rec scorePath state path =
    match path with
    | [] -> 0
    | next :: tail -> 
        if tail.Length = 0 then 1 //last move is always 1 (move forward into goal)
        else 
            let cost =             
                match state.heading = next.heading with
                | true -> 1
                | false -> 1000
            cost + scorePath next tail
    
let render map state =
    map |> Matrix.withValueAt state.pos 'o'

let world = getWorld "input.txt"

let solution = solveAStar world |> Option.get |> Seq.rev |> List.ofSeq
let sub = solution.GetSlice (Some 1, Some (solution.Length - 2))
sub |> List.fold render world.map |> Matrix.printColored Matrix.defaultColormap |> ignore

solution.GetSlice (Some 1, None) |> scorePath {pos=world.start; heading=Direction.East} |> printfn "%A"

open System.IO
open Utilities
open System

type World =
    {
        map : Matrix
        start : int * int
        goal : int * int
    }

type Player = 
    {
        pos : int * int
        heading : Direction
        trail : (int * int) list
    }

let getWorld filename = 
    let map = File.ReadAllLines(filename) |> Matrix.fromStrings
    { 
        map = map
        start = map |> Matrix.find 'S'
        goal = map |> Matrix.find 'E'
    }

let initializePlayer world =
    {
        pos = world.start
        heading = Direction.East
        trail = []
    }

let move player pos =
    { player with
        pos = pos
        trail = player.trail @ [pos]
    }

let rec traverse world player =
    if world.goal = player.pos then seq { player.trail }
    else
        player.pos 
        |> Matrix.neighborsWithValues world.map
        |> Seq.filter (fun kvp -> kvp.Value <> '#')
        |> Seq.map (fun kvp -> kvp.Key)
        |> Seq.except player.trail
        |> Seq.map (move player)
        |> Seq.collect (traverse world)



let rec scorePath pos heading path =
    match path with
    | [] -> 0
    | next :: tail -> 
        let offset = Vector.subtract next pos
        let direction = Direction.direction offset
        let rotationScore = match direction with 
                            | h when h = heading -> 0
                            | _ -> 1000
        rotationScore + 1 + scorePath next direction tail

let world = getWorld "sample.txt"
let player = initializePlayer world

let paths = player |> traverse world
paths |> Seq.map (scorePath world.start Direction.East) |> Seq.min |> printfn "Part 1: %A"

//[<CustomComparison; CustomEquality>]
//type PlayerState = 
//    {
//        pos : int * int
//        heading : Direction option
//    }
//    override this.GetHashCode() =
//        hash (this.pos)

//    override this.Equals(other) =
//        match other with
//        | :? PlayerState as otherPlayer -> (this.pos = otherPlayer.pos) && (this.heading = None || otherPlayer.heading = None || this.heading = otherPlayer.heading)
//        | _ -> false

//    interface IComparable with
//        member this.CompareTo other =
//            match other with
//            | :? PlayerState as otherPlayer -> (this :> IComparable<_>).CompareTo otherPlayer
//            | _ -> -1

//    interface IComparable<PlayerState> with
//        member this.CompareTo other = Vector.manhattanDistance this.pos other.pos

//let solveAStar world =
//    let neighbors player = 
//        Matrix.neighborCoordsWithDirection world.map player.pos
//        |> Seq.filter (fun kvp -> '#' <> Matrix.get world.map kvp.Value)
//        |> Seq.map (fun kvp -> {heading = Some kvp.Key; pos = kvp.Value})

//    let fCost state next = 
//        (float (Vector.manhattanDistance next.pos state.pos + 1000)) //fixme: 0 if next is straight ahead, 2000 if facing away

//    //cost of actually moving from state to next (must be a neighbor)
//    let gCost state next = 
//        let offset = Vector.subtract next.pos state.pos
//        let direction = Direction.direction offset
//        let rotationCount = Direction.countRotationsBetween state.heading.Value direction
//        (float (rotationCount * 1000 + 1))

//    AStar.search { pos = world.start; heading = Some Direction.East} {pos = world.goal; heading = None} {
//        neighbours = neighbors
//        fCost = fCost
//        gCost = gCost
//        maxIterations = None
//    }
    
//let state = {pos = world.start; heading = Some East}
//solveAStar world |> printfn "%A"

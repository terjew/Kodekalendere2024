﻿open System.IO
open Utilities

let getTrailheads map =
    map |> Matrix.findAll '0'

let rec reachablePeaks map currentHeight trail pos =
    let newTrail = pos :: trail
    match currentHeight with
    | '9' -> [pos]
    | _ -> 
        let next = char((int)currentHeight + 1)
        let continuations = Matrix.neighborCoordsWithDirection map pos 
                            |> Seq.map (fun kvp -> kvp.Value)
                            |> Seq.filter (fun (x,y) -> (Matrix.get map (x,y)) = next)
        let foo = continuations |> Seq.map (reachablePeaks map next newTrail) |> Seq.collect id
        foo |> List.ofSeq

let processTrailhead filter map pos  =
    pos 
    |> reachablePeaks map '0' []
    |> filter
    |> Seq.length

let scoreTrailhead = processTrailhead Seq.distinct
let rateTrailhead = processTrailhead id

let map = File.ReadAllLines("input.txt") |> Matrix.fromStrings
let trailheads = map |> getTrailheads

trailheads |> Seq.sumBy (scoreTrailhead map) |> printfn "Part 1: %A"
trailheads |> Seq.sumBy (rateTrailhead map) |> printfn "Part 2: %A"

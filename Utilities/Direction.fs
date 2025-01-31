﻿namespace Utilities

type Direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
module Direction =
    let cardinal = [|North ; East ; South ; West|]
    let ordinal = [|NorthEast ; SouthEast ; SouthWest ; NorthWest|]
    let cardinalAndOrdinal = [|North ; NorthEast ; East ; SouthEast ; South ; SouthWest ; West ; NorthWest|]

    let cardinalSymbols = "^>v<" |> Seq.toArray

    let directionToChar cardinalDirection = 
        let index = cardinal |> Array.findIndex ((=) cardinalDirection)
        cardinalSymbols[index]

    let directionFromChar char =
        let index = cardinalSymbols |> Array.findIndex ((=) char)
        cardinal[index]

    let degrees direction =
        match direction with
        | North -> 0
        | NorthEast -> 45
        | East -> 90
        | SouthEast -> 135
        | South -> 180
        | SouthWest -> 225
        | West -> 270
        | NorthWest -> 315

    let headingDiff h1 h2 = (h2 - h1 + 540) % 360 - 180;

    let countRotationsBetween a b =
        let h1 = degrees a
        let h2 = degrees b
        headingDiff h1 h2 |> abs |> (fun deg -> deg / 90)

    let oposite direction = 
        match direction with
        | North -> South
        | NorthEast -> SouthWest
        | East -> West
        | SouthEast -> NorthWest
        | South -> North
        | SouthWest -> NorthEast
        | West -> East
        | NorthWest -> SouthEast

    let offset direction = 
        match direction with
        | North -> (0,-1)
        | NorthEast -> (1,-1)
        | East -> (1,0)
        | SouthEast -> (1,1)
        | South -> (0,1)
        | SouthWest -> (-1,1)
        | West -> (-1,0)
        | NorthWest -> (-1,-1)

    let direction offset = 
        match offset with
        | (0,-1) -> North
        | (1,-1) -> NorthEast
        | (1,0) -> East
        | (1,1) -> SouthEast
        | (0,1) -> South
        | (-1,1) -> SouthWest
        | (-1,0) -> West
        | (-1,-1) -> NorthWest 
        | _ -> failwith "Unknown offset"

    let next direction steps = 
        let index = cardinalAndOrdinal |> Array.findIndex ((=) direction)
        let next = (index + steps + cardinalAndOrdinal.Length) % cardinalAndOrdinal.Length
        cardinalAndOrdinal[next]

module DirectionBitmask =
    let bitmaskSymbols = "?╵╶└╷║┌╟╴┘═╧┐╢╤╬"

    let toBitmask cardinalDirection =
        match cardinalDirection with
        | North -> 1
        | East -> 2
        | South -> 4
        | West -> 8
        | _ -> failwith "unexpected direction"

    let charFromBitmask int =
        bitmaskSymbols[int]

    let bitmaskFromChar (char:char) =
        match bitmaskSymbols.IndexOf char with
        | -1 -> 0
        | a -> a

    let directionsFromBitmask int =
        [
            if (int &&& 1) = 1 then yield North
            if (int &&& 2) = 2 then yield East
            if (int &&& 4) = 4 then yield South
            if (int &&& 8) = 8 then yield West
        ]

    let directionsFromBitmaskChar char =
        char |> bitmaskFromChar |> directionsFromBitmask

    let directionsToBitmaskChar directions = 
        directions |> Seq.sumBy toBitmask |> charFromBitmask

namespace Utilities

type Direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
module Direction =
    let cardinal = [|North ; East ; South ; West|]
    let ordinal = [|NorthEast ; SouthEast ; SouthWest ; NorthWest|]
    let cardinalAndOrdinal = [|North ; NorthEast ; East ; SouthEast ; South ; SouthWest ; West ; NorthWest|]

    let cardinalSymbols = "╵╴╷╶"
    let cardinalSymbolCombinations = "?╵╶└╷║┌╟╴┘═╧┐╢╤╬"


    let toInt cardinalDirection =
        match cardinalDirection with
        | North -> 1
        | East -> 2
        | South -> 4
        | West -> 8
        | _ -> failwith "unexpected direction"

    let char int =
        cardinalSymbolCombinations[int]

    let intValueFromChar (char:char) =
        match cardinalSymbolCombinations.IndexOf char with
        | -1 -> 0
        | a -> a

    let directionsFromInt int =
        [
            if (int &&& 1) = 1 then yield North
            if (int &&& 2) = 2 then yield East
            if (int &&& 4) = 4 then yield South
            if (int &&& 8) = 8 then yield West
        ]

    let directionsFromChar char =
        char |> intValueFromChar |> directionsFromInt

    let directionsToChar directions = 
        directions |> Seq.sumBy toInt |> char

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
        let next = (index + steps) % cardinalAndOrdinal.Length
        cardinalAndOrdinal[next]

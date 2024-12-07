namespace Utilities

type Direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
module Direction =
    let cardinal = [|North ; East ; South ; West|]
    let ordinal = [|NorthEast ; SouthEast ; SouthWest ; NorthWest|]
    let cardinalAndOrdinal = [|North ; NorthEast ; East ; SouthEast ; South ; SouthWest ; West ; NorthWest|]

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

    let symbol direction = 
        //let symbols = @"|/-\|/-\"
        let symbols = @"^/>\v/<\"
        let index = cardinalAndOrdinal |> Array.findIndex ((=) direction)
        symbols[index]

    let lineSymbol direction = 
        let symbols = @"|/-\|/-\"
        let index = cardinalAndOrdinal |> Array.findIndex ((=) direction)
        symbols[index]

    let next direction steps = 
        let index = cardinalAndOrdinal |> Array.findIndex ((=) direction)
        let next = (index + steps) % cardinalAndOrdinal.Length
        cardinalAndOrdinal[next]

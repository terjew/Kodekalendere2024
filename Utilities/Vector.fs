namespace Utilities

module Vector = 
    let add (x1,y1) (x2,y2) =
        (x1 + x2, y1 + y2)

    let subtract (x1,y1) (x2,y2) =
        (x1 - x2, y1 - y2)

    let mul (x,y) amount =
        (x * amount, y * amount)

    let neighbor pos dir =
        (dir, dir |> Direction.offset |> add pos)

    let manhattanDistance (x1,y1) (x2,y2) =
        abs (x2 - x1) + abs (y2 - y1)

    let offsetWith pos dir amount =
        dir 
        |> Direction.offset 
        |> (fun (x,y) -> (x * amount, y * amount))
        |> add pos

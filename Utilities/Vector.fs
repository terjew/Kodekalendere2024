namespace Utilities

module Vector = 
    let apply f (x,y) =
        (f x, f y)
    
    let add (x1,y1) (x2,y2) =
        (x1 + x2, y1 + y2)

    let subtract (x1,y1) (x2,y2) =
        (x1 - x2, y1 - y2)

    let mul (x,y) amount =
        (x * amount, y * amount)

    let neighbor pos dir =
        dir |> Direction.offset |> add pos

    let neighbors pos =
        Direction.cardinal 
        |> Seq.map (neighbor pos)

    let neighborWithDirection pos dir =
        (dir, neighbor pos dir)
    
    let neighborsWithDirection pos =
        Direction.cardinal 
        |> Seq.map (neighborWithDirection pos)

    let manhattanDistance (x1,y1) (x2,y2) =
        abs (x2 - x1) + abs (y2 - y1)

    let offsetWith pos dir amount =
        dir 
        |> Direction.offset 
        |> (fun (x,y) -> (x * amount, y * amount))
        |> add pos

module Vector3 =
    let apply f (x,y,z) =
        (f x, f y, f z)

    let add ((x1,y1,z1):'int64'*int64*int64) ((x2,y2,z2):int64*int64*int64) =
        (x1 + x2, y1 + y2, z1 + z2)

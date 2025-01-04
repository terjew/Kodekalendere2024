open System.IO
open Utilities

let parseLine line =
    line |> Regex.transformGroups4 "p=([-\d]+),([-\d]+) v=([-\d]+),([-\d]+)" Value.asInt Value.asInt Value.asInt Value.asInt

let parse filename = 
    File.ReadAllLines filename
    |> Seq.map parseLine

let sx = 101
let sy = 103
let filename = "input.txt"

let modulo y x =
    match x % y with
    | a when a >= 0 -> a
    | b -> b + y

let modx = modulo sx
let mody = modulo sy

let calculatePosition steps (px,py,vx,vy) =
    let (px1,py1) = 
        steps 
        |> Vector.mul (vx,vy)
        |> Vector.add (px,py)
    (px1 |> modx, py1 |> mody)

let getSide p size =
    let half = size / 2
    if p < half then Some 0
    elif p > half then Some 1
    else None

let getQuadrant (x,y) =
    let sideX = getSide x sx
    let sideY = getSide y sy
    if sideX.IsNone || sideY.IsNone then None
    else Some (sideX.Value, sideY.Value)
    
let robots = parse filename

robots
|> Seq.map (calculatePosition 100)
|> Seq.choose getQuadrant
|> Seq.countBy id
|> Seq.map snd
|> Seq.fold (*) 1
|> printfn "Part 1: %A"

let render matrix robot =
    matrix |> Matrix.withValueAt robot '*'

let findTree () =
    seq {
        let blank = Matrix.create sx sy '.'
        for i in [0..1000000] do
            if ((i % sx) = 11) || ((i % sy) = 89) then 
                let map = 
                    robots
                    |> Seq.map (calculatePosition i)
                    |> Seq.fold render blank
                let horizontalRows = map |> Matrix.findRowsMatching @"\*\*\*\*\*\*\*\*"
                if horizontalRows |> Seq.length > 0 then
                    yield Some (i,map)            
                else
                    yield None
    }

findTree() 
|> Seq.pick id 
|> (fun (i,m) -> 
    printfn "Part 2: %i" i
    //m |> (Matrix.printColored Matrix.defaultColormap)
    )
|> ignore



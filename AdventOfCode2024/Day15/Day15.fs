open System.IO
open Utilities
open System.Drawing

let colormap char =
    let color = 
        match char with 
        |'#' -> Color.DimGray
        |'@' -> Color.Green
        |'[' -> Color.DarkOrange
        |']' -> Color.DarkOrange
        |'O' -> Color.DarkOrange
        |'.' -> ColorTranslator.FromHtml("#222222")
        | _  -> Color.Cyan
    (char,color)

let parse filename = 
    File.ReadAllLines filename
    |> Seq.toList
    |> SequenceHelper.splitByValue ""
    |> (fun sections -> (Matrix.fromStrings sections[0], String.concat "" sections[1]))

let rec tryMove source pos dir matrix =
    let targetPos = Vector.neighbor pos dir
    let target = Matrix.get matrix targetPos
    match target with 
    | 'O' -> tryMove target targetPos dir matrix
    | '[' -> tryMove target targetPos dir matrix
    | ']' -> tryMove target targetPos dir matrix
    | '.' -> Matrix.withValueAt targetPos source matrix |> Some
    | '#' -> None
    | _ -> failwith "wtf mate"

let move matrix direction =
    let dir = Direction.directionFromChar direction
    let pos = Matrix.find '@' matrix
    let ret = 
        match tryMove '@' pos dir matrix with
        | None -> matrix
        | Some m -> m |> Matrix.withValueAt pos '.' |> Matrix.withValueAt (Vector.neighbor pos dir) '@'
    ret

let (map, moves) = parse "input.txt"

let endstate = moves |> Seq.fold move map 
endstate |> Matrix.printColored colormap |> ignore

endstate
|> Matrix.findAll 'O'
|> Seq.sumBy (fun (x,y) -> y * 100 + x)
|> printfn "Part1: %d"

let widenBoard matrix = 
    matrix 
    |> Matrix.rows 
    |> Seq.map (fun (row:string) -> 
                row.Replace(".", "..").Replace("#", "##").Replace("O", "[]").Replace("@", "@.")
                )
    |> Matrix.fromStrings


let rec getBoxesVertical pos dir matrix =
    let handleBox boxPos otherHalfDirection matrix =
        seq [
            yield boxPos
            yield! getBoxesVertical boxPos dir matrix
            let otherPos = Vector.neighbor boxPos otherHalfDirection
            yield otherPos
            yield! getBoxesVertical otherPos dir matrix
        ]
    seq [
        let targetPos = Vector.neighbor pos dir
        let target = Matrix.get matrix targetPos
        match target with 
        | '[' -> yield! handleBox targetPos Direction.East matrix           
        | ']' -> yield! handleBox targetPos Direction.West matrix
        | _ -> ()
    ]

let rec getBoxesHorizontal pos dir matrix =
    seq [
        let targetPos = Vector.neighbor pos dir
        let target = Matrix.get matrix targetPos
        match target with 
        | char when "[]".Contains char -> 
            yield targetPos
            yield! getBoxesHorizontal targetPos dir matrix
        | _ -> ()
    ]

let rec tryMoveWide pos dir matrix =
    let cannotMove boxPos =
        Matrix.neighbor matrix dir boxPos = '#'

    let moveBox modifiedMap boxPos =
        let newPos = Vector.neighbor boxPos dir
        let boxValue = Matrix.get matrix boxPos
        modifiedMap |> Matrix.withValueAt boxPos '.' |> Matrix.withValueAt newPos boxValue

    let distance a b =
        let diff = Vector.distance b a
        match dir with
        | Direction.North -> diff |> snd
        | Direction.South -> diff |> snd
        | _ -> diff |> fst

    let getBoxes = 
        match dir with
        | Direction.North -> getBoxesVertical
        | Direction.South -> getBoxesVertical
        | _ -> getBoxesHorizontal

    let target = Matrix.neighbor matrix dir pos
    match target with
    | '#' -> None
    | '.' -> Some matrix
    | _ -> 
        let boxesInvolved = getBoxes pos dir matrix |> Set
        if (boxesInvolved |> Seq.exists (cannotMove)) 
        then 
            None
        else 
            boxesInvolved 
            |> Seq.sortByDescending (distance pos)
            |> Seq.fold moveBox matrix 
            |> Some

let moveWide matrix direction =
    let dir = Direction.directionFromChar direction
    let pos = Matrix.find '@' matrix
    let ret = 
        match tryMoveWide pos dir matrix with
        | None -> matrix
        | Some m -> m |> Matrix.withValueAt pos '.' |> Matrix.withValueAt (Vector.neighbor pos dir) '@'
    ret

let wideMap = widenBoard map

let endstate2 = moves |> Seq.fold moveWide wideMap 
endstate2 |> Matrix.printColored colormap |> ignore

endstate2
|> Matrix.findAllOf "["
|> Seq.sumBy (fun (x,y) -> y * 100 + x)
|> printfn "Part2: %d"

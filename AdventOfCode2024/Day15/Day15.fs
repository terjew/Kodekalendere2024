open System.IO
open Utilities
open System.Threading

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
    //System.Console.Clear()
    //ret |> Matrix.printColored Matrix.defaultColormap
    //Thread.Sleep(1)
    ret

let (map, moves) = parse "input.txt"

let endstate = moves |> Seq.fold move map 
endstate |> Matrix.printColored Matrix.defaultColormap

endstate
|> Matrix.findAll 'O'
|> Seq.sumBy (fun (x,y) -> y * 100 + x)
|> printfn "Part1: %d"
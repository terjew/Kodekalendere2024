open System.IO
open Utilities
open Pastel

let parseMap filename =
    File.ReadAllLines(filename)
    |> Matrix.fromStrings

let solveAStar start goal map =
    let h pos next = 
        (float (Vector.manhattanDistance pos next))
    let g _ _ = 
        1.0
    let neighbors pos = 
        Matrix.neighborsWithValues map pos
        |> Seq.filter (fun kvp -> '#' <> kvp.Value)
        |> Seq.map (fun kvp -> kvp.Key)

    AStar.search start goal {
        fCost = h
        gCost = g
        neighbours = neighbors
        maxIterations = None
    }

let colormap char =
    let color = 
        match char with 
        |'#' -> System.ConsoleColor.DarkRed
        |'X' -> System.ConsoleColor.Magenta
        |'S' -> System.ConsoleColor.Cyan
        |'E' -> System.ConsoleColor.Cyan
        |'o' -> System.ConsoleColor.DarkCyan
        | _  -> System.ConsoleColor.Black
        //System.Drawing.ColorTranslator.FromHtml("#124542")
    (string char).Pastel(color)

let printColor (string:string) =    
    string 
    |> Seq.map colormap 
    |> String.concat "" 
    |> printfn "%s" 

let draw char map pos =
    map |> Matrix.withValueAt pos char

let drawPath char map positions = 
    positions |> Seq.fold (draw char) map 

let printMapColored map =
    map
    |> Matrix.rows 
    |> Seq.map printColor 
    |> Seq.toList

let map = parseMap "input.txt"
let start = map |> Matrix.find 'S'
let goal = map |> Matrix.find 'E'
let solution = map |> solveAStar start goal |> Option.get |> Seq.toList

printMapColored map |> ignore

let canCheat directions map pos =
    let neighbors = directions |> Seq.map (fun dir -> dir |> Vector.neighbor pos |> Matrix.tryGet map)
    neighbors |> Seq.forall (fun neighbor -> neighbor.IsSome && neighbor.Value <> '#')

type Cheat = 
    | Horizontal of int*int 
    | Vertical of int*int
    | None

let cheatDirection map pos =
    if canCheat [Direction.West;Direction.East] map pos 
        then Horizontal pos
    elif canCheat [Direction.North;Direction.South] map pos 
        then Vertical pos
    else 
        None

let getCheats map = 
    map 
    |> Matrix.findAll '#'
    |> Seq.map (cheatDirection map)

let cheats = getCheats map |> Seq.filter (fun cheat -> cheat <> None)

let scoreCheat solution cheat =
    let neighbors = 
        match cheat with 
        | Vertical (x,y) -> (x,y) |> Vector.neighborsVertical
        | Horizontal (x,y) -> (x,y) |> Vector.neighborsHorizontal
        | _ -> failwith "wtf"
    neighbors 
    |> Seq.map (fun elem -> List.findIndex ((=) elem) solution)
    |> (fun seq -> abs (Seq.head seq - Seq.last seq) - 2)


let scores = cheats |> Seq.map (scoreCheat solution) 
scores |> Seq.countBy id |> Seq.sortBy fst |> List.ofSeq |> printfn "%A"
scores |> Seq.countBy ((<) 99) |> printfn "Part 1: %A"

let indexOf list value =
    list |> List.findIndex ((=) value) 
    
let getCheatsFromPosition map solution position =
    let startIndex = List.findIndex ((=) position) solution
    printfn "%A" startIndex
    //find all cells that have manhattan distance of 20 or lower
    let posWithIndex = 
        position 
        |> Matrix.allWithinManhattanDistance map 20
        //and are in the solution
        |> Seq.filter (fun pos -> List.contains pos solution)
        //and have an index above current
        |> Seq.map (fun pos -> (pos, List.findIndex ((=) pos) solution))

    posWithIndex
    //and calculate their diff
    |> Seq.map (fun (pos,index) -> index - startIndex - (Vector.manhattanDistance pos position))
    |> Seq.filter (fun value -> value >= 100)

let getAdvancedCheats map (solution:(int*int) list) = 
    //for each position in solution
    solution 
    //|> Seq.take 10
    |> Seq.map (getCheatsFromPosition map solution)
    |> Seq.sumBy Seq.length
        
let scores2 = getAdvancedCheats map solution
//scores2 |> Seq.countBy id |> Seq.sortBy fst |> List.ofSeq |> printfn "%A"
scores2 (*|>*) (*Seq.countBy ((<) 99)*) |> printfn "Part 2: %A"

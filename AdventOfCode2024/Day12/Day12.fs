open System.IO
open Utilities

let parseMap filename =
    File.ReadAllLines(filename)
    |> Matrix.fromStrings

let getRegion map pos value =
    let mutable toVisit = [pos] |> Set
    let mutable region = Set.empty

    let visit p =
        region <- Set.add p region
        Matrix.neighborsWithValues map p
        |> Map.filter (fun pos _ -> (Set.contains pos region) |> not)
        |> Map.filter (fun _ v -> value = v)
        |> Map.keys
    
    while Set.isEmpty toVisit |> not do
        let current = toVisit |> Seq.head
        let next = visit current |> Set
        toVisit <- (toVisit |> Set.remove current |> Set.union next)

    region

let calculateArea region =
    Seq.length region


let getPerimeter region = 
    region 
    |> Seq.collect Vector.neighborsWithDirection
    |> Seq.filter (fun (_, pos) -> (Seq.contains pos region) |> not)

let calculatePerimeter region = 
    region 
    |> getPerimeter
    |> Seq.length

let areVerticalNeighbors (ax,ay) (bx,by) = (ax = bx) && ((ay - by) |> abs) = 1     
let areHorizontalNeighbors (ax,ay) (bx,by) = (ay = by) && ((ax - bx) |> abs) = 1        

let isHorizontalEdge dir =
    match dir with 
    | Direction.North -> true
    | Direction.South -> true
    | _ -> false

let getSegments (dir, edgenodes) =
    let isHorizontal = isHorizontalEdge dir
    let areNeighbors = if isHorizontal then areHorizontalNeighbors else areVerticalNeighbors
    let ordering (x,y) = if isHorizontal then (y,x) else (x,y)
    let ordered = edgenodes |> Seq.map snd |> Seq.sortBy ordering |> Seq.toList
    seq {
        yield ordered[0]
        for i in [1..ordered.Length - 1] do
            let current = ordered[i-1]
            let next = ordered[i]
            if (not (areNeighbors current next)) then yield current
    }

    
let calculatePerimeterWithDiscount region = 
    let directionsWithEdges = region 
                            |> getPerimeter
                            |> Seq.groupBy (fun (dir, _) -> dir)

    directionsWithEdges |> Seq.collect getSegments |> Seq.length

let calculateFence region =
    let area = calculateArea region
    let perimeter = calculatePerimeter region
    area * perimeter

let calculateFenceWithDiscount region =
    let area = calculateArea region
    let perimeter = calculatePerimeterWithDiscount region
    area * perimeter

let map = parseMap "input.txt"
map
|> Matrix.map getRegion
|> Seq.distinct
|> Seq.sumBy calculateFence
|> printfn "Part 1: %A"

map
|> Matrix.map getRegion
|> Seq.distinct
|> Seq.sumBy calculateFenceWithDiscount
|> printfn "Part 2: %A"
open Utilities
open Function
open System.IO

let dirPad = Matrix.fromStringArray [|"#^A";"<v>"|]
let numPad = Matrix.fromStringArray [|"789";"456";"123";"#0A"|]

let scoreSolution solution = 
    //prefer streaks
    solution |> List.pairwise |> List.sumBy (fun (a,b) -> if a=b then 0 else 1)

let charsToString (chars:char seq) =
        chars |> System.String.Concat

let render solution =
    solution
    |> Seq.rev 
    |> Seq.pairwise
    |> Seq.map (fun (a,b) -> Vector.subtract b a)
    |> Seq.map (Direction.direction >> Direction.directionToChar)
    |> Seq.toList
    |> (fun l -> l @ ['A'])

let getMovesBetween = memoize <| fun (pad, start, goal) ->    
    let startPos = Matrix.find start pad
    let goalPos = Matrix.find goal pad
    let solutions = Matrix.solveAll startPos goalPos pad [startPos] |> List.sortBy List.length
    let shortestLength = solutions[0].Length
    solutions 
    |> List.filter (fun solution -> solution.Length = shortestLength)
    |> List.map render
    |> List.sortBy scoreSolution
    |> List.head

let getInputs pad code = 
    "A" + code 
    |> Seq.pairwise 
    |> Seq.collect (fun (a,b) -> getMovesBetween (pad, a, b))
    |> Seq.map string 
    |> String.concat ""

let getCompoundInputs2 = 
    getInputs numPad 
    >> getInputs dirPad 
    >> getInputs dirPad

let complexity ((code:string), inputs) = 
    let a = String.length inputs |> int64
    let b = code.Substring(0, 3) |> int64
    a * b

File.ReadAllLines "input.txt" 
|> Seq.map (fun code -> (code, getCompoundInputs2 code)) 
|> Seq.sumBy complexity 
|> printfn "Part 1: %A"



(**
Part 2 starts here. Not much reuse from part 1.
**)

let dirPadMovesL1 = 
    "A<^>v" 
    |> Seq.toList 
    |> SequenceHelper.getPermsWithRep 2
    |> Seq.map (fun lst -> (lst[0],lst[1]))
    |> Seq.map (fun (start,goal) -> ((start,goal),getMovesBetween (dirPad,start,goal) |> charsToString))
    |> Map.ofSeq

let calculateToLevel toLevel sequence = 
    let mutable currentSequence = sequence
    let mutable map = Map.empty
    let length = currentSequence |> String.length |> int64
    map <- Map.add 1 (length,currentSequence) map

    for i in [2..toLevel - 1] do
        currentSequence <- getInputs dirPad currentSequence
        let length = currentSequence.Length |> int64
        map <- Map.add i (length,currentSequence) map
    map

let middleCacheLevel = 14
let middleCache = dirPadMovesL1 |> Map.map (fun _ value -> value |> calculateToLevel middleCacheLevel)

let cost l1Move atLevel = 
    let middle = middleCache[l1Move]
    let (_, middleSequence) = middle[middleCacheLevel-1]
    let levelOffset = atLevel - middleCacheLevel
    "A" + middleSequence 
    |> Seq.pairwise
    |> Seq.map (fun pair -> middleCache[pair])
    |> Seq.map (fun cacheForPair -> cacheForPair[levelOffset])
    |> Seq.sumBy (fun (length,_) -> length |> int64)

let bottomCacheLevel = 26
let bottomCache = dirPadMovesL1 |> Map.map (fun pair _ -> cost pair bottomCacheLevel) 

let bottomCacheLookup pair = 
    Map.find pair bottomCache

let complexity26 ((code:string), numpadInputs) = 
    let a = 
        "A" + numpadInputs 
        |> Seq.pairwise
        |> Seq.sumBy bottomCacheLookup
    let b = code.Substring(0, 3) |> int64
    a * b

File.ReadAllLines "input.txt" 
|> Seq.map (fun code -> (code, getInputs numPad code)) 
|> Seq.sumBy complexity26 
|> printfn "Part 2: %A"

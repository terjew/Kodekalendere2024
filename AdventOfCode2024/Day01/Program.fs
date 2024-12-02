open System.IO
open System.Text.RegularExpressions

let getNumbers pattern line =
    Regex.Matches(line, pattern) 
        |> Seq.map (fun m -> m.Groups[1].Value |> int)

let parse =
    getNumbers "([0-9]+)"

let getLeftAndRightLists seq =
    let left = Seq.map Seq.head seq
    let right = Seq.map Seq.last seq
    (left,right)

let sortBothLists (left,right) =
    let sortedLeft =  left |> Seq.sort
    let sortedRight = right |> Seq.sort
    (sortedLeft, sortedRight)

let distance (l,r) =
   r - l |> abs

let pairwiseDistance (left,right) =
    let pairs = Seq.zip left right
    pairs |> Seq.map distance

let getOccurenceMap seq =
    seq |> Seq.countBy id |> Map

let getCount item dict =
    if dict |> Map.containsKey item then dict |> Map.find item else 0

let getSimilarityScore item dict =
    item * getCount item dict

let calculateSimilarities (left, right) =
    let lookup = getOccurenceMap right 
    left |> Seq.map (fun l -> getSimilarityScore l lookup)

let input = File.ReadLines("input.txt")
let sample = File.ReadLines("sample.txt")

input
|> Seq.map parse
|> getLeftAndRightLists
|> sortBothLists
|> pairwiseDistance
|> Seq.sum
|> printfn "Part 1: %A"

input
|> Seq.map parse
|> getLeftAndRightLists
|> calculateSimilarities
|> Seq.sum
|> printfn "Part 2: %A"

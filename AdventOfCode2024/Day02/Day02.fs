open System.IO
open System.Text.RegularExpressions

type Report = { 
    Levels: list<int>
    Steps : list<int>
}

let getNumbers pattern line =
    Regex.Matches(line, pattern) 
        |> Seq.map (fun m -> m.Groups[1].Value |> int)

let parse =
    getNumbers "([0-9]+)"

let getDifferences list =
    let pairs = List.pairwise list
    pairs |> List.map (fun (a,b) -> b - a)

let getReport seq =
    let levels = seq |> Seq.toList
    let steps = levels |> getDifferences
    { Levels = levels; Steps = steps }

let allSameSign list =
    let signcount = list |> Seq.map sign |> Seq.distinct |> Seq.length
    signcount = 1

let allWithinRange list =
    list |> List.map abs |> List.forall (fun f -> f > 0 && f <= 3)

let isValid report =
    allSameSign report.Steps && allWithinRange report.Steps

let outOfRangeCount list = 
    list |> List.filter (fun f -> f > 0 && f <= 3) |> List.length
    
let isValidWithDampening report =
    let levelcount = report.Levels |> List.length
    let length = levelcount - 1
    let variants = report.Levels |> Utilities.SequenceHelper.combinations [] length |> Seq.map getReport
    variants |> Seq.filter isValid |> Seq.isEmpty |> not

let sample = File.ReadLines("sample.txt")
let input = File.ReadLines("input.txt")

input
|> Seq.map parse
|> Seq.map getReport
|> Seq.map isValid
|> Seq.filter id
|> Seq.length
|> printfn "Part 1: %A"

input
|> Seq.map parse
|> Seq.map getReport
|> Seq.map isValidWithDampening
|> Seq.filter id
|> Seq.length
|> printfn "Part 2: %A"



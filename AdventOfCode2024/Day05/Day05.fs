open System.IO
open System.Text.RegularExpressions
open Utilities

let parseRule line =
    let m = Regex.Match(line, @"(\d+)\|(\d+)") 
    (int m.Groups[1].Value, int m.Groups[2].Value)

let parseUpdate line =
    Regex.Matches(line, @"\d+")
    |> Seq.map (fun m -> int m.Value) |> List.ofSeq

let isValidUpdate rules update =
    update 
    |> Seq.pairwise
    |> Seq.forall (fun (a,b) -> (Set.contains (a,b) rules))

let getMiddleValue update =
    update 
    |> List.item (update.Length / 2)

let isHead update rules candidate =
    rules 
    |> Set.exists (fun (a,b) -> b = candidate && List.contains a update) 
    |> not

let rec makeValid rules update = 
    match update with
    | [] -> []
    | _ -> 
        let head = update |> Seq.find (isHead update rules)
        let tail = update |> List.except [head]
        head :: makeValid rules tail

let input = File.ReadLines("input.txt")
            |> List.ofSeq
            |> SequenceHelper.splitByValue ""

let rules = input |> Seq.head |> Seq.map parseRule |> Set
let updates = input |> Seq.last |> Seq.map parseUpdate

updates
|> Seq.filter (isValidUpdate rules) 
|> Seq.sumBy getMiddleValue 
|> printfn "Part 1: %A"

updates
|> Seq.filter (isValidUpdate rules >> not) 
|> Seq.map (makeValid rules)
|> Seq.sumBy getMiddleValue 
|> printfn "Part 2: %A"


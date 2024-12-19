open System.Text.RegularExpressions
open System.IO
open Utilities

let parseInput filename = 
    File.ReadAllLines filename
    |> Seq.toList
    |> SequenceHelper.splitByValue ""
    |> (fun list -> match list with 
                    | head::tail -> (Seq.head head, Seq.collect id tail)
                    | [] -> failwith "wtf"
        )

let checkDesign patterns (design:string) =
    let regexPattern = patterns |> String.splitWithAny ", " |> Seq.map (sprintf "(?:%s)") |> String.concat "|" |> sprintf "^(%s)*$"
    Regex.IsMatch(design, regexPattern)

let (patterns, designs) = parseInput "input.txt"

designs |> Seq.filter (checkDesign patterns) |> Seq.length |> printfn "Part 1: %A"

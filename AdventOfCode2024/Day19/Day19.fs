open System.Text.RegularExpressions
open System.IO
open Utilities
open Function

let parseInput filename = 
    File.ReadAllLines filename
    |> Seq.toList
    |> SequenceHelper.splitByValue ""
    |> (fun list -> match list with 
                    | head::tail -> (Seq.head head |> String.splitWithAny ", ", Seq.collect id tail)
                    | [] -> failwith "wtf"
        )

let checkDesign patterns (design:string) =
    let regexPattern = patterns |>  Seq.map (sprintf "(?:%s)") |> String.concat "|" |> sprintf "^(%s)*$"
    Regex.IsMatch(design, regexPattern)

let rec enumerateMemo = memoizeRec <| fun enumerateRec ((patterns:string seq), (design:string)) ->
    if design.Length = 0 then 1L
    else
        patterns 
        |> Seq.filter (fun pattern -> design.StartsWith pattern)
        |> Seq.sumBy (fun pattern -> 
            let shortened = design.Substring(pattern.Length)
            enumerateRec (patterns, shortened)
        )

let (patterns, designs) = parseInput "input.txt"

designs |> Seq.filter (checkDesign patterns) |> Seq.length |> printfn "Part 1: %A"
designs |> Seq.map (fun design -> enumerateMemo (patterns, design)) |> Seq.sum |> printfn "Part 2: %A"

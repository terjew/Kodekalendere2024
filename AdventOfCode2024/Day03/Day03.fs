open System.IO
open System.Text.RegularExpressions

let parseMuls line =
    Regex.Matches(line, @"mul\((\d{1,3}),(\d{1,3})\)") 
    |> Seq.map (fun m -> (int m.Groups[1].Value, int m.Groups[2].Value))

let mul (a,b) =
    a * b

let parseActiveSections (text:string) =
    Regex.Matches("do()"+text+"don't()", @"(?:do\(\))(.*?)(?:don't\(\))", RegexOptions.Singleline) 
    |> Seq.map (fun m -> m.Groups[1].Value)

File.ReadLines("input.txt")
|> Seq.collect parseMuls
|> Seq.sumBy mul
|> printfn "Part 1: %A"

File.ReadAllText("input.txt")
|> parseActiveSections
|> Seq.collect parseMuls
|> Seq.sumBy mul
|> printfn "Part 2: %A"


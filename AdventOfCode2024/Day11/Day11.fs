open System.IO
open System.Text.RegularExpressions
open Function

let parseLine line =
    Regex.Matches(line, @"\d+")
    |> Seq.map (fun m -> int64 m.Value)
    |> List.ofSeq

let log10i64 i =
    (int (log10 (float i)))

let countDigitsi64 i =
    (log10i64 i) + 1

let canSplit i =
    ((countDigitsi64 i) % 2) = 0

let split i =
    let numDigits = countDigitsi64 i
    let factor = pown 10L (numDigits >>> 1)
    [
        i / factor;
        i % factor;
    ]

let transform (number:int64) =
    match number with 
    | 0L -> [1L]
    | i when canSplit i -> split i
    | _ -> [number * 2024L]

let rec blinkRec count numbers =
    let transformed = numbers |> List.collect transform
    match count - 1 with
    | 0 -> transformed
    | next -> blinkRec next transformed

let blink count numbers =
    numbers |> blinkRec count |> Seq.length

let blinkRecMemo = memoizeRec <| fun blinkRec (count,number) ->
    let transformed = number |> transform
    match count - 1 with
    | 0 -> int64 transformed.Length
    | next -> 
        let ret = transformed |> Seq.sumBy (fun v -> (blinkRec (next,v)))
        ret

let blinkMemo count numbers =
    numbers |> Seq.sumBy (fun v -> (blinkRecMemo (count,v)))

let input = File.ReadAllLines("input.txt") |> Seq.map parseLine |> Seq.head

input |> blink 25 |> printfn "Part 1: %d"
input |> blinkMemo 75 |> printfn "Part 2: %d"

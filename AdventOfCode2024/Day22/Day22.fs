open System.IO
open Utilities

let mix value secret = 
    value ^^^ secret

let prune secret =
    secret % 16777216L

let step1 secret =
    secret |> mix (secret * 64L) |> prune

let step2 secret = 
    secret |> mix (secret / 32L) |> prune

let step3 secret = 
    secret |> mix (secret * 2048L) |> prune

let next = step1 >> step2 >> step3

let secretAt iteration secret =
    [0..iteration - 1] |> Seq.fold (fun state t -> next state) secret

File.ReadAllLines "input.txt"
|> Seq.map int64
|> Seq.sumBy (secretAt 2000)
|> printfn "Part1: %A"

let addSecret list =
    list @ [list |> List.last |> next]

let lastDigit secret =
    int (secret % 10L)

let buildSecretCache (secret:int64) =
    printf "."
    let initial = secret :: List.empty 
    let prices = 
        [0..2000 - 1] 
        |> Seq.fold (fun list i -> addSecret list) initial
        |> List.map lastDigit 

    let changes = 
        prices 
        |> List.pairwise 
        |> Seq.map (fun (a,b) -> 'J' + char (b-a))
        |> System.String.Concat

    (prices,changes)

let caches = 
    File.ReadAllLines "input.txt"
    |> Seq.map int64
    |> Seq.map buildSecretCache
    |> List.ofSeq

let enumerateSequences =
    [-9..9]
    |> List.map (fun i -> 'J' + (char i))
    |> SequenceHelper.getPermsWithRep 4
    |> Seq.mapi (fun i sequence -> (i, System.String.Concat sequence))

let priceSequence (sequence:string) (prices:int list,changes:string) =
    match changes.IndexOf(sequence) with
    | -1 -> 0
    | pos -> prices[pos+4]

let priceSequenceAll (i,sequence) = 
    if (i % (19*19) = 0) then
        printf "#" 
    caches |> Seq.sumBy (priceSequence sequence)

let findBestSequence =
    enumerateSequences 
    |> Seq.toArray
    |> Array.Parallel.maxBy priceSequenceAll
    |> priceSequenceAll

findBestSequence |> printfn "Part 2: %A"

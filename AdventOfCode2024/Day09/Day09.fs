open System.IO
open Utilities

let toInt char =
    int char - int '0'

let unpack i (file:int array) =
    [
        let filesize = file[0]
        let spacesize = match file.Length with 
                        | 2 -> file[1]
                        | _ -> 0

        for _ in [1..filesize] do
            yield Some i
        for _ in [1..spacesize] do
            yield None
    ]

let rec defrag blocks =
    let lastUsed = blocks |> List.findIndexBack ((<>) None)
    let firstEmpty = blocks |> List.findIndex ((=) None)
    if (lastUsed % 100) = 0 then printfn "%d - %d" lastUsed firstEmpty
    match lastUsed > firstEmpty with
    | false -> blocks
    | true -> blocks |> SequenceHelper.swapIndices lastUsed firstEmpty |> defrag

let checksumPart i v =
    match v with 
    | Some fileId -> i * fileId |> int64
    | None -> 0

let checksum blocks =
    blocks |> List.mapi checksumPart |> List.sum

File.ReadAllText("input.txt")
|> Seq.map toInt
|> Seq.chunkBySize 2
|> Seq.mapi unpack
|> Seq.collect id
|> List.ofSeq
|> defrag
|> checksum
|> printfn "Part 1: %A"

(** 

Part Two is essentially a completely new program... 

**)

type FileInfo = {
    FileId : int;
    Size : int;
    Start : int;
}

let parseBlockRanges (ints:int List) : FileInfo list = 
    [
        let mutable totalSize = 0
        for fileId in [0..((ints.Length - 1) / 2)] do
            let fileSize = ints[fileId * 2]
            yield {
                FileId = fileId;
                Size = fileSize;
                Start = totalSize;
            }
            totalSize <- totalSize + fileSize

            if (fileId * 2 + 1) < ints.Length then
                let spaceSize = ints[fileId * 2 + 1]
                totalSize <- totalSize + spaceSize
    ]

let spaceOfMinimum size (a,b) =
    (b.Start - (a.Start + a.Size)) >= size

let getFirstSpace list minSize =
    list |> List.pairwise |> List.tryFind (spaceOfMinimum minSize)

let moveFile list file (before,after) =
    let updatedFile = {file with Start = before.Start + before.Size}
    list 
    |> Seq.map (fun f -> if f.FileId = file.FileId then updatedFile else f)
    |> Seq.sortBy (fun f -> f.Start)
    |> Seq.toList

let rec defragFile (list:FileInfo list) fileId =
    if fileId = 0 then 
        list 
    else
        let file = list |> List.find (fun f -> f.FileId = fileId)
        let space = getFirstSpace list file.Size
        let updatedList = match space with 
                            |Some (a,b) when a.Start < file.Start -> moveFile list file (a,b)
                            |_ -> list 
        defragFile updatedList (fileId - 1)

let defragFiles list =
    let lastFile = list |> List.last
    defragFile list lastFile.FileId

let checksumFile file =
    [file.Start..(file.Start + file.Size - 1)]
    |> Seq.map int64 |>  Seq.sumBy ((*) (int64 file.FileId))

let checksumFiles list = 
    list |> List.sumBy checksumFile

File.ReadAllText("input.txt")
|> Seq.map toInt
|> List.ofSeq
|> parseBlockRanges
|> defragFiles
|> checksumFiles
|> printfn "%A"

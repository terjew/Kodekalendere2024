open System.IO
open System.Text.RegularExpressions
open Utilities

let parseLine line =
    Regex.Matches(line, @"\d+")
    |> Seq.map (fun m -> int64 m.Value)
    |> (fun nums -> (Seq.head nums, Seq.tail nums))

let concat a b =
    int64 ((string a) + (string b))

let apply partResult operator operand =
    let func = match operator with
                | "+" -> (+)
                | "*" -> (*)
                | "||" -> concat
                | _ -> failwith "Unknown operator"
    func partResult operand

let isSolution (target, operands) operators =
    let head = operands |> Seq.head
    let result = Seq.fold2 apply head operators (Seq.tail operands)
    target = result

let canBeSolved (target, operands)=
    let operators = ["+";"*";"||"]
    let numOperators = (Seq.length operands) - 1
    let operatorVariants = SequenceHelper.getPermsWithRep numOperators operators
    operatorVariants |> Seq.exists (isSolution (target, operands))

File.ReadLines("input.txt")
|> Seq.map parseLine
|> Seq.filter canBeSolved
|> Seq.sumBy (fun (target,_) -> target)
|> printfn "Part 1: %A"

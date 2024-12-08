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
                | "+" -> ( + )
                | "*" -> ( * )
                | "||" -> concat
                | _ -> failwith "Unknown operator"
    func partResult operand

let isSolution (target, operands) operators =
    let head = operands |> Seq.head
    let result = Seq.fold2 apply head operators (Seq.tail operands)
    target = result

let canBeSolved operators (target, operands) =
    let numOperators = (Seq.length operands) - 1
    let operatorVariants = SequenceHelper.getPermsWithRep numOperators operators
    operatorVariants |> Seq.exists (isSolution (target, operands))

let sumSolvable operators equations =
    equations 
    |> Seq.filter (canBeSolved operators)
    |> Seq.sumBy (fun (target,_) -> target)

let equations = File.ReadLines("input.txt") |> Seq.map parseLine

equations |> sumSolvable ["+";"*"] |> printfn "Part 1: %A"
equations |> sumSolvable ["+";"*";"||"] |> printfn "Part 2: %A"

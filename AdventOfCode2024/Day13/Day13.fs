open System.IO
open System.Text.RegularExpressions
open Utilities

type Game = 
    {
        A : int * int
        B : int * int
        Prize : int * int
    }

let parseVector line pattern  =
    let m = Regex.Match(line, pattern)
    (m.Groups[1].Value |> int, m.Groups[2].Value |> int)

let parseMachine (lines:string list) =
    { 
        A = parseVector lines[0] @"A: X([+-]\d+), Y([+-]\d+)";
        B = parseVector lines[1] @"B: X([+-]\d+), Y([+-]\d+)";
        Prize = parseVector lines[2] @"X=(\d+), Y=(\d+)"
    }

let parseMachines filename =
    File.ReadAllLines filename 
    |> Seq.toList
    |> SequenceHelper.splitByValue ""
    |> Seq.map parseMachine

let solutionsNaive machine =
    [
        for a in [0..100] do
            for b in [0..100] do
                let aVector = Vector.mul machine.A a
                let bVector = Vector.mul machine.B b
                let pos = Vector.add aVector bVector
                if pos = machine.Prize then yield (a,b)
    ]


let bestSolutionNaive machine = 
    match machine |> solutionsNaive with
    | [] -> None
    | list -> list |> Seq.minBy fst |> Some

let score solution = 
    match solution with
    | Some (a,b) -> a * 3 + b
    | None -> 0

//solve sets of linear equations of the form ax + by = c
let solveCrossMultiplication ((a1,b1,c1), (a2,b2,c2)) = 
    let d1 = -c1
    let d2 = -c2
    let x = (b1 * d2 - b2 * d1) / (b2 * a1 - b1 * a2)
    let y = (d1 * a2 - d2 * a1) / (b2 * a1 - b1 * a2)
    if a1 * x + b1 * y = c1 && a2 * x + b2 * y = c2
    then Some (x,y)
    else None

let toEquationSet machine =
    let vec3 = (machine.A, machine.B, machine.Prize)
    let offset = (0L,0L,10000000000000L)
    let eq1 = vec3 |> Vector3.apply fst |> Vector3.apply int64 |> Vector3.add offset
    let eq2 = vec3 |> Vector3.apply snd |> Vector3.apply int64 |> Vector3.add offset
    (eq1,eq2)

let scoreL solution = 
    match solution with
    | Some (a,b) -> a * 3L + b
    | None -> 0L

let machines = parseMachines "input.txt"

machines
|> Seq.map bestSolutionNaive
|> Seq.sumBy score
|> printfn "Part 1: %A"

machines
|> Seq.map toEquationSet
|> Seq.map solveCrossMultiplication
|> Seq.sumBy scoreL
|> printfn "Part 2: %A"

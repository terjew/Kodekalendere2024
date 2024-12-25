open System.IO
open Utilities

type Gate =
    | Input of bool
    | AND of string*string
    | OR of string*string
    | XOR of string*string
    
    static member create op a b =
        match op with
        | "AND" -> AND (a,b)
        | "OR" -> OR (a,b)
        | "XOR" -> XOR (a,b)
        | any -> failwithf "Unknown operation %s" any

let getGateValueMemo = Function.memoizeRec <| fun getGateValueRec (gates,gateId) ->
    let gate = gates |> Map.find gateId
    match gate with 
    | Input input -> input
    | AND (a,b) -> getGateValueRec (gates, a) && getGateValueRec (gates, b) 
    | OR (a,b) -> getGateValueRec (gates, a) || getGateValueRec (gates, b) 
    | XOR (a,b) -> getGateValueRec (gates, a) <> getGateValueRec (gates, b) 
    
let parseInputs inputLines map =
    let parseInput map line =
        line 
        |> Regex.transformGroups2 "(\w+): ([01])" Value.asString Value.asBool
        |> (fun (id,value) -> map |> Map.add id (Input value))
    inputLines |> Seq.fold parseInput map

let parseGates gateLines map =
    let parseGate map line =
        line 
        |> Regex.transformGroups4 "(\w+) (\w+) (\w+) -> (\w+)" Value.asString Value.asString Value.asString Value.asString
        |> (fun (a,op,b,id) -> map |> Map.add id (Gate.create op a b))
    gateLines |> Seq.fold parseGate map

let parseFile filename = 
    let (inputLines,gateLines) = 
        File.ReadAllLines filename
        |> Seq.toList
        |> SequenceHelper.splitByValue ""
        |> SequenceHelper.toTuple2
    Map.empty
    |> parseInputs inputLines 
    |> parseGates gateLines 

let getOutputs (map:Map<string,'a>) =
    map 
    |> Map.keys 
    |> Seq.filter (fun str -> str.StartsWith "z") 

let setBit number ((name:string),value)  =
    let bitValue = match value with | true -> 1L | false -> 0L //fixme use int all the way
    let pos = name.Substring 1 |> int
    number ||| (bitValue <<< pos)

let schematic = parseFile "input.txt"
let outputs = getOutputs schematic |> Seq.sort

outputs 
|> Seq.map (fun name -> (name,getGateValueMemo (schematic,name))) 
|> Seq.fold setBit 0L
|> printfn "Part 1: %d"

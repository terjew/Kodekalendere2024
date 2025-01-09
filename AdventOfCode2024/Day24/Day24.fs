open System.IO
open Utilities
open System.Collections.Generic

type Gate =
    | Input of int64
    | AND of string*string
    | OR of string*string
    | XOR of string*string
    
    static member create op a b =
        match op with
        | "AND" -> AND (a,b)
        | "OR" -> OR (a,b)
        | "XOR" -> XOR (a,b)
        | any -> failwithf "Unknown operation %s" any
    
let rec getGateValue (gates,gateId) =
    let gate = gates |> Map.find gateId
    match gate with 
    | Input input -> input
    | AND (a,b) -> getGateValue (gates, a) &&& getGateValue (gates, b) 
    | OR (a,b) -> getGateValue (gates, a) ||| getGateValue (gates, b) 
    | XOR (a,b) -> getGateValue (gates, a) ^^^ getGateValue (gates, b) 

let rec getGateValueMaybe depth gates gateId =
    if depth > 90 then None
    else
        let recurse = getGateValueMaybe (depth+1) gates
        let applyFunc f (a,b) =
            let aVal = recurse a
            let bVal = recurse b
            if aVal = None || bVal = None then None
            else Some (f aVal.Value bVal.Value)
        let gate = gates |> Map.find gateId
        match gate with 
        | Input input -> Some input
        | AND (a,b) -> (a,b) |> applyFunc (&&&) 
        | OR (a,b) -> (a,b) |> applyFunc (|||)
        | XOR (a,b) -> (a,b) |> applyFunc (^^^)

let parseInputs inputLines map =
    let parseInput map line =
        line 
        |> Regex.transformGroups2 "(\w+): ([01])" Value.asString Value.asInt64
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

let getInputsOutputs (map:Map<string,'a>) (letter:string) =
    map 
    |> Map.keys 
    |> Seq.filter (fun str -> str.StartsWith letter) 
    |> Seq.sortDescending

let setBit number ((name:string),value)  =
    let pos = name.Substring 1 |> int
    number ||| (value <<< pos)

let runSchematic schematic =
    let outputs = 
        getInputsOutputs schematic "z"
        |> Seq.map (fun name -> (name,getGateValueMaybe 0 schematic name)) 
    if outputs |> Seq.exists (fun (_,value) -> value |> Option.isNone)
        then None
    else
        outputs 
        |> Seq.map (fun (name,value) -> (name, value.Value))
        |> Seq.fold setBit 0L
        |> Some

let schematic = parseFile "input.txt"

schematic 
|> runSchematic 
|> Option.get
|> printfn "Part 1: %d"

let setInputBit name value schematic =
    schematic |> Map.map (fun n gate -> if n = name then Input value else gate)

let getBitsDiffering expected actual =
    [0..63]
    |> List.filter (fun bitNo -> (expected &&& (1L <<< bitNo)) <> (actual &&& (1L <<< bitNo)))

let runTestsForBit schematic bit =
    [
    for (x,y,c) in SequenceHelper.getPermsWithRep 3 [0L..1L] |> Seq.map SequenceHelper.toTuple3 do
        let bitnoStr = sprintf "%02i" bit
        let carryStr = sprintf "%02i" (bit-1)
        let modifiedInput = 
            schematic 
            |> setInputBit ("x" + bitnoStr) x 
            |> setInputBit ("y" + bitnoStr) y
            |> (fun s -> 
                    if bit > 0 then
                        s
                        |> setInputBit ("x" + carryStr) c 
                        |> setInputBit ("y" + carryStr) c
                    else s
                )
        let expected = (x <<< bit) + (y <<< bit) + (
                                                        if bit > 0 && c = 1 then
                                                            (1L <<< bit)
                                                        else 0L
                                                    )
        let actualMaybe = modifiedInput |> runSchematic
        match actualMaybe with 
        | None -> yield None
        | Some actual -> 
        if expected <> actual then 
            yield Some (bit,x,y,c, (getBitsDiffering expected actual))
    ]

let rec getGates depth schematic gateId =
    if depth >= 100 then [] //to avoid endless loops
    else 
        let recurse = getGates (depth+1) schematic 
        match schematic |> Map.find gateId with 
        | Input _ -> []
        | AND (a,b) -> [gateId] @ recurse a @ recurse b
        | OR (a,b) -> [gateId] @ recurse a @ recurse b
        | XOR (a,b) -> [gateId] @ recurse a @ recurse b

let swapGates schematic (aName,bName)  =
    let aGate = schematic |> Map.find aName
    let bGate = schematic |> Map.find bName

    schematic |> Map.map (fun n gate -> 
        if n = aName then bGate 
        elif n = bName then aGate
        else gate
    )
    
let testSwap schematic (zA,zB) pair =
    let swappedSchematic = pair |> (swapGates schematic)
    match runTestsForBit swappedSchematic zA = [] && runTestsForBit swappedSchematic zB = [] with
    | true -> Some pair
    | false -> None

let tryEasyFix schematic (zA,zB) =
    //find all input and intermediate nodes involved in calculating z0 and z1
    let targetsA = getGates 0 schematic (sprintf "z%02i" zA) |> Set
    let targetsB = getGates 0 schematic (sprintf "z%02i" zB) |> Set

    //pairwise, try swapping output nodes in that set until the correct value is produced for all test cases
    let diffA = Set.difference targetsA targetsB |> List.ofSeq
    let diffB = Set.difference targetsB targetsA |> List.ofSeq

    Seq.allPairs diffA diffB
    |> Seq.tryPick (testSwap schematic (zA,zB))


let tryExtensiveFix schematic (zA,zB) =
    let targetsA = getGates 0 schematic (sprintf "z%02i" zA) |> Set
    let targetsB = getGates 0 schematic (sprintf "z%02i" zB) |> Set
    let targets = Set.union targetsA targetsB |> List.ofSeq

    let possibleSwaps = SequenceHelper.getCombs2Tuple targets

    possibleSwaps
    |> Seq.tryPick (testSwap schematic (zA,zB))


let findMismatchedOutputs schematic = 
    [0..44] 
    |> Seq.collect (runTestsForBit schematic)
    |> Seq.map (fun diffMaybe -> match diffMaybe with Some (_,_,_,_,differing) -> differing | None -> failwith "asdg")
    |> Seq.distinct
    |> Seq.filter (fun l -> l.Length = 2)
    |> Seq.map SequenceHelper.toTuple2


//Stuff for visualizing the schematic:
let shape gate = 
    match gate with 
    | Input _ -> "rect"
    | AND _ -> "circle"
    | OR _ -> "triangle"
    | XOR _ -> "cds"

let connection b a =
    sprintf "%s -> %s" a b

let inputs gate name =
    match gate with 
    | Input _ -> []
    | AND (a,b) -> [a;b] |> List.map (connection name)
    | OR (a,b) -> [a;b] |> List.map (connection name)
    | XOR (a,b) -> [a;b] |> List.map (connection name)

let formatNode name gate =
    sprintf "%s [shape=%s]" name (shape gate) //print node

let formatConnection name gate =
    seq [
        yield! inputs gate name //print connections
    ]

let printGraph schematic =

    printf @"

    digraph G {
        ranksep=5
        "

    schematic |> Map.map formatConnection |> Map.values |> Seq.collect id |> List.ofSeq |> String.concat "\n\t" |> printfn "%s"

    let inputx = getInputsOutputs schematic "x"
    let inputy = getInputsOutputs schematic "y"
    let outputz = getInputsOutputs schematic "z"

    let xyChain = inputx |> Seq.zip inputy |> Seq.collect SequenceHelper.fromTuple2 |> String.concat " -> "
    let zChain = outputz |> String.concat " -> "

    let inputOutputNodes = inputx |> Seq.append inputy |> Seq.append outputz |> Set
    let internalnodes = inputOutputNodes |> Set.difference (schematic |> Map.keys |> Set)

    let isInternal name _ = 
        internalnodes |> Set.contains name

    let isCategory i _ gate = 
        let cat = 
            match gate with
            | Input _-> 0
            | AND (a,b) -> if a.StartsWith "x" || b.StartsWith "x" then 1 else 2
            | XOR _-> 1
            | OR _ -> 3
        cat = i

    let formatNodes nodes =
        nodes
        |> Map.map formatNode
        |> Map.toSeq
        |> Seq.map snd
        |> String.concat "; "

    for category in [1..3] do
        schematic 
        |> Map.filter isInternal
        |> Map.filter (isCategory category)
        |> formatNodes
        |> printfn @"
            {
            rank = same;
            edge[ style=invis];
            %s;
            }" 

    let xNodes = schematic |> Map.filter (fun name _ -> inputx |> Seq.contains name) |> formatNodes
    let yNodes = schematic |> Map.filter (fun name _ -> inputy |> Seq.contains name) |> formatNodes
    let zNodes = schematic |> Map.filter (fun name _ -> outputz |> Seq.contains name) |> formatNodes
    printfn @"
    {
    rank = min;
    rankdir = LR;
    edge[ style=invis];
    %s;
    %s;
    %s;
    }" xNodes yNodes xyChain

    printfn @"
    {
    rank = max;
    rankdir = LR;
    edge[ style=invis];
    %s;
    %s;
    }
    }" zNodes zChain

let findSwappedGates (schematic:Map<string,Gate>) =
    let schematicWithAllZeroInput = 
        schematic 
        |> Map.map (fun name gate -> if name.StartsWith "x" || name.StartsWith "y" then (Input 0) else gate)

    let mismatched = findMismatchedOutputs schematicWithAllZeroInput

    let easySwaps = 
        mismatched
        |> Seq.choose (tryEasyFix schematicWithAllZeroInput)
        |> Seq.toList

    let partiallyFixed = easySwaps |> Seq.fold swapGates schematicWithAllZeroInput

    let difficultSwaps = 
        partiallyFixed
        |> findMismatchedOutputs 
        |> Seq.toList
        |> List.choose (fun fault -> tryExtensiveFix partiallyFixed fault)

    Seq.append difficultSwaps easySwaps 

schematic |> printGraph

let swapped = findSwappedGates schematic
swapped |> Seq.fold swapGates schematic |> printGraph

swapped
|> Seq.collect SequenceHelper.fromTuple2
|> Seq.sort
|> String.concat ","
|> printfn "Part 2: %s"


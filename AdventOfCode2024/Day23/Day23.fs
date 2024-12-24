open System.IO
open Utilities

let addToTree map line =
    let (a,b) = line |> Regex.matches @"\w+" |> (fun mc -> (mc[0].Value, mc[1].Value))
    map |> Graph.addConnection (a,b) |> Graph.addConnection (b,a)

let readTree filename =
    File.ReadAllLines filename
    |> Seq.fold addToTree Map.empty

let buildCycle a (b,c) =
    [a;b;c] |> List.sort

let findLvl3Cycles tree node =
    Map.find node tree 
    |> SequenceHelper.getCombs2Tuple
    |> Seq.filter (Graph.areConnected tree)
    |> Seq.map (buildCycle node)

let tree = readTree "input.txt"

tree 
|> Map.filter (fun node _ -> node.StartsWith "t")
|> Map.map (fun node _ -> findLvl3Cycles tree node)
|> Map.values |> Seq.collect id |> Seq.distinct |> Seq.length
|> printfn "Part 1: %A"

let findMaxCliques graph = Graph.bronKerbosch Set.empty (graph |> Map.keys |> set) Set.empty graph

tree 
|> findMaxCliques
|> Seq.maxBy Seq.length
|> Seq.sort |> String.concat ","
|> printfn "Part 2: %A"

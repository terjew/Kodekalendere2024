open System.IO
open Utilities

let addConnection (a,b) map =
    map 
    |> Map.change a (fun seqMaybe -> 
        match seqMaybe with
        | Some aSeq -> Some (List.append [b] aSeq)
        | None -> Some [b]
    ) 

let buildTree map line =
    let (a,b) = line |> Regex.matches @"\w+" |> (fun mc -> (mc[0].Value, mc[1].Value))
    map |> addConnection (a,b) |> addConnection (b,a)

let readTree filename =
    File.ReadAllLines filename
    |> Seq.fold buildTree Map.empty

let areConnected tree (a,b) =
    Map.find a tree |> Seq.contains b

let buildCycle a (b,c) =
    [a;b;c] |> List.sort

let findLvl3Cycles tree node =
    Map.find node tree 
    |> SequenceHelper.getCombs2Tuple
    |> Seq.filter (areConnected tree)
    |> Seq.map (buildCycle node)

let tree = readTree "input.txt"

tree 
|> Map.filter (fun node _ -> node.StartsWith "t")
|> Map.map (fun node _ -> findLvl3Cycles tree node)
|> Map.values |> Seq.collect id |> Seq.distinct |> Seq.length
|> printfn "Part 1: %A"

//https://fssnip.net/jg/title/BronKerbosch-maximal-cliques-algorithm
let rec bronKerbosch R P X graph =
    let neighbors vertex =
        graph
        |> Map.find vertex
        |> set
    seq {
        if (Set.isEmpty P) && (Set.isEmpty X) then
          yield (Set.toSeq R)
        let vPX =
            Seq.unfold
                (function
                | (v::tailP as P, currX) ->
                    let newX = Set.add v currX
                    Some((v, set <| P, currX), (tailP, newX))
                | ([], _) -> None)
                (P |> Set.toList, X)
        for (v, P, X) in vPX do
            let n = neighbors v
            yield! bronKerbosch (Set.add v R) (Set.intersect P n) (Set.intersect X n) graph
    }

let findMaxCliques graph = bronKerbosch Set.empty (graph |> Map.keys |> set) Set.empty graph

tree 
|> findMaxCliques
|> Seq.maxBy (fun s -> Seq.length s)
|> Seq.sort |> String.concat ","
|> printfn "Part 2: %A"
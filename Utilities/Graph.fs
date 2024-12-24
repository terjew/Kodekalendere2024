module Graph

open System.Collections.Generic

//https://red-green-rewrite.github.io/2018/03/10/Saving-New-York-with-F-Bloxorz-and-John-McClane/
let bfs idof fanout node=
    let queue = Queue<'c>([|node|])
    let visited = HashSet()

    // DSL
    let enqueue = queue.Enqueue
    let dequeue = queue.Dequeue
    let empty () = queue.Count = 0
    let mark = idof >> visited.Add >> ignore
    let test = idof >> visited.Contains >> not

    // algorithm
    seq {
        while not (empty ()) do
            let current = dequeue ()
            mark current
            yield current
            current |> fanout |> Seq.filter test |> Seq.iter enqueue
    }

//https://red-green-rewrite.github.io/2018/03/10/Saving-New-York-with-F-Bloxorz-and-John-McClane/
let dfs idof fanout node=
    let stack = Stack<'c>([|node|])
    let visited = HashSet()

    // DSL
    let enqueue = stack.Push
    let dequeue = stack.Pop
    let empty () = stack.Count = 0
    let mark = idof >> visited.Add >> ignore
    let test = idof >> visited.Contains >> not

    // algorithm
    seq {
        while not (empty ()) do
            let current = dequeue ()
            mark current
            yield current
            current |> fanout |> Seq.filter test |> Seq.iter enqueue
    }

//let getChildren node = Map.find node tree

//bfs id getChildren "vc" |> Seq.toList |> printfn "%A"
//dfs id getChildren "vc" |> Seq.toList |> printfn "%A"

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

let addConnection (a,b) map =
    map 
    |> Map.change a (fun seqMaybe -> 
        match seqMaybe with
        | Some aSeq -> Some (List.append [b] aSeq)
        | None -> Some [b]
    ) 

let areConnected tree (a,b) =
    Map.find a tree |> Seq.contains b



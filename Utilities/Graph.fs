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



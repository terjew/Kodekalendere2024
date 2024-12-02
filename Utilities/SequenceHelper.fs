namespace Utilities

module SequenceHelper = 
    
    let rec pairs list = 
        seq {  
            match list with 
            | head::tail -> for element in tail do
                                yield head, element
                            yield! pairs tail
            | _ -> () 
        }

    let duplicateIndices indicesToDuplicate (sequence : seq<'T>) =
        sequence 
        |> Seq.mapi (fun i r -> (i,r))
        |> Seq.collect (fun (i,r) -> match Set.contains i indicesToDuplicate with
                                        | true -> seq{r; r}
                                        | false -> seq{r} )

    //https://www.fssnip.net/nr/title/Split-a-list-using-a-separator
    /// Split a list into chunks using the specified separator
    /// This takes a list and returns a list of lists (chunks)
    /// that represent individual groups, separated by the given
    /// separator 'v'
    let splitByValue v list =
      let yieldRevNonEmpty list = 
        if list = [] then []
        else [List.rev list]

      let rec loop groupSoFar list = seq { 
        match list with
        | [] -> yield! yieldRevNonEmpty groupSoFar
        | head::tail when head = v ->
            yield! yieldRevNonEmpty groupSoFar
            yield! loop [] tail
        | head::tail ->
            yield! loop (head::groupSoFar) tail }
      loop [] list |> List.ofSeq

    /// Split a list into chunks using the specified separator function
    /// This takes a list and returns a list of lists (chunks)
    /// that represent individual groups, separated by the given
    /// separator function 'func'
    let splitByFunc func list =
      let yieldRevNonEmpty list = 
        if list = [] then []
        else [List.rev list]

      let rec loop groupSoFar list = seq { 
        match list with
        | [] -> yield! yieldRevNonEmpty groupSoFar
        | head::tail when func(head) ->
            yield! yieldRevNonEmpty groupSoFar
            yield! loop [] tail
        | head::tail ->
            yield! loop (head::groupSoFar) tail }
      loop [] list |> List.ofSeq

    //from https://stackoverflow.com/questions/4495597/combinations-and-permutations-in-f

    let rec combinations acc size set = seq {
      match size, set with 
      | n, x::xs -> 
          if n > 0 then yield! combinations (x::acc) (n - 1) xs
          if n >= 0 then yield! combinations acc n xs 
      | 0, [] -> yield acc 
      | _, [] -> () }

    /// Rotates a list by one place forward.
    let rotate lst =
        List.tail lst @ [List.head lst]

    /// Gets all rotations of a list.
    let getRotations lst =
        let rec getAll lst i = if i = 0 then [] else lst :: (getAll (rotate lst) (i - 1))
        getAll lst (List.length lst)

    /// Gets all permutations (without repetition) of specified length from a list.
    let rec getPerms n lst = 
        match n, lst with
        | 0, _ -> seq [[]]
        | _, [] -> seq []
        | k, _ -> lst |> getRotations |> Seq.collect (fun r -> Seq.map ((@) [List.head r]) (getPerms (k - 1) (List.tail r)))

    /// Gets all permutations (with repetition) of specified length from a list.
    let rec getPermsWithRep n lst = 
        match n, lst with
        | 0, _ -> seq [[]]
        | _, [] -> seq []
        | k, _ -> lst |> Seq.collect (fun x -> Seq.map ((@) [x]) (getPermsWithRep (k - 1) lst))
        // equivalent: | k, _ -> lst |> getRotations |> Seq.collect (fun r -> List.map ((@) [List.head r]) (getPermsWithRep (k - 1) r))

    /// Gets all combinations (without repetition) of specified length from a list.
    let rec getCombs n lst = 
        match n, lst with
        | 0, _ -> seq [[]]
        | _, [] -> seq []
        | k, (x :: xs) -> Seq.append (Seq.map ((@) [x]) (getCombs (k - 1) xs)) (getCombs k xs)

    /// Gets all combinations (with repetition) of specified length from a list.
    let rec getCombsWithRep n lst = 
        match n, lst with
        | 0, _ -> seq [[]]
        | _, [] -> seq []
        | k, (x :: xs) -> Seq.append (Seq.map ((@) [x]) (getCombsWithRep (k - 1) lst)) (getCombsWithRep k xs)
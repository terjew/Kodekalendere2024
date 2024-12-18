namespace Utilities
open System.Text.RegularExpressions

module Value = 
    type ValueUnion =
        | MatchValue of m : Match
        | GroupValue of g : Group
    
    type CollectionUnion = 
        | MatchCollectionValue of m : MatchCollection
        | GroupCollectionValue of g : GroupCollection

    let getValue union =
        match union with 
        | MatchValue m -> m.Value
        | GroupValue g -> g.Value

    let getElements collection = 
        match collection with 
        | MatchCollectionValue m -> m |> Seq.map MatchValue
        | GroupCollectionValue g -> g |> Seq.skip 1 |> Seq.map GroupValue

    let asInt64 (union : ValueUnion) =
        getValue union |> int64

    let asInt (union : ValueUnion) =
        getValue union |> int

    let asFloat (union : ValueUnion) =
        getValue union |> float



module ValueCollection =
    let asIntVector2 collection =
        Value.getElements collection 
        |> Seq.map Value.getValue
        |> Seq.map int
        |> Seq.toList
        |> (fun list -> (list[0], list[1]))

module Regex =

    let isMatchPattern pattern input =
        Regex.IsMatch(input, pattern)
    
    let matchPattern pattern input =
        Regex.Match(input, pattern)

    let matches pattern input =
        Regex.Matches(input, pattern)

    let matchesWithOptions pattern options input=
        Regex.Matches(input, pattern, options)

    let transformWith transform collection =
        collection |> Value.getElements |> Seq.map transform

    let transformMatches pattern transform input =
        Regex.Matches(input, pattern) |> Value.MatchCollectionValue |> transformWith transform

    let transformGroups pattern transform input =
        Regex.Match(input, pattern).Groups |> Value.GroupCollectionValue |> transformWith transform

    let combineMatches pattern transform input =
        Regex.Matches(input, pattern) |> Value.MatchCollectionValue |> transform

    let combineGroups pattern transform input  =
        Regex.Match(input, pattern).Groups |> Value.GroupCollectionValue |> transform


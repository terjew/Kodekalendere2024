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

    let asString (union : ValueUnion) =
        getValue union |> id

    let asBool (union : ValueUnion) =
        getValue union |> String.parseBool

module ValueCollection =
    let asIntVector2 collection =
        Value.getElements collection 
        |> Seq.map Value.getValue
        |> Seq.map int
        |> Seq.toList
        |> (fun list -> (list[0], list[1]))

module Regex =

    let isMatchPattern (pattern:string) (input:string) =
        Regex.IsMatch(input, pattern)
    
    let matchPattern pattern input =
        Regex.Match(input, pattern)

    let matches pattern input =
        Regex.Matches(input, pattern)

    let matchesWithOptions pattern options input=
        Regex.Matches(input, pattern, options)

    let transformWith transform collection =
        collection |> Value.getElements |> Seq.map transform

    let transformWith2 transform1 transform2 collection =
        let values = collection |> Value.getElements |> Seq.toList
        (transform1 values[0], transform2 values[1])

    let transformWith3 transform1 transform2 transform3 collection =
        let values = collection |> Value.getElements |> Seq.toList
        (transform1 values[0], transform2 values[1], transform3 values[2])

    let transformWith4 transform1 transform2 transform3 transform4 collection =
        let values = collection |> Value.getElements |> Seq.toList
        (transform1 values[0], transform2 values[1], transform3 values[2], transform4 values[3])

    let transformMatch pattern transform input =
        Regex.Match(input, pattern) |> Value.MatchValue |> transform

    let transformMatches pattern transform input =
        Regex.Matches(input, pattern) |> Value.MatchCollectionValue |> transformWith transform

    let transformGroups pattern transform input =
        Regex.Match(input, pattern).Groups |> Value.GroupCollectionValue |> transformWith transform

    let transformGroups2 pattern transform1 transform2 input =
        Regex.Match(input, pattern).Groups |> Value.GroupCollectionValue |> transformWith2 transform1 transform2

    let transformGroups3 pattern transform1 transform2 transform3 input =
        Regex.Match(input, pattern).Groups |> Value.GroupCollectionValue |> transformWith3 transform1 transform2 transform3

    let transformGroups4 pattern transform1 transform2 transform3 transform4 input =
        Regex.Match(input, pattern).Groups |> Value.GroupCollectionValue |> transformWith4 transform1 transform2 transform3 transform4

    let combineMatches pattern transform input =
        Regex.Matches(input, pattern) |> Value.MatchCollectionValue |> transform

    let combineGroups pattern transform input  =
        Regex.Match(input, pattern).Groups |> Value.GroupCollectionValue |> transform


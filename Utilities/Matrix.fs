﻿namespace Utilities
open Pastel
open System.IO

type Matrix = 
    {
        SizeX : int
        SizeY : int
        Data : string[]
    }
    override this.ToString (): string = 
        String.concat "\n" this.Data

module Matrix =

    //Creation
    let create sizeX sizeY (initial : char) = 
        {
            SizeX = sizeX;
            SizeY = sizeY;
            Data = Array.create sizeY (Array.create sizeX initial |> System.String)
        }

    let fromStringArray (data : string[]) =
        let sizeX = data[0].Length
        let sizeY = data.Length
        {
            SizeX = sizeX;
            SizeY = sizeY;
            Data = data
        }

    let fromString (str : string) =
        str |> String.splitWithAny "\r\n" |> fromStringArray

    let fromStrings strings = 
        let data = Seq.toArray strings
        fromStringArray data

    let fromFile filename =
        File.ReadAllLines(filename)
        |> fromStrings

    //Equality
    let equal matrix matrix2 = 
        matrix.SizeX = matrix2.SizeX &&
        matrix.SizeY = matrix2.SizeY &&
        matrix.Data = matrix2.Data
       
    //Lookups
    let isInside matrix (x,y) =
        x >= 0 && x < matrix.SizeX && y >= 0 && y < matrix.SizeY

    let get matrix (x,y) =
        matrix.Data[y][x]

    let tryGet matrix pos =
        match isInside matrix pos with
        | true -> Some (get matrix pos)
        | false -> None
    
    let getWithOverflow matrix (x,y) = 
        get matrix ((x + matrix.SizeX) % matrix.SizeX, (y + matrix.SizeY) % matrix.SizeY)

    let row matrix y =
        matrix.Data[y]

    let column matrix x =
        matrix.Data 
        |> Seq.map (fun str -> str[x])
        |> Seq.toArray
        |> System.String

    //Iteration
    let rowIndices matrix = 
        seq {0 .. matrix.SizeY - 1}

    let columnIndices matrix = 
        seq {0 .. matrix.SizeX - 1}

    let rows matrix =
        seq {
            for row in rowIndices matrix do yield matrix.Data[row]
        }

    let allPos matrix =
        seq {
            for y in 0 .. matrix.SizeY - 1 do 
                for x in 0 .. matrix.SizeX - 1 do yield (x,y)
        }

    let map f matrix =
        allPos matrix
        |> Seq.map (fun pos -> f matrix pos (get matrix pos))


    let find value matrix =
        matrix 
        |> allPos 
        |> Seq.find (fun pos -> value = get matrix pos)

    let findAll value matrix =
        matrix 
        |> allPos 
        |> Seq.filter (fun pos -> value = get matrix pos)

    let findAllOf seq matrix =
        matrix 
        |> allPos 
        |> Seq.filter (fun pos -> seq |> Seq.contains (get matrix pos))

    let findRowsMatching pattern matrix =
        rowIndices matrix
        |> Seq.filter (fun i -> row matrix i |> Regex.isMatchPattern pattern)
        |> Set.ofSeq

    let findColumnsMatching pattern matrix =
        columnIndices matrix
        |> Seq.filter (fun i -> column matrix i |> Regex.isMatchPattern pattern)
        |> Set.ofSeq

    //Neighbor cells
    let neighbor matrix direction pos =
        Vector.neighbor pos direction |> get matrix

    let neighborCoordsDiagonal matrix (x,y)  = 
        let minx = if x = 0 then x else x - 1
        let miny = if y = 0 then y else y - 1
        let maxx = if x = matrix.SizeX - 1 then x else x + 1
        let maxy = if y = matrix.SizeY - 1 then y else y + 1
        seq {
        for iy in miny .. maxy do
            for ix in minx .. maxx do
                if not (ix = x && iy = y) then yield (ix, iy)
        }

    let neighborsDiagonal matrix pos =
        neighborCoordsDiagonal matrix pos
        |> Seq.map (get matrix)

    let neighborCoordsWithDirection matrix pos =
        Direction.cardinal 
        |> Seq.map (Vector.neighborWithDirection pos)
        |> Map.ofSeq
        |> Map.filter (fun _ pos -> isInside matrix pos)

    let neighborsWithDirection matrix pos =
        neighborCoordsWithDirection matrix pos
        |> Map.map (fun _ pos -> get matrix pos)

    let neighborsWithValues matrix pos =
        neighborCoordsWithDirection matrix pos
        |> Seq.map (fun kvp -> (kvp.Value, get matrix kvp.Value))
        |> Map.ofSeq

    let allWithinManhattanDistance matrix max pos=
        matrix |> allPos |> Seq.filter (fun other -> max >= Vector.manhattanDistance pos other )

    //Transforming
    let withValueAt (x,y) value matrix =
        let updatedLine = matrix.Data[y] |> String.mapi(fun i char -> if i=x then value else char)
        let updatedArray = matrix.Data |> Array.mapi(fun i line -> if i=y then updatedLine else line)
        {
            SizeX = matrix.SizeX;
            SizeY = matrix.SizeY;
            Data = updatedArray
        }

    let transformBy func matrix =
        let transformRow matrix y func =
            seq { 
                for x in 0 .. matrix.SizeX - 1 do yield (func matrix (x,y) )
            } |> Array.ofSeq |> System.String

        let transformedRows = seq {
            for y in 0 .. matrix.SizeY - 1 do yield transformRow matrix y func
        }
        {
            SizeX = matrix.SizeX
            SizeY = matrix.SizeY
            Data = Array.ofSeq transformedRows
        }


    let withDuplicatedRows matrix rowsToDuplicate =
        let data = matrix 
                    |> rows 
                    |> SequenceHelper.duplicateIndices rowsToDuplicate
                    
        {
            SizeX = matrix.SizeX
            SizeY = matrix.SizeY + rowsToDuplicate.Count
            Data = Array.ofSeq data
        }

    let withDuplicatedColumns matrix columnsToDuplicate =
        let data = matrix 
                    |> rows 
                    |> Seq.map (fun row -> row |> SequenceHelper.duplicateIndices columnsToDuplicate |> Seq.toArray |> System.String)
        {
            SizeX = matrix.SizeX + columnsToDuplicate.Count
            SizeY = matrix.SizeY 
            Data = Array.ofSeq data
        }

    let gridOf matrix count =
        let repeatedCols = matrix |> rows |> Seq.map (fun row -> String.replicate count row)
        let repeatedRows = seq{1..count} |> Seq.collect (fun _ -> repeatedCols)
        fromStrings repeatedRows

    let maxDim matrix =
        max matrix.SizeX matrix.SizeY

    //Traversing

    let solveAStar start goal matrix =
        let h pos next = 
            (float (Vector.manhattanDistance pos next))
        let g _ _ = 
            1.0
        let neighbors pos = 
            neighborsWithValues matrix pos
            |> Seq.filter (fun kvp -> '#' <> kvp.Value)
            |> Seq.map (fun kvp -> kvp.Key)

        AStar.search start goal {
            fCost = h
            gCost = g
            neighbours = neighbors
            maxIterations = None
        }

    let rec solveAll start goal matrix moves =
        if start = goal then [moves]
        else
        [
            let neighbors = 
                neighborsWithValues matrix start
                |> Seq.filter (fun kvp -> '#' <> kvp.Value)
                |> Seq.filter (fun kvp -> not <| List.contains kvp.Key moves)
                |> Seq.map (fun kvp -> kvp.Key)
            for n in neighbors do
                yield! solveAll n goal matrix (n::moves)
        ]

    //Printing

    let defaultColormap char =
        let color = 
            match char with 
            |'#' -> System.Drawing.Color.DarkRed
            |'@' -> System.Drawing.Color.Orange
            |'*' -> System.Drawing.Color.Green
            |'.' -> System.Drawing.ColorTranslator.FromHtml("#222222")
            | _  -> System.Drawing.Color.Cyan
        (char,color)

    let print matrix =
        printfn "Matrix[%d,%d]" matrix.SizeX matrix.SizeY
        printfn "" 
        for y in 0 .. matrix.SizeY - 1 do
            printfn "%s" matrix.Data[y]
        printfn "" 

    let draw char matrix pos =
        matrix |> withValueAt pos char

    let drawPath char matrix positions = 
        positions |> Seq.fold (draw char) matrix 

    let printColored colormap matrix =
        matrix
        |> rows 
        |> Seq.map (String.printColor colormap)
        |> Seq.toList
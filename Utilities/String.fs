namespace Utilities
open Pastel

module Any =
    let print obj =
        printfn "%A" obj

module String = 
    let splitWithAny (separators : string) (str : string) =
        str.Split(separators |> Seq.toArray, System.StringSplitOptions.RemoveEmptyEntries)



    let printColor colormap (str:string) =   
        let applyColor ((char:char),(color:System.Drawing.Color)) =
            (string char).Pastel(color)
        str 
        |> Seq.map colormap 
        |> Seq.map applyColor
        |> String.concat "" 
        |> printfn "%s" 

    let parseBool string =
        match string with
        | ("True"|"true"|"1") -> true
        | ("False"|"false"|"0") -> false
        | any -> failwithf "Unknown bool format: %s" any

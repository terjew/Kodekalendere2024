namespace Utilities

module Any =
    let print obj =
        printfn "%A" obj

module String = 
    let splitWithAny (separators : string) (str : string) =
        str.Split(separators |> Seq.toArray, System.StringSplitOptions.RemoveEmptyEntries)

    let printColor colormap (string:string) =   
        string 
        |> Seq.map colormap 
        |> String.concat "" 
        |> printfn "%s" 

    let parseBool string =
        match string with
        | ("True"|"true"|"1") -> true
        | ("False"|"false"|"0") -> false
        | any -> failwithf "Unknown bool format: %s" any

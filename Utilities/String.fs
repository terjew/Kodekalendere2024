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


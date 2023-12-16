<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

let swap (pl: _ array2d) iS iE j = 
    let t = pl[iS, j]
    pl[iS, j] <- pl[iE, j]
    pl[iE, j] <- t 
    
let rec tiltUp pl iS iE j =
    let (iL, jL) = pl |> Array2D.length1, pl |> Array2D.length2
    let nextCell () = tiltUp pl (iS + 1) (iS + 1) j
    match iS = iL - 1, iE = iL with 
    | true, _ -> ()
    | _, true -> nextCell ()
    | _ -> 
        match pl[iS, j] with 
        | 'O' | '#' -> nextCell ()
        | '.' -> 
            match pl[iE, j] with 
            | '#' -> nextCell ()
            | '.' -> tiltUp pl iS (iE + 1) j
            | 'O' -> 
                swap pl iS iE j 
                tiltUp pl (iS + 1) (iE + 1) j
            | _ -> failwith "Error"
        | _ -> failwith "Error"
    

let tilt pl =
    [for j in 0..(pl |> Array2D.length2) - 1 do tiltUp pl 0 0 j] |> ignore
    pl

let calcLoad (pl: _ array2d) = 
    pl |> Array2D.mapi (fun i j c -> if c = 'O' then (pl |> Array2D.length1) - i else 0) |> List.ofSeq |> List.sum
    
let main() =
    let input = 14 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) 
    let platform = input |> List.map List.ofSeq |> array2D 
    
    platform |> tilt |> Dump
    
    
    
main()
<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

type State = |Edge|In|Out

let parseRow (row: string) = 
    let parts = row.Split(" ")
    (parts[0], parts[1] |> int, parts[2])
    
let createMap rows = 
    let getEnd start dir size = 
        let (i, j) = start 
        match dir with 
        | "U" ->  (i - size, j) 
        | "D" -> (i + size, j)
        | "L" -> (i, j - size) 
        | "R" -> (i, j + size) 
        | _ -> failwith "Error"
        
    let (coords, _) =
        rows |> List.mapFold (fun coords (dir, size, color) -> 
            let startCoords = getEnd coords dir 1
            let endCoords = getEnd coords dir size
            ((startCoords, endCoords, color), endCoords)) (0, 0)
    let minI = coords |> List.map (fun ((i1, _), (i2, _), _) -> Math.Min(i1, i2)) |> List.min
    let minJ = coords |> List.map (fun ((_, j1), (_, j2), _) -> Math.Min(j1, j2)) |> List.min
    let maxI = coords |> List.map (fun ((i1, _), (i2, _), _) -> Math.Max(i1, i2)) |> List.max
    let maxJ = coords |> List.map (fun ((_, j1), (_, j2), _) -> Math.Max(j1, j2)) |> List.max
    
    let map = Array2D.create (maxI - minI + 1) (maxJ - minJ + 1) '.'
    coords |> List.map (fun ((i1, j1), (i2, j2), color) -> (i1 - minI, j1 - minJ), (i2 - minI, j2 - minJ), color)
           |> List.iter (fun ((i1, j1), (i2, j2), _) -> for i in Math.Min(i1, i2)..Math.Max(i1, i2) do for j in Math.Min(j1, j2)..Math.Max(j1, j2) do map[i, j] <- '#')
    
    map

let rec paint (map: char array2d) tiles  = 
    let l1, l2 = (map |> Array2D.length1), (map |> Array2D.length2)
    let inline isWithinMap i j = 0 <= i && i < l1 && 0 <= j && j < l2
    
    match tiles with 
    | [] -> ()
    | (i, j)::ts ->    
        match map[i, j] with 
        | '#' -> paint map ts 
        | '.' -> 
            map[i,j] <- '#'
            paint map (([for i' in i - 1..i + 1 do for j' in  j - 1..j + 1 do (i', j')] |> List.filter (fun t -> t ||> isWithinMap)) @ ts)
        | _ -> failwith "Error"
    
let paintMap (map: char array2d) =     
    let (startI, _, _) = 
        map |> Array2D.mapi (fun i j c -> (i, j, c)) 
        |> Seq.cast<int * int * char> |> Seq.filter (fun (_, _, c) -> c = '#') |> Seq.minBy (fun (i, _, _) -> i) 

    let startJ = map[startI, 0..] |> List.ofSeq |> List.findIndex (fun c -> c = '#')
    
    paint map [startI+1, startJ + 1] // We assume that this cell is .
    
let main() =
    let input = 18 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) 
    let map = input |> List.map parseRow |> createMap
    
    map |> paintMap 
    map |> Seq.cast |> Seq.filter (fun x -> x = '#') |> Seq.length |> Dump
    
main()
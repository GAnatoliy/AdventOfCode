<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

type Direction = |N | E | S | W
    
let nextTiles (grid: char array2d) (i, j, dir) =
    // TODO: consider to simplify/generate table.
    match dir, grid[i, j] with 
    | N, '.' -> [(i - 1, j, N)]
    | N, '-' -> [(i, j - 1, W); (i, j + 1, E)]
    | N, '|' -> [(i - 1, j, N)]
    | N, '/' -> [(i, j + 1, E)]
    | N, '\\' -> [(i, j - 1, W)]
    | E, '.' -> [(i, j + 1, E)]
    | E, '-' -> [(i, j + 1, E)]
    | E, '|' -> [(i - 1, j, N); (i + 1, j, S)] 
    | E, '/' -> [(i - 1, j, N)]
    | E, '\\' -> [(i + 1, j, S)]
    | S, '.' -> [(i + 1, j, S)]
    | S, '-' -> [(i, j - 1, W); (i, j + 1, E)]
    | S, '|' -> [(i + 1, j, S)]
    | S, '/' -> [(i, j - 1, W)]
    | S, '\\' -> [(i, j + 1, E)]
    | W, '.' -> [(i, j - 1, W)]
    | W, '-' -> [(i, j - 1, W)]
    | W, '|' -> [(i - 1, j, N); (i + 1, j, S)] 
    | W, '/' -> [(i + 1, j, S)]
    | W, '\\' -> [(i - 1, j, N)]
    | _ -> failwith "Error" 
    
let inline charToInt c = int c - int '0'
let draw (grid: char array2d) tile = 
    let (i, j, dir) = tile 
    grid[i, j] <- 
        match grid[i, j] with 
        | '^' | '>' | 'v' | '<' -> 2 |> Convert.ToChar
        | c -> if c |> Char.IsDigit then (c |> string |> int) + 1 |> Convert.ToChar else match dir with | N -> '^' | E -> '>' | S -> 'v' | W -> '<'
    
let isOutOfGrid (grid: char array2d) (i, j, _) = i < 0 || i >= (grid |> Array2D.length1) || j < 0 || j >= (grid |> Array2D.length2)
let isAlreadyPassed path tile = path |> List.exists (fun x -> x = tile)  
    
let rec runBeam (grid: char array2d) path tiles  = 
    match tiles with 
    | [] -> path
    | x::xs -> 
        runBeam grid (x::path) <|
            match nextTiles grid x |> List.filter (isOutOfGrid grid >> not) |> List.filter (isAlreadyPassed path >> not) with 
            | [] -> xs
            | [t] -> t::xs 
            | t1::t2::_ -> t1::t2::xs 
    
    
let main() =
    let input = 16 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) 
    let grid = input |> List.map Seq.toList |> array2D |> Dump
    
    let path = runBeam grid [] [(0, 0, E)] 
    path |> List.iter (draw grid) 
    grid |> Dump
    
    path |> List.map (fun (i, j, _) -> (i, j)) |> List.distinct |> List.length |> Dump |> ignore
    
    
    
    
    
main()
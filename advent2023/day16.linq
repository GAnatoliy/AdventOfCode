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
    
let isOutOfGrid (grid: char array2d) (i, j, _) = i < 0 || i >= (grid |> Array2D.length1) || j < 0 || j >= (grid |> Array2D.length2)
let isAlreadyPassed path tile = path |> Set.contains tile
    
let rec runBeam (grid: char array2d) path tiles  = 
    match tiles with 
    | [] -> path
    | x::xs -> 
        runBeam grid (path |> Set.add x) <|
            match nextTiles grid x |> List.filter (isOutOfGrid grid >> not) |> List.filter (isAlreadyPassed path >> not) with 
            | [] -> xs
            | [t] -> t::xs 
            | t1::t2::_ -> t1::t2::xs 
    
let getInitTiles (grid: _ array2d) = 
    let (iL, jL) = grid |> Array2D.length1, grid |> Array2D.length2
    [for j in 0..(jL - 1) do [(0, j, S); (iL - 1, j, N)]] @ [for i in 0..(iL - 1) do [(i, 0, E); (i, jL - 1, W)]] |> List.collect id
    
let main() =
    let input = 16 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) 
    let grid = input |> List.map Seq.toList |> array2D 
    
    runBeam grid Set.empty [(0, 0, E)] |> Set.map (fun (i, j, _) -> (i, j)) |> Set.count |> Dump |> ignore
    getInitTiles grid |> List.map (fun t -> runBeam grid Set.empty [t] |> Set.map (fun (i, j, _) -> (i, j)) |> Set.count) |> List.max |> Dump |> ignore
    
main()
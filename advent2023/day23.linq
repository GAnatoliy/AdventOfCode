<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\GAnat\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

type Direction = |N |E |S |W

let isOutOfGrid (grid: _ array2d) (i, j, _) = i < 0 || i > (grid |> Array2D.length1) - 1 || j < 0 || j > (grid |> Array2D.length2) - 1
let canPath (grid: char array2d) (i, j, dir) = 
    let passableSlope = match dir with | N -> '^' | E -> '>' | S -> 'v' | W -> '<'
    [passableSlope; '.'] |> List.contains grid[i, j]
    
let getNextTiles grid (i, j) path = 
    [(-1, 0, N); (0, 1, E); (1, 0, S); (0, -1, W)] 
        |> List.map (fun (i', j', d) -> (i + i', j + j', d))
        |> List.filter (isOutOfGrid grid >> not)
        |> List.filter (canPath grid)
        |> List.map (fun (i', j', _) -> (i', j'))
        |> List.filter (fun t -> path |> List.contains t |> not) 

let rec findMaxPathLength grid tile path = 
    let tiles = getNextTiles grid tile path
    match tiles with 
    | [] -> path |> List.length
    | ts -> ts |> List.map (fun t -> findMaxPathLength grid t (tile::path)) |> List.max    
    
let main() = 
    let input = 23 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) 
    let grid = input |> List.map List.ofSeq |> array2D 
    
    let start = 0, grid[0,0..] |> Array.findIndex (fun c -> c = '.') |> Dump
    
    findMaxPathLength grid start [] |> Dump |> ignore
    
main()
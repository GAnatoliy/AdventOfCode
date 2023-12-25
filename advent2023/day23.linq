<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\GAnat\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

type Direction = |N |E |S |W

//let isOutOfGrid (grid: _ array2d) (i, j, _) = i < 0 || i > (grid |> Array2D.length1) - 1 || j < 0 || j > (grid |> Array2D.length2) - 1
//let canPath (grid: char array2d) (i, j, dir) = 
//    let passableSlope = match dir with | N -> '^' | E -> '>' | S -> 'v' | W -> '<'
//    [passableSlope; '.'] |> List.contains grid[i, j]
//    
//let getNextTiles grid (i, j) path = 
//    [(-1, 0, N); (0, 1, E); (1, 0, S); (0, -1, W)] 
//        |> List.map (fun (i', j', d) -> (i + i', j + j', d))
//        |> List.filter (isOutOfGrid grid >> not)
//        |> List.filter (canPath grid)
//        |> List.map (fun (i', j', _) -> (i', j'))
//        |> List.filter (fun t -> path |> List.contains t |> not) 

//let rec findMaxPathLength grid tile path = 
//    let tiles = getNextTiles grid tile path
//    match tiles with 
//    | [] -> path |> List.length
//    | ts -> ts |> List.map (fun t -> findMaxPathLength grid t (tile::path)) |> List.max    
    
//let isCrossroad grid (i, j) = 
//    [(-1, 0, N); (0, 1, E); (1, 0, S); (0, -1, W)] 
//        |> List.map (fun (i', j', d) -> (i + i', j + j', d))
//        |> List.filter (isOutOfGrid grid >> not)
//        |> List.filter (fun (i', j', _) -> grid[i', j'] = '.' || grid[i', j'] = '<' || grid[i', j'] = '>' || grid[i', j'] = '^' || grid[i', j'] = 'v')
//        |> List.length > 2
    
let isWithinGrid (grid: _ array2d) (i, j) =  0 <= i && i < (grid |> Array2D.length1) && 0 <= j && j < (grid |> Array2D.length2)
let canPass (grid: char array2d) (i, j) = 
    ['^'; '>'; 'v'; '<'; '.'] |> List.contains grid[i, j]
    
let getAdjoiningPassableTiles grid (i, j) = 
    [(-1, 0); (0, 1); (1, 0); (0, -1)] 
        |> List.map (fun (i', j') -> i' + i, j' + j) 
        |> List.filter (isWithinGrid grid)
        |> List.filter (canPass grid)
//let isCrossroad grid tile  = 
//    getAdjoiningPassableTiles grid tile |> List.length > 2
//let isDeadEnd grid tile =
//    getAdjoiningPassableTiles grid tile |> List.length < 2
        
let rec findConnection grid tile path = 
    let tiles = getAdjoiningPassableTiles grid tile |> List.filter (fun t -> path |> List.contains t |> not) 
    match tiles with 
    | [t] -> findConnection grid t (tile::path)
    | _ -> (tile, path.Length)
    
let findConnections grid tile = 
    let tiles = getAdjoiningPassableTiles grid tile
    tiles |> List.map (fun t -> findConnection grid t [tile]) 
     
let buildGraph grid = 
    let isVertex grid tile  = getAdjoiningPassableTiles grid tile |> List.length <> 2
    
    let addEdge (graph: Dictionary<_,_>) v1 v2 length = 
        if not (graph.ContainsKey v1) then graph.Add(v1, [])
        if not (graph.ContainsKey v2) then graph.Add(v2, [])
        graph[v1] <- (v2, length)::graph[v1]
        
    let graph = new Dictionary<(int * int), ((int * int) * int) list> ()    
    let l1, l2 = grid |> Array2D.length1, grid |> Array2D.length2
    let crossroads = 
        [for i in 0..l1 - 1 do for j in 0..l2 - 1  do (i, j)] 
        |> List.filter (fun (i, j) -> grid[i, j] <> '#')
        |> List.filter (isVertex grid) 
    
    crossroads |> List.iter (fun t -> findConnections grid t |> List.iter (fun (t', length) -> addEdge graph t t' length))
    graph
    
//let mutable max = 0
let mutable finalPath = []
let rec findMaxPathLength (graph: Dictionary<_, _>) (v, (l: int)) endV path = 
    let vertexes = graph[v] |> List.filter (fun (v', _) -> path |> List.exists (fun (v'', _) -> v' = v'') |> not) 
    match vertexes with 
    | [] ->
        match v = endV with
        | true -> 
            let length = l + (path |> List.map (fun (_, l) -> l) |> List.sum)
            let finalPathLength = (finalPath |> List.map (fun (_, l) -> l) |> List.sum)
            if length > finalPathLength then 
                finalPath <- (v,l)::path
            //(path,  path |> List.map (fun (_, l) -> l) |> List.sum) |> Dump |> ignore
            length 
        | false -> 0
    | vs -> vs |> List.map (fun v' -> findMaxPathLength graph v' endV ((v, l)::path)) |> List.max   
    
let createFile path (content: string) =     
    use file = File.Create(path)
    let bytes = System.Text.Encoding.UTF8.GetBytes(content)
    do file.Write (bytes)
    
let displayGraph (graph: Dictionary<_, _>) = 
    let isInFinalPath v1 v2 = finalPath |> List.rev |> List.map (fun (v, _) -> v) |> List.windowed 2 |> List.exists (fun edge -> edge = [v1; v2])
    let getColor v1 v2 = if isInFinalPath v1 v2 then "red" else "black"
    let vertexes = graph.Keys |> List.ofSeq |> List.map (fun v -> $"\"{v}\";")
    let edges = graph.Keys |> List.ofSeq |> List.map (fun v1 -> graph[v1] |> List.map (fun (v2, l) -> $"\"{v1}\" -> \"{v2}\" [label=\"{l}\", color={getColor v1 v2}];")) |> List.collect id
    let content = String.Join(Environment.NewLine, "digraph G {"::vertexes @ edges @ ["}"])
    createFile "c://Temp//gr.dot" content 
    
let main() = 
    let input = 23 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) 
    let grid = input |> List.map List.ofSeq |> array2D  |> Dump
    
    let start = 0, grid[0,0..] |> Array.findIndex (fun c -> c = '.')
    let endRow = (grid |> Array2D.length1) - 1
    let endTile = endRow, grid[endRow,0..] |> Array.findIndex (fun c -> c = '.') 
    
    endTile |> Dump
    //findMaxPathLength grid start [] |> Dump |> ignore
    
    //[for i = 0 to (grid |> Array2D.length1) - 1 do 
    //    for j = 0 to (grid |> Array2D.length2) - 1  do (i, j)]
    //        |> List.filter (fun (i, j) -> grid[i, j] <> '#')
    //        |> List.filter (isCrossroad grid) |> List.length |> Dump |> ignore
    
    let graph = buildGraph grid // |> Dump
    findMaxPathLength graph (start, 0) endTile [] |> Dump |> ignore
    displayGraph graph 
    finalPath |> Dump
    
main()
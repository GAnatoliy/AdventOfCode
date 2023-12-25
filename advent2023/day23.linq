<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\GAnat\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

type Direction = |N |E |S |W

let isWithinGrid (grid: _ array2d) (i, j, _) =  0 <= i && i < (grid |> Array2D.length1) && 0 <= j && j < (grid |> Array2D.length2)

let canPass (grid: char array2d) isSlopePasseble (i, j, dir)  = 
    let passableSlopes = if isSlopePasseble then ['^'; '>'; 'v'; '<'] else [match dir with | N -> '^' | E -> '>' | S -> 'v' | W -> '<']
    '.'::passableSlopes |> List.contains grid[i, j]
    
let getAdjoiningPassableTiles grid (i, j) isSlopePasseble = 
    [(-1, 0, N); (0, 1, E); (1, 0, S); (0, -1, W)] 
        |> List.map (fun (i', j', d) -> i' + i, j' + j, d) 
        |> List.filter (isWithinGrid grid)
        |> List.filter (canPass grid isSlopePasseble)
        |> List.map (fun (i', j', _) -> (i', j'))
        
let rec findConnection grid tile path isSlopePasseble = 
    let tiles = getAdjoiningPassableTiles grid tile isSlopePasseble |> List.filter (fun t -> path |> List.contains t |> not) 
    match tiles with 
    | [t] -> findConnection grid t (tile::path) isSlopePasseble
    | _ -> (tile, path.Length)
    
let findConnections grid tile isSlopePasseble = 
    let tiles = getAdjoiningPassableTiles grid tile isSlopePasseble
    tiles |> List.map (fun t -> findConnection grid t [tile] isSlopePasseble) 
     
let buildGraph grid isSlopePasseble = 
    let isVertex grid tile  = getAdjoiningPassableTiles grid tile true |> List.length <> 2
    
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
    
    crossroads |> List.iter (fun t -> findConnections grid t isSlopePasseble |> List.iter (fun (t', length) -> addEdge graph t t' length))
    graph
    
let rec findMaxPathLengthRec (graph: Dictionary<_, _>) (visitedVertexes: Dictionary<_, _>) (v, (l: int)) endV pathLength = 
    visitedVertexes[v] <- true
    let vertexes = graph[v] |> List.filter (fun (v', _) -> visitedVertexes[v'] |> not) 
    let rez = 
        match v = endV, vertexes with 
        | true, _ -> l + pathLength
        | false, [] -> 0
        | false, vs -> vs |> List.map (fun v' -> findMaxPathLengthRec graph visitedVertexes v' endV (l + pathLength)) |> List.max   
    visitedVertexes[v] <- false
    rez
    
let findMaxPathLength (graph: Dictionary<_, _>) startV endV = 
    findMaxPathLengthRec graph (graph.Keys |> List.ofSeq |> List.map (fun k -> (k, false)) |> dict |> Dictionary) (startV, 0) endV 0
    
let main() = 
    let input = 23 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) 
    let grid = input |> List.map List.ofSeq |> array2D
    
    let start = 0, grid[0,0..] |> Array.findIndex (fun c -> c = '.')
    let endRow = (grid |> Array2D.length1) - 1
    let endTile = endRow, grid[endRow,0..] |> Array.findIndex (fun c -> c = '.') 
    
    let graph = buildGraph grid false 
    findMaxPathLength graph start endTile |> Dump |> ignore
    
    let graph = buildGraph grid true 
    findMaxPathLength graph start endTile |> Dump |> ignore
    
main()
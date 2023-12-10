<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

type Direction = |N | E | S | W
let startType = 'S'

let pipesConnectorsMap = Map [
    '|', [S; N]; 
    '-', [W; E];
    'F', [S; E];
    '7', [S; W];
    'J', [W; N];
    'L', [E; N];
    '.', []] 
    
let getOppositeDir dir = match dir with |N -> S|E -> W|S -> N|W -> E

let isConnect cS p1 p2 dir =
    let p1C = pipesConnectorsMap[if p1 <> startType then p1 else cS]
    let p2C = pipesConnectorsMap[if p2 <> startType then p2 else cS]
    // (p1C, p2C) |> Dump
    p1C |> List.exists (fun x -> x = dir) && p2C |> List.exists (fun x -> x = getOppositeDir dir)
    
let getConnections cS (tiles: char array2d) (cTile: int * int) (cType: char) =
    let (i, j) = cTile 
    [(i - 1, j, N); (i, j + 1, E); (i + 1, j, S); (i, j - 1, W)] 
        // TODO: detect correct length 
        |> List.filter (fun (i', j', _) -> 0 <= i' && i' < (tiles |> Array2D.length1) && 0 <= j' && j' < (tiles |> Array2D.length2))
        |> List.filter (fun (i', j', dir) -> isConnect cS cType tiles[i', j'] dir)
        |> List.map (fun (_, _, d) -> d)
        
let nextTile currentTile dir =
    let (i, j) = currentTile 
    match dir with 
    | N -> (i - 1, j)
    | E -> (i, j + 1)
    | S -> (i + 1, j)
    | W -> (i, j - 1)
    

let main() =
    let input = 10 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath))
    let tiles = input |> List.map List.ofSeq |> array2D 
    
    let startTile = 
        input |> List.mapi (fun i (r: string) -> if r.Contains(startType) then (i, r.IndexOf(startType)) |> Some else None)
        |> List.find (fun x -> x |> Option.isSome) |> Option.get
        
    let rec validateLoop cS length currentTile fromDir =
        let (i, j) = currentTile
        let currentType = tiles[i, j]
        let currentType = if currentType = startType then cS else currentType
        
        let connections = getConnections cS tiles currentTile currentType
        match connections |> List.length with 
        | 2 -> 
            let toDir = connections |> List.find (fun c -> c <> fromDir)
            let nextTile = nextTile currentTile toDir
            let nextTileType = tiles[fst nextTile, snd nextTile]
            match nextTileType with 
            | 'S' -> Some length 
            | _ -> validateLoop cS (length + 1) nextTile (getOppositeDir toDir)
        | _ -> None 
    
    pipesConnectorsMap 
        |> Map.filter (fun t _ -> t <> '.') 
        |> Map.toList
        |> List.map (fun (t, cs) -> validateLoop t 1 startTile cs[0])
        |> List.find (fun length -> length |> Option.isSome)
        |> Option.get |> fun x -> x / 2 |> Dump |> ignore
    
main()
<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

type Direction = |N | E | S | W
type Mark = |Unknown|Loop|Inner
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

let findDir t1 t2 =
    let (i1, j1) = t1
    let (i2, j2) = t2
    match (i2 - i1, j2 - j1) with 
    | -1, 0 -> N 
    | 0, 1 -> E 
    | 1, 0 -> S
    | 0, -1 -> W
    | _ -> failwith "Error" 
        
let innerTiles (tiles: char array2d) cT dir = 
    let (i, j) = cT
    let cType = tiles[i, j]
    match cType, dir with 
    | '-', E -> [(i + 1, j)]
    | '-', W -> [(i - 1, j)]
    | '|', N -> [(i, j + 1)]
    | '|', S -> [(i, j - 1)]
    | 'F', E -> [(i + 1, j + 1)]
    | 'F', S -> [(i, j - 1); (i - 1, j - 1); (i - 1, j)]
    | '7', S -> [(i + 1, j - 1)]
    | '7', W -> [(i - 1, j); (i - 1, j + 1); (i, j + 1)]
    | 'J', W -> [(i - 1, j - 1)]
    | 'J', N -> [(i, j + 1);(i + 1, j + 1);(i + 1, j)]
    | 'L', N -> [(i - 1, j + 1)]
    | 'L', E -> [(i, j - 1); (i + 1, j - 1); (i + 1, j)]
    | _ -> failwith "Error"
    

let main() =
    let input = 10 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath))
    let tiles = input |> List.map List.ofSeq |> array2D 
    
    let startTile = 
        input |> List.mapi (fun i (r: string) -> if r.Contains(startType) then (i, r.IndexOf(startType)) |> Some else None)
        |> List.find (fun x -> x |> Option.isSome) |> Option.get
        
    let rec validateLoop cS path currentTile fromDir =
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
            | 'S' -> Some path 
            | _ -> validateLoop cS (nextTile::path) nextTile (getOppositeDir toDir)
        | _ -> None 
    
    let (startType, path) = 
        pipesConnectorsMap 
        |> Map.filter (fun t _ -> t <> '.') 
        |> Map.toList
        |> List.map (fun (t, cs) -> (t, validateLoop t [startTile] startTile cs[0]))
        |> List.find (fun (t, path) -> path |> Option.isSome)
        |> fun (t, path) -> (t, path |> Option.get)
        
    path.Length / 2 |> Dump |> ignore
    
    // Part 2
    
    // Find one of the most left tiles
    let startJ = path |> List.map (fun (_, j) -> j) |> List.min
    let start = path |> List.filter (fun (_, j) -> j = startJ) |> List.minBy (fun (i, _) -> i)
    
    let tiles = tiles |> Array2D.map (fun t -> if t <> 'S' then t else startType)
    let markedTiles = tiles |> Array2D.mapi (fun i j _ -> Unknown)
    
    // Fill markedTiles with Path
    path |> List.iter (fun (i, j) -> markedTiles[i, j] <- Loop)
    
    // Roate path if it isn't clock wise.
    let (startIndex, _) = path |> List.indexed |> List.find (fun (_, t) -> t = start) 
    let nextIndex = startIndex + 1
    let next = path[if nextIndex <> path.Length then nextIndex else 0]
    let dir = findDir start next
    let path = if dir = N || dir = E then path else path |> List.rev
        
        
    let rec markInDepth t = 
        //t |> Dump |> ignore
        let (i, j) = t 
        match markedTiles[i, j] with 
        | Unknown -> 
            markedTiles[i, j] <- Inner 
            // markedTiles[i - 1..i + 1, j - 1..j + 1] |> Array2D.iter markInDepth 
            // TODO: implement via generator
            [(i - 1, j - 1);(i - 1, j);(i - 1, j + 1);(i, j - 1);(i, j + 1);(i + 1, j - 1);(i + 1, j);(i + 1, j + 1)] |> List.iter markInDepth 
        | Loop -> ()
        | Inner -> ()
        
        
    // Add one element in order to handle the latest element.
    let path = (path.Last())::path
    
    let rec traverse path =
        match path with 
        | [] | [_] -> ()
        | t1::t2::ts -> 
            // findDir t1 t2 |> innerTile tiles t1 |> markInDepth
            findDir t1 t2 |> innerTiles tiles t1 |> List.iter markInDepth
            traverse (t2::ts)
            
    traverse path
    
    markedTiles |> Seq.cast |> Seq.filter (fun x -> x = Inner) |> Seq.length |> Dump |> ignore
    
main()
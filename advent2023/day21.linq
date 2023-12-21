<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

let getNextTiles (i, j) = [(i - 1, j); (i + 1, j); (i, j + 1); (i, j - 1)] 
let isNotRock (map: char array2d) (i, j) = map[i, j] <> '#' 
let rec traverse (memo: Dictionary<_, _>) map step tiles = 
    match step with 
    | 0 -> tiles 
    | s -> 
        tiles |> List.map (
            fun (i, j) -> 
                match memo.ContainsKey (i, j, s - 1) with 
                | true -> memo[(i, j, s - 1)]
                | false -> 
                    let rez = 
                        (i, j) |> getNextTiles 
                        |> List.filter (Utils.Array2D.isWithinArrayTuple map)
                        |> List.filter (isNotRock map)
                        |> traverse memo map (s - 1)
                    memo.[(i, j, s - 1)] <- rez
                    rez) 
        |> List.collect id |> List.distinct
        
    
let main() = 
    let input = 21 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) 
    let map = input |> List.map List.ofSeq |> array2D 
    
    let startPosition = map |> Utils.Array2D.tryFind 'S' |> Option.get
    
    let memo = new Dictionary<int * int * int, (int * int) list>()
    traverse memo map 64 [startPosition] |> List.length |> Dump |> ignore

main()
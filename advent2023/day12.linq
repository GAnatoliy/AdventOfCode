<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

let canPlace (subrow: string) = subrow.Length <> 0 && not (subrow.Contains('.'))
let canSkip (subrow: string) = subrow.Length <> 0 && not (subrow.Contains('#'))
let canSkipTail (tail: string) = not (tail.Contains('#'))
    
let mutable memo = Map.empty<int list * string, int64> 

let rec numbersOfArrangementRec (broken: int list)  (row: string) =
    match memo |> Map.tryFind (broken, row) with 
    | Some r -> r
    | None -> 
        let result = 
            match broken with 
            | [] -> failwith "Error"
            | b::[] -> 
                match row.Length < b with 
                | true -> 0L
                | false -> 
                    (if canSkip row[0..0] then numbersOfArrangementRec broken row[1..] else 0) + 
                    match canPlace (row[..b - 1]) with 
                    | true -> 
                        match canSkipTail row[b..] with 
                        | true -> 1L
                        | false -> 0L
                    | false -> 0L
            | b::bs -> 
                match row.Length < b with 
                | true -> 0
                | false -> 
                    (if canSkip row[0..0] then numbersOfArrangementRec broken row[1..] else 0) + 
                    match canPlace (row[..b - 1]) with 
                    | true -> 
                        match canSkip row[b..b] with 
                        | true -> numbersOfArrangementRec bs row[b+1..]  // out of index? 
                        | false -> 0L
                    | false -> 0L
                    
        memo <- memo |> Map.add (broken, row) result 
        result
        
let numbersOfArrangement (broken: int list)  (row: string) = 
    numbersOfArrangementRec broken row
    
let main () =
    let input = 12 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath))
    input |> List.map (fun str -> 
        let row::brokenStr::_ = str.Split(' ') |> List.ofArray 
        numbersOfArrangement (brokenStr.Split(',') |> List.ofArray |> List.map int) row) |> List.sum |> Dump
        
    input |> List.map (fun str -> 
        let row::brokenStr::_ = str.Split(' ') |> List.ofArray 
        let row = [for i in 0..4 do row] |> String.concat "?"
        let brokenStr = [for i in 0..4 do brokenStr] |> String.concat ","  
        numbersOfArrangement (brokenStr.Split(',') |> List.ofArray |> List.map int) row) |> List.sum |> Dump
    
    
main ()
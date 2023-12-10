<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

let main() = 
    let input = 9 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath))
    let parseValues (row: string) = row.Split(' ') |> List.ofSeq |> List.map int64
    	
    let rec findValue picker operation (acc: int64 list) (values: int64 list)  = 
        let diffs = values |> List.pairwise |> List.map (fun (v1, v2) -> v2 - v1)
        match diffs |> List.forall (fun x -> x = 0) with 
        | true -> 0L::(values |> picker)::acc |> List.reduce (fun v1 v2 -> operation v1 v2)
        | false -> findValue picker operation ((values |> picker)::acc) diffs 
        
    let findNextValue = findValue (fun l -> l.Last()) (fun v1 v2 -> v1 + v2)
    let findPrevValue = findValue (fun l -> l.First()) (fun v1 v2 -> v2 - v1)
    
    input |> List.map (parseValues >> findNextValue []) |> List.sum |> Dump |> ignore
    input |> List.map (parseValues >> findPrevValue []) |> List.sum |> Dump |> ignore
main () 
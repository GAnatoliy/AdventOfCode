<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

let isReqPlaced placements reqPlacements = 
    reqPlacements |> List.forall (fun (srp, erp) -> placements |> List.exists (fun (sp, ep) -> sp <= srp && erp <= ep))

let rec numbersOfArrangementRec rows broken (placements: (int * int) list) (reqPlacements: (int * int) list) = 
    // rows [(start, end),...] - list of positions where could be broken springs 
    // broken [1, 3, 5] - uninterapted brokens 
    // placements [(start, end),...] list of already placed broken springs 
    // reqPlacements [(start, end),...] list springs that should be placed
    match (rows, broken) with 
    | (_, []) -> if isReqPlaced placements reqPlacements then 1 else 0
    | ([], _) -> 0
    | ((s, e)::rs, _) -> 
        let brokenChainSize = broken[0]
        let subrowSize = e - s + 1
        let cantContainChain = subrowSize < brokenChainSize
        match cantContainChain with 
        | true -> numbersOfArrangementRec rs broken placements reqPlacements
        | false ->
            let canContainMoreChains = brokenChainSize < subrowSize + 2
            numbersOfArrangementRec ((s + 1, e)::rs) broken placements reqPlacements + 
            match canContainMoreChains with 
            | true -> numbersOfArrangementRec ((s + brokenChainSize + 1, e)::rs) broken[1..] ((s, (s + brokenChainSize - 1))::placements) reqPlacements
            | false -> numbersOfArrangementRec rs broken[1..] ((s, s + brokenChainSize - 1)::placements) reqPlacements

let numbersOfArrangement rows broken reqPlacements  = 
    match broken with 
    | [] -> 0
    | _ -> numbersOfArrangementRec rows broken [] reqPlacements
    
let tests () = 
    // Tests
    numbersOfArrangement [(0, 0)] [] [] |> Dump // 0
    numbersOfArrangement [(0, 0)] [1] [] |> Dump // 1
    numbersOfArrangement [(0, 1)] [1] [] |> Dump // 2
    numbersOfArrangement [(0, 0)] [2] [] |> Dump // 0 (can't place)
    numbersOfArrangement [(0, 1)] [2] [] |> Dump // 1
    numbersOfArrangement [(0, 0); (2, 2)] [1] [] |> Dump // 2
    numbersOfArrangement [(0, 1); (2, 2)] [1] [] |> Dump // 3
    numbersOfArrangement [(0, 2); (4, 6)] [1; 1; 3] [(4, 6)] |> Dump // 1
    numbersOfArrangement [(0,11)] [3;2;1] [(1, 3)] |> Dump // 10
    
let main () = 
    // tests () |> ignore
    
    let input = 12 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath))
    
    let parseStr (str: string) =  
        let rowStr::brokenStr::_ = str.Split (' ') |> List.ofArray
        let indexedRow = rowStr |> List.ofSeq |> List.mapi (fun i c -> (i, c))
        let broken = brokenStr.Split (',') |> Seq.map (fun x -> x |> int) |> List.ofSeq
        
        let (accRows, rows) = 
            indexedRow |> List.fold (fun (acc, rez) (i, c) -> 
            match (acc, c) with 
            | None, '.' -> (acc, rez)
            | Some a, '.' -> (None, a::rez)
            | None, _ -> (Some (i, i), rez)
            | Some (sa, ea), _ -> (Some (sa, ea + 1), rez)) (None, []) 
        let rows = match accRows with |Some r -> r::rows | None -> rows 
        // TODO: requse code 
        let (accPlacemants, reqPlacements) = 
            indexedRow |> List.fold (fun (acc, rez) (i, c) -> 
            match (acc, c) with 
            | None, '#' -> (Some (i, i), rez)
            | Some (sa, ea), '#' -> (Some (sa, ea + 1), rez)
            | None, _ -> (acc, rez)
            | Some a, _ -> (None, a::rez)) (None, [])
            
        let reqPlacements = match accPlacemants with |Some rp -> rp::reqPlacements | None -> reqPlacements 
        
        (rows |> List.rev, broken, reqPlacements |> List.rev)
        
    input |> List.map parseStr |> List.map (fun (rows, broken, reqPlacements) -> numbersOfArrangement rows broken reqPlacements) |> List.sum |> Dump
    
    
    
    
main()
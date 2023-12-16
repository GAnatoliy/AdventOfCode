<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

let rec hash v (step: char list) = 
    match step with 
    | [] -> v 
    | s::ss -> ss |> hash ((v + (s |> int) ) * 17 % 256)

let deserialize (str: string) = 
    match str.Split("=") |> List.ofSeq with 
    | label::focus::_ -> (label, "=", focus |> int |> Some)
    | _ -> (str[..str.Length - 2], "-", None)
        
        
let processLense (boxes: (string * int) list array) (lense: string * string * (int option)) = 
    let (label, operation, focus) = lense 
    let labelHash = hash 0 (label |> Seq.toList)
    let box = boxes[labelHash]
    
    boxes[labelHash] <- 
        match operation with 
        | "-" -> 
            match box |> List.tryFindIndex (fun (l, _) -> l = label)  with
            | Some i -> box |> List.removeAt i 
            | None -> box 
        | "=" -> 
            match box |> List.tryFindIndex (fun (l, _) -> l = label) with 
            | Some i -> box |> List.removeAt i |> List.insertAt i (label, focus |> Option.get)
            | None -> box @ [(label, focus |> Option.get)]
        | _ -> failwith "Error" 
    

let main() =
    let steps = 15 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) |> List.head
    steps.Split(',') |> Seq.map (Seq.toList >> hash 0) |> Seq.sum |> Dump |> ignore
    
    let boxes = Array.create 256 []
    steps.Split(',') |> Seq.iter (deserialize >> processLense boxes) 
    
    boxes |> Array.mapi (fun i box -> box |> List.mapi (fun i' (_, f) -> (1 + i) * (1 + i') * f)) 
        |> List.ofArray |> List.collect id |> List.sum |> Dump |> ignore
    
main()
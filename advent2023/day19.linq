<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\GAnat\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

type Action = | Reject | Accept |Redirect of string 

type Rule = 
| LessThen of param: char * value: int 
| MoreThen of param: char * value: int 
| Default 

type Step = {
    Rule: Rule 
    Action: Action
}

type Part = Map<char, int> 


let parse (rows: string list) = 
    let mapAction a = match a with | "A" -> Accept | "R" -> Reject | _ -> a |> Redirect 
    let mapRule (r: string) = 
        let pv = r[0],  r[2..] |> int
        if r[1] = '<' then LessThen pv else MoreThen pv
    let parseStep (step: string) = 
        match step.Split(':') with 
        | [|a|] -> { Rule = Default; Action = a |> mapAction }
        | [|rule; a|] -> { Rule = rule |> mapRule; Action = a |> mapAction }
        | _ -> failwith "Error"
        
    let parseWorkflow (workflow: string) = 
        let parts = workflow.Split('{')
        let name = parts[0]
        let steps = (parts[1][..(parts[1].Length - 2)]).Split(',') |> Seq.map parseStep |> List.ofSeq
        (name, steps)
        
    let parseParts (part: string) = 
        let parsePart (p: string) = 
            let parts = p.Split('=')
            (parts[0][0], parts[1] |> int)
        part[1..part.Length - 2].Split(',') |> List.ofSeq |> List.map parsePart 
        
        
    let parts = rows |> Utils.Core.splitBy "" 
    let workflows = parts[0] |> List.map parseWorkflow |> List.fold (fun (wfs: Map<_,_>) wf -> wfs.Add wf) Map.empty<string, Step list>
    let parts = parts[1] |> List.map (parseParts >> Map.ofList)
    workflows, parts
    
let rec processPart (workflows: Map<_,_>) steps (part: Map<_, _>) =    
    match steps with 
    | [] -> failwith "Error" 
    | s::ss ->
        let inline doAction a = 
            match s.Action with 
            | Reject -> None
            | Accept -> Some part 
            | Redirect w -> processPart workflows workflows[w] part
        match s.Rule with 
        | Default -> s.Action |> doAction
        | LessThen (p, v) when part[p] < v -> s.Action |> doAction
        | MoreThen (p, v) when part[p] > v -> s.Action |> doAction
        | _ -> processPart workflows ss part
       
let rec predict (workflows: Map<_,_>) steps (partStat: Map<_, _>) = 
    match steps with 
    | [] -> failwith "Error"
    | s::ss -> 
        let inline doAction a ps = 
            match s.Action with 
            | Reject -> [None] 
            | Accept -> [Some ps] 
            | Redirect w -> predict workflows workflows[w] ps
            
        match s.Rule with 
        | Default -> doAction s.Action partStat
        | LessThen (p, v) -> 
            let min, max = partStat[p]
            doAction s.Action (partStat |> Map.add p (min, v - 1)) @
                predict workflows ss (partStat |> Map.add p (v, max))
        | MoreThen (p, v) -> 
            let min, max = partStat[p]
            doAction s.Action (partStat |> Map.add p (v + 1, max)) @
                predict workflows ss (partStat |> Map.add p (min, v))
        
let main() = 
    let input = 19 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) 
    let (workflows, parts) = input |> parse
    
    parts |> List.map (processPart workflows workflows["in"]) |> List.filter Option.isSome 
        |> List.map (fun p -> p.Value |> Map.values |> Seq.sum) |> List.sum |> Dump |> ignore
    
    let min, max = 1, 4000
    let createPartStat = [('x', (min, max)); ('m', (min, max)); ('a', (min, max)); ('s', (min, max))] |> Map
    createPartStat |> predict workflows workflows["in"] |> List.filter Option.isSome
        |> List.map (fun ps -> ps.Value |> Map.values |> Seq.map (fun (min, max) -> max - min + 1 |> int64) |> Seq.fold (*) 1L) 
        |> List.sum |> Dump |> ignore

main ()
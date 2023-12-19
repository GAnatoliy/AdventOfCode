<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
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
    | [] -> None 
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
        
let main() = 
    let input = 19 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) 
    let (workflows, parts) = input |> parse
    
    // workflows |> Dump
    // parts |> Dump
    
    parts |> List.map (processPart workflows workflows["in"]) |> List.filter (!=) null |> Dump
    
main ()
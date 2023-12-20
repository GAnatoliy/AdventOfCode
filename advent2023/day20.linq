<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\GAnat\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

type Pulse = |Low|High 
type Name = string 
type Type = |FlipFlop of Pulse |Conjunction  of Map<Name, Pulse> | Broadcaster | Untyped 
type Module = {
    Name: Name 
    Type: Type 
    Outputs: Name list
    Counter: (int * int)
}

let parseRow (row: string) = 
    let (moduleStr::outputStr::_) = row.Split("->") |> List.ofSeq
    let outputs = outputStr.Split(",") |> Seq.map (fun n -> n.Trim(' ') |> Name) |> List.ofSeq
    let inline getName (ns: string) = ns[1..ns.Length - 2]
    match moduleStr[0] with 
    | '%' -> { Type = FlipFlop(Low); Name = getName moduleStr |> Name; Outputs = outputs; Counter = (0, 0) } 
    | '&' -> { Type = Conjunction(Map.empty); Name = getName moduleStr |> Name; Outputs = outputs; Counter = (0, 0) } 
    | 'b' -> { Type = Broadcaster; Name = "broadcaster"; Outputs = outputs; Counter = (0, 0)}
    
let parse (rows: string list) = 
    let fillConjunctions ms  = 
        ms |> Map.map (fun _ m ->
                match m.Type with 
                | Conjunction _ -> 
                    let inputs = ms |> Map.values |> Seq.filter (fun m' -> m'.Outputs |> List.contains m.Name) 
                                 |> Seq.map (fun m -> (m.Name, Low)) |> Map  
                    { m with Type = Conjunction inputs }
                | _ -> m)
        
    let addUntyped ms = 
        ms |> Map.values |> Seq.collect (fun m -> m.Outputs) 
           |> Seq.fold (fun ms n -> if ms |> Map.containsKey n then ms else ms |> Map.add n { Type = Untyped; Name = n; Outputs = []; Counter = (0, 0)}) ms
           
    rows |> List.map (parseRow >> (fun m -> (m.Name, m))) |> Map |> addUntyped |> fillConjunctions
    
    
let updateCounter p (low, high) = match p with |Low -> (low + 1, high) | High -> (low, high + 1)
let sendPulseToModule m source pulse  = 
    match m.Type with 
    | Broadcaster -> { m with Counter = m.Counter |> updateCounter pulse }, m.Outputs |> List.map (fun o -> m.Name, o, pulse)
    | FlipFlop cp -> 
        match pulse with 
        | High -> { m with Counter = m.Counter |> updateCounter pulse }, []  
        | Low -> 
            let output = match cp with |Low -> High|High -> Low 
            { m with Counter = m.Counter |> updateCounter pulse; Type = FlipFlop output }, m.Outputs |> List.map (fun o -> m.Name, o, output)
    | Conjunction memory -> 
        let memory = memory |> Map.add source pulse 
        let output = if memory |> Map.values |> Seq.forall (fun v -> v = High) then Low else High 
        { m with Counter = m.Counter |> updateCounter pulse; Type = Conjunction memory }, m.Outputs |> List.map (fun o -> m.Name, o, output) 
    | Untyped ->   
        { m with Counter = m.Counter |> updateCounter pulse }, []

let rec sendPulse (modules: Map<Name, Module>) (outputs: (Name * Name * Pulse) list) = 
    match outputs with 
    | [] -> modules
    | (source, dest, pulse)::os -> 
        let m', outputs' = sendPulseToModule modules[dest] source pulse
        sendPulse (modules |> Map.add dest m') (os @ outputs')
                
let main() =
    let input = 20 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) 
    
    let modules = input |> parse 
    // let modules = sendPulse modules [("", "broadcaster", Low)]
    let modules = [1..1000] |> List.fold (fun ms _ -> sendPulse ms [("", "broadcaster", Low)]) modules 
    let lowSum, highSum = 
        modules |> Map.values |> Seq.map (fun {Counter = (l, h)} -> (l, h)) 
        |> Seq.fold (fun (ls, hs) (l, h) -> (ls + l), (hs + h)) (0, 0) 
    
    lowSum * highSum |> Dump |> ignore
    
main()
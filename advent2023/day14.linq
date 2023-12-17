<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

type Direction = |N | E | S | W

let swap (pl: _ array2d) (iS, jS) (iD, jD) = 
    let t = pl[iS, jS]
    pl[iS, jS] <- pl[iD, jD]
    pl[iD, jD] <- t 
    
let isEnd pl (iS, jS) (iD, jD) d = 
    let (iL, jL) = pl |> Array2D.length1, pl |> Array2D.length2
    match d with
    | N -> iS = iL, iD = iL
    | E -> jS = -1, jD = -1
    | S -> iS = -1, iD = -1
    | W -> jS = jL, jD = jL

let nextCell pl (i, j) d = 
    match d with 
    | N -> (i + 1, j)
    | E -> (i, j - 1)
    | S -> (i - 1, j)
    | W -> (i, j + 1)
    
let rec tilt (pl: _ array2d) sc dc d =
    let nsc = nextCell pl sc d
    let ndc = nextCell pl dc d
    let inline at (i, j) = pl[i, j]
        
    match isEnd pl sc dc d with 
    | true, _ -> ()
    | _, true -> tilt pl nsc nsc d
    | _ -> 
        match at sc with 
        | 'O' | '#' -> tilt pl nsc nsc d
        | '.' -> 
            match at dc with 
            | '#' -> tilt pl nsc nsc d
            | '.' -> tilt pl sc ndc d
            | 'O' -> 
                swap pl sc dc 
                tilt pl nsc ndc d 
            | _ -> failwith "Error"
        | _ -> failwith "Error"
        
let tiltPlatform d pl =
    let (iL, jL) = pl |> Array2D.length1, pl |> Array2D.length2
    match d with 
    | N -> [0..jL - 1] |> List.iter (fun j -> tilt pl (0, j) (0, j) d)
    | E -> [0..iL - 1] |> List.iter (fun i -> tilt pl (i, jL - 1) (i, jL - 1) d)
    | S -> [0..jL - 1] |> List.iter (fun j -> tilt pl (iL - 1, j) (iL - 1, j) d)
    | W -> [0..iL - 1] |> List.iter (fun i -> tilt pl (i, 0) (i, 0) d)
    pl

let spin pl = 
    [N; W; S; E] |> List.iter (fun d -> pl |> tiltPlatform d |> ignore)
    pl

let calcLoad (pl: char array2d) = 
    pl |> Array2D.mapi (fun i j c -> if c = 'O' then (pl |> Array2D.length1) - i else 0) |> Seq.cast<int> |>  Seq.sum

let callibrate spinsN (pl: _ array2d) = 
    let mutable loads = Map.empty<int, int list>
    let mutable lastLoad = 0
    for i in 0..spinsN - 1 do 
        let load = pl |> spin |> calcLoad
        loads <- loads |> Map.add load (if loads |> Map.containsKey load then i::loads[load] else [i])
        lastLoad <- load 
    
    let spins = loads[lastLoad]  
    match spins.Count () < 7 with 
    | true -> failwith "Error. Not enouph data."
    | false -> spins[0] - spins[1]
    
let runSpins spinsN (pl: _ array2d) = 
    let callibrationSpins = 1000
    let spinCycle = pl |> callibrate callibrationSpins 
    let leftSpins = spinsN - callibrationSpins
    for i in 0..(leftSpins % spinCycle - 1) do pl |> spin |> ignore
    pl
    
let main() =
    let input = 14 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) 
    let platform = input |> List.map List.ofSeq |> array2D 
    
    platform |> tiltPlatform N |> calcLoad |> Dump |> ignore
    
    let platform = input |> List.map List.ofSeq |> array2D 
    platform |> runSpins 1000000000 |> calcLoad |> Dump |> ignore
    
main()
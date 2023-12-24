<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\GAnat\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

type Coord = int * int * int
type Brick = int * Coord * Coord 
type Pile = Brick list 

let lowerBrick (n, (x1, y1, z1), (x2, y2, z2)) = (n, (x1, y1, z1 - 1), (x2, y2, z2 - 1))
let isNotTheSame (n1, _, _) (n2, _, _) = n1 <> n2

let parse (rows: string list) = 
    let parseCoord (coords: string) = 
        let cs = coords.Split(",") |> Seq.map (fun c -> c |> int) |> List.ofSeq
        (cs[0], cs[1], cs[2]) |> Coord 
    let parseRow i (row: string) = 
        let rs = row.Split("~") |> List.ofSeq
        (i, rs[0] |> parseCoord, rs[1] |> parseCoord) |> Brick
        
    rows |> List.mapi parseRow |> List.sortBy (fun (_, (_, _, z1), _) -> z1)
    
let isCollideByAxis a1 b1 a2 b2 = a2 <= a1 && a1 <= b2 || a2 <= b1 && b1 <= b2 || a1 <= a2 && a2 <= b1 
let isCollide brick1 brick2 = 
    let (_, (br1x1, br1y1, br1z1), (br1x2, br1y2, br1z2)) = brick1
    let (_, (br2x1, br2y1, br2z1), (br2x2, br2y2, br2z2)) = brick2
    isCollideByAxis br1x1 br1x2 br2x1 br2x2 && isCollideByAxis br1y1 br1y2 br2y1 br2y2 && isCollideByAxis br1z1 br1z2 br2z1 br2z2
    
let rec dropBrick (pile: Pile) brick = 
    let isNextLevelCollide pile brick = 
        let (_, (_, _, z1), _) = brick 
        let lowerBrick = brick |> lowerBrick 
        z1 = 1 || pile |> List.exists (fun b -> isCollide b lowerBrick && isNotTheSame b brick)
    
    match isNextLevelCollide pile brick with 
    | true -> brick 
    | false -> dropBrick pile (brick |> lowerBrick)
    
    
let rec isSafeToDisintegrate pile brick = 
    let inline projectOnLevelUp (n, (x1, y1, z1), (x2, y2, z2)) = (n, (x1, y1, z2 + 1), (x2, y2, z2 + 1))
    let inline  projectOnLevelDown (n, (x1, y1, z1), (x2, y2, z2)) = (n, (x1, y1, z1 - 1), (x2, y2, z1 - 1)) 
    
    let upper = brick |> projectOnLevelUp
    let upperCollisions = pile |> List.filter (fun b -> isCollide b upper && isNotTheSame b upper) |> List.map projectOnLevelDown
    upperCollisions |> List.exists (fun b -> pile |> List.filter (fun b' -> isCollide b' b && isNotTheSame b' b && isNotTheSame b' brick) |> List.length = 0) |> not
    
let removeBrick (n, _, _) pile = pile |> List.findIndex (fun (n', _, _) -> n = n') |> List.removeAt <| pile
let addBrick br pile = br::pile 
let wasMoved orign changed = orign <> changed 

let calcFalls pile brick = 
    match isSafeToDisintegrate pile brick with 
    | true -> 0
    | false -> 
        let pile' = pile |> removeBrick brick |> List.sortBy (fun (_, (_, _, z1), _) -> z1)
        pile' |> List.fold (fun (c, pile'') b -> 
                                let b' = b |> dropBrick pile''
                                match wasMoved b' b with 
                                | true -> c + 1, pile'' |> removeBrick b |> addBrick b'
                                | false -> c, pile'') (0, pile') |> fst 
        
let main() =
    let input = 22 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) 
    let bricks = input |> parse 
    let pile = bricks |> List.fold (fun pile brick -> (dropBrick pile brick)::pile) [] 
    
    pile |> List.filter (fun b -> isSafeToDisintegrate pile b) |> List.length |> Dump |> ignore
    pile |> List.map (calcFalls pile) |> List.sum |> Dump |> ignore
    
main()

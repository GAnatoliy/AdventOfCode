<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

let main () =
    let input = 11 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath))
    let image = input |> List.map List.ofSeq |> array2D
    let imageRowsC = image |> Array2D.length1
    let imageColumnC = image |> Array2D.length2
    
    let isSpace c = c = '.'
    let noGalaxyRowMap = [for i in 0..imageColumnC - 1 do image[i,0..] |> Seq.forall isSpace]
    let noGalaxyColumnMap = [for j in 0..imageRowsC - 1 do image[0..,j] |> Seq.forall isSpace]
    
    let galaxies = image |> Array2D.mapi (fun i j c -> (i, j, c)) |> Seq.cast<int*int*char> 
                   |> Seq.filter (fun (_, _, c) -> c = '#') |> Seq.map (fun (i, j, _) -> (i, j)) |> List.ofSeq
                   
    let expandCount (expandFactor:int64) (map:bool list) (x1:int) (x2:int) = 
        let expand = map[Math.Min(x1, x2)..Math.Max(x1, x2)] |> List.filter id |> List.length 
        int64 (expand) * (expandFactor - 1L)
        
    let calcDistance (expandFactor:int64) (i1:int, j1:int) (i2, j2) = 
        int64 (Math.Abs(i1 - i2)) + int64 (Math.Abs(j1 - j2)) + 
            (expandCount expandFactor noGalaxyRowMap i1 i2) + (expandCount expandFactor noGalaxyColumnMap j1 j2)
            
    let pairs = Utils.Core.combinations [] 2 galaxies
    pairs |> Seq.map (fun (g1::g2::_) -> calcDistance 2L g1 g2) |> Seq.sum |> Dump |> ignore
    pairs |> Seq.map (fun (g1::g2::_) -> calcDistance 1000000L g1 g2) |> Seq.sum |> Dump |> ignore
    
main ()

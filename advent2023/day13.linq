<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
</Query>

let transpose m = Array2D.init (m |> Array2D.length2) (m |> Array2D.length1) (fun r c -> m.[c,r])
let reflectRows m = 
    let (l1, l2) = m |> Array2D.length1, m |> Array2D.length2
    Array2D.init l1 l2 (fun r c -> m.[(m |> Array2D.length1) - r - 1,c])

let findReflect middle (pattern: char array2d)  = 
    let l1 = pattern |> Array2D.length1
    let halfSize = Math.Min(middle, l1 - middle)
    match pattern[middle - halfSize..middle-1, 0..] = reflectRows pattern[middle..middle + halfSize - 1, 0..] with 
    | true -> middle
    | false -> 0

let rec findReflectionNumbers middle skipReflection (pattern: char array2d)  = 
    let l1 = pattern |> Array2D.length1
    match middle = l1 with 
    | true -> 0 
    | false ->
        let reflections = findReflect middle pattern
        match reflections = 0 || reflections =  skipReflection with 
        | true -> findReflectionNumbers (middle + 1) skipReflection pattern 
        | false -> reflections
        
let swap i j (p: char array2d) = p[i, j] <- if p[i, j] = '.' then '#' else '.'
let rec findReflectionsWithSmudge middle (pattern: char array2d) = 
    let (ro, co) = findReflectionNumbers middle -1 pattern, findReflectionNumbers middle -1 (pattern |> transpose)
    let (l1, l2) = pattern |> Array2D.length1, pattern |> Array2D.length2
    let reflections =
        [for i in 0..l1 - 1 do 
            for j in 0..l2 - 1 do 
                swap i j pattern
                let reflections = findReflectionNumbers middle ro pattern, findReflectionNumbers middle co (pattern |> transpose)
                swap i j pattern
                reflections] |> List.filter(fun x->x<>(0, 0)) |> List.distinct
                    
    match reflections with 
    | [(x, y)] -> x * 100 + y
    | [] | _ -> failwith "Error"
    

let main () =
    let input = 13 |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath))
    let patterns = input |> Utils.Core.splitBy "" |> List.map (fun p -> p |> List.map List.ofSeq |> array2D) //|> Dump
    
    patterns |> List.map (fun p -> 100 * (p |> findReflectionNumbers 1 -1) + (p |> transpose |> findReflectionNumbers 1 -1)) |> List.sum |> Dump
    patterns |> List.map (findReflectionsWithSmudge 1) |> List.sum |> Dump
    
main ()
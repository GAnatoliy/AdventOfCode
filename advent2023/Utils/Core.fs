namespace Utils

module Core =
    let readInput path = System.IO.File.ReadLines(path)

    let getInputName root day = $"{root}\input\{day}.txt"
    
    let readInputLines (root: string) (day: int) = day |> getInputName root |> readInput |> List.ofSeq

    /// Gets all combinations (without repetition) of specified length from a list.
    /// Code is taken here https://itecnote.com/tecnote/combinations-and-permutations-in-f/
    let rec combinations acc size set = seq {
      match size, set with 
      | n, x::xs -> 
          if n > 0 then yield! combinations (x::acc) (n - 1) xs
          if n >= 0 then yield! combinations acc n xs 
      | 0, [] -> yield acc 
      | _, [] -> () }

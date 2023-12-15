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


    /// Split a list into chunks using the specified separator
    /// This takes a list and returns a list of lists (chunks)
    /// that represent individual groups, separated by the given
    /// separator 'v'
    /// Taken here https://www.fssnip.net/nr/title/Split-a-list-using-a-separator
    let splitBy v list =
      let yieldRevNonEmpty list = 
        if list = [] then []
        else [List.rev list]

      let rec loop groupSoFar list = seq { 
        match list with
        | [] -> yield! yieldRevNonEmpty groupSoFar
        | head::tail when head = v ->
            yield! yieldRevNonEmpty groupSoFar
            yield! loop [] tail
        | head::tail ->
            yield! loop (head::groupSoFar) tail }
      loop [] list |> List.ofSeq

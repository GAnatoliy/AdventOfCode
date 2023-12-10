namespace Utils

module Core =
    let readInput path = System.IO.File.ReadLines(path)

    let getInputName root day = $"{root}\input\{day}.txt"
    
    let readInputLines (root: string) (day: int) = day |> getInputName root |> readInput |> List.ofSeq

<Query Kind="FSharpProgram">
  <Reference Relative="Utils.dll">C:\Users\Anatoliy\OneDrive\Workspace\LinqPadQueries\AdventOfCode\advent2023\Utils.dll</Reference>
  <NuGetReference>FSharp.Data</NuGetReference>
  <Namespace>FSharp.Data</Namespace>
</Query>

// Advent of code 2023 https://adventofcode.com/2023
    
let day1_1 (input: string list): int64 =
    input 
        |> List.map (fun line -> line 
                                |> List.ofSeq 
                                |> List.filter (fun c -> c |> Char.IsDigit) 
                                |> fun charList -> $"{charList.First()}{charList.Last()}" |> int64)
        |> List.fold (fun state number -> state + number) 0 
     
let day1_2 (input: string list): int64 =
    let mapping = [("one", 1); ("two", 2); ("three", 3); ("four", 4); ("five", 5); ("six", 6); ("seven", 7); ("eight", 8); ("nine", 9)]
    // oneight should be 18 but my method will give 11, but result is correct (so there just wasn't such case).
    // more correct would be to search first from left and then independently first from right (e.g. last)
    
    let rec getTextDigit (line: string) (buffer: string) = 
        match line with 
        | "" -> None 
        | l -> 
            let char = l[0]
            let newBuffer = buffer + char.ToString ()
            let textDigit = mapping |> List.tryFind (fun (t, _) -> newBuffer = t) 
            match textDigit with 
            | Some (t, _) -> Some t 
            | None -> 
                let possibleDigits = mapping |> List.filter (fun (t, _) -> t.StartsWith newBuffer) 
                match possibleDigits with 
                | [] -> None 
                | l -> getTextDigit line[1..] newBuffer
                
    let rec getTextDigits (line: string) = 
        match line with 
        | "" -> []
        | l -> 
            let char = l[0]
            match char |> Char.IsDigit with 
            | true -> char::getTextDigits l[1..]
            | false -> 
                match getTextDigit l "" with 
                | None -> getTextDigits l[1..]
                | Some textDigit -> 
                    let digit = mapping |> List.find (fun (t, _) -> textDigit = t) |> (fun (_, d) -> d)
                    digit.ToString()[0] ::getTextDigits l[1..]
    
    input 
        |> List.map (fun line -> line 
                                |> getTextDigits
                                |> fun charList -> $"{charList.First()}{charList.Last()}" |> int
                                |> (fun x -> 
                                    $"{line}, {x}" |> Dump |> ignore
                                    x))
        |> List.sum 
        |> int64
            
type Set = {
    Red: int
    Blue: int
    Green: int
}
    
let day2_1 (input: string list) =  
    // NOTE: pattarn matching isn't complete since we assume that input is correct.
    
    let redMax = 12;
    let greenMax = 13;
    let blueMax = 14;
    
    // Return game index if it is possible or 0 in other case.
    let analyseGame (line:string): int = 
        let game::setsLine::_ = line.Split(':') |> List.ofSeq;
        let _::gameNumber::_ = game.Split(' ') |> List.ofSeq;
        let setsStrings = setsLine.Split(';') |> List.ofSeq
        let sets = setsStrings |> List.map (
            fun setString -> 
                let subsets = setString.Split(',') |> List.ofSeq
                subsets |> List.fold (
                    fun set subset -> 
                        let n::color::_ = subset.Trim(' ').Split(' ') |> List.ofSeq
                        match color with 
                        | "red" -> {set with Red = n |> int }
                        | "green" -> {set with Green = n |> int }
                        | "blue" -> {set with Blue = n |> int }
                ) { Set.Blue = 0; Green = 0; Red = 0 }
            )
            
        match sets |> List.forall (fun set -> set.Red <= redMax && set.Green <= greenMax && set.Blue <= blueMax) with
        | true -> gameNumber |> int
        | false -> 0
        
    input |> List.map analyseGame |> List.sum |> int64
    
let day2_2 (input: string list) =  
    // NOTE: pattarn matching isn't complete since we assume that input is correct.
    
    let redMax = 12;
    let greenMax = 13;
    let blueMax = 14;
    
    // Return power of a set.
    let analyseGame (line:string): int = 
        let game::setsLine::_ = line.Split(':') |> List.ofSeq;
        let _::gameNumber::_ = game.Split(' ') |> List.ofSeq;
        let setsStrings = setsLine.Split(';') |> List.ofSeq
        let sets = setsStrings |> List.map (
            fun setString -> 
                let subsets = setString.Split(',') |> List.ofSeq
                subsets |> List.fold (
                    fun set subset -> 
                        let n::color::_ = subset.Trim(' ').Split(' ') |> List.ofSeq
                        match color with 
                        | "red" -> {set with Red = n |> int }
                        | "green" -> {set with Green = n |> int }
                        | "blue" -> {set with Blue = n |> int }
                ) { Set.Blue = 0; Green = 0; Red = 0 }
            )
        
        let maxRed = sets |> List.map (fun s -> s.Red) |> List.max 
        let maxGreen = sets |> List.map (fun s -> s.Green) |> List.max 
        let maxBlue = sets |> List.map (fun s -> s.Blue) |> List.max 
        
        maxRed * maxGreen * maxBlue
        
    input |> List.map analyseGame |> List.sum |> int64

let day3_1 (input: string list) = 
    // NOTE: for simplicity we don't check input for empty list
    let rowsCount = input.Length
    let columnsCount = input[0].Length
    let matrix = Array2D.init rowsCount columnsCount (fun i j -> input[i][j])
    
    let rec getNumbersFromRow buffer index row: (int * int) list = 
        match row with 
        | "" -> match buffer with 
                | "" -> []
                | b -> [(b |> int, index - 1)]
        | r -> 
            let firstChar = row[0]
            match firstChar |> Char.IsDigit with
            | true -> getNumbersFromRow (buffer + firstChar.ToString()) (index + 1)  row[1..]  
            | false -> 
                match buffer with 
                | "" -> getNumbersFromRow  "" (index + 1) row[1..]
                | b -> ((b |> int), index - 1)::getNumbersFromRow  "" (index + 1) row[1..] 
                
    let isPart number row endColumn = 
        let numberString = number.ToString()
        let upperRow = if row = 0 then 0 else row - 1
        let bottomRow = if row = rowsCount then row else row + 1
        let leftColumn = if endColumn - numberString.Length + 1 = 0 then endColumn - numberString.Length + 1 else endColumn - numberString.Length
        let rightColumn = if endColumn = columnsCount then endColumn else endColumn + 1
        
        let subMatrix = matrix[upperRow..bottomRow, leftColumn..rightColumn]
        
        subMatrix |> Seq.cast<Char> |> List.ofSeq |> List.exists (fun x -> not (x |> Char.IsDigit) && x <> '.')
        
        
    let numbers = input |> List.map (getNumbersFromRow "" 0)
    //numbers |> Dump
    
    numbers |> List.mapi (fun rowIndex numbersRow -> 
                              numbersRow |> List.filter (fun (number, endIndex) -> isPart number rowIndex endIndex)) 
        |> List.collect (fun row -> row) |> List.map (fun (n, _) -> n) |> List.sum |> int64
 
let day3_2 (input: string list) = 
    // NOTE: for simplicity we don't check input for empty list
    let rowsCount = input.Length
    let columnsCount = input[0].Length
    let matrix = Array2D.init rowsCount columnsCount (fun i j -> input[i][j])
    
    let rec getNumbersFromRow buffer index row: (int * int) list = 
        match row with 
        | "" -> match buffer with 
                | "" -> []
                | b -> [(b |> int, index - 1)]
        | r -> 
            let firstChar = row[0]
            match firstChar |> Char.IsDigit with
            | true -> getNumbersFromRow (buffer + firstChar.ToString()) (index + 1)  row[1..]  
            | false -> 
                match buffer with 
                | "" -> getNumbersFromRow  "" (index + 1) row[1..]
                | b -> ((b |> int), index - 1)::getNumbersFromRow  "" (index + 1) row[1..] 
        
    let numbers = 
        input 
        |> List.map (getNumbersFromRow "" 0)
        |> List.mapi (fun rowIndex ns -> ns |> List.map (fun (n, colIndex) -> (n, rowIndex, colIndex)))
        |> List.collect (fun ns -> ns)
        
    let gears = 
        input 
            |> List.mapi (
                fun rowIndex row -> 
                    row 
                    |> List.ofSeq
                    |> List.mapi (fun i element -> (i, element)) 
                    |> List.filter (fun (i, element) -> element = '*')
                    |> List.map (fun (i, _) -> (rowIndex, i)))
            |> List.collect (fun gears -> gears)
            
    let isNumberNearGear gearRow gearColumn number = 
        let (n, rowIndex, endColumn) = number 
        let numberString = n.ToString()
        let upperRow = if rowIndex = 0 then 0 else rowIndex - 1
        let bottomRow = if rowIndex = rowsCount then rowIndex else rowIndex + 1
        let leftColumn = if endColumn - numberString.Length + 1 = 0 then endColumn - numberString.Length + 1 else endColumn - numberString.Length
        let rightColumn = if endColumn = columnsCount then endColumn else endColumn + 1
        
        // if gear within digit bounding box.
        upperRow <= gearRow && gearRow <= bottomRow && leftColumn <= gearColumn && gearColumn <= rightColumn
        
    let numbersNearGear gearRow gearColumn = 
        numbers |> List.filter (isNumberNearGear gearRow gearColumn) |> List.map (fun (n, _, _) -> n)
            
    gears 
        |> List.map (fun (row, column) -> (row, column, numbersNearGear row column))
        |> Dump
        |> List.filter (fun (_, _, ns) -> ns.Length = 2) 
        |> List.map (fun (_, _, ns) -> ns[0] * ns[1])
        |> List.sum
        |> int64
        
let day4_1 (input: string list) = 
    let getTwoListsFromRow (row: string) = 
        let _::numbers::_ = row.Split(':') |> List.ofSeq
        let winningNumbers::yourNumbers::_ = 
            numbers.Split('|') 
            |> List.ofSeq 
            |> List.map (fun ns -> ns.Split(' ') |> List.ofSeq |> List.filter (fun n -> n <> "") |> List.map (fun n -> n |> int))
        
        (winningNumbers, yourNumbers)
        
    let getYourWinningNumbers (winningNumbers, yourNumbers) =
        (winningNumbers |> Set.ofList) |> Set.intersect (yourNumbers |> Set.ofList) |> Set.toList
      
    let calculatePoints (yourWinningNumbers: int list) = 
        Math.Pow (2, yourWinningNumbers.Length - 1 |> float) |> int
        
    input 
        |> List.map (getTwoListsFromRow >> getYourWinningNumbers >> calculatePoints)
        |> List.sum
        |> int64
    
let day4_2 (input: string list) = 
    let getTwoListsFromRow (row: string) = 
        let _::numbers::_ = row.Split(':') |> List.ofSeq
        let winningNumbers::yourNumbers::_ = 
            numbers.Split('|') 
            |> List.ofSeq 
            |> List.map (fun ns -> ns.Split(' ') |> List.ofSeq |> List.filter (fun n -> n <> "") |> List.map (fun n -> n |> int))
        
        (winningNumbers, yourNumbers)
        
    let getYourWinningNumbers winningNumbers yourNumbers =
        (winningNumbers |> Set.ofList) |> Set.intersect (yourNumbers |> Set.ofList) |> Set.toList
    

    let cards = input |> List.mapi (fun index row -> row  |> getTwoListsFromRow |> fun (wn, yn) -> (index + 1, getYourWinningNumbers wn yn |> List.length))
    
    // Tail recursion in depth (in this case it will be up to 200 depth, and in width it is too long (slice and appned became bottleneck)).
    let rec countCards (acc: int) (cardsQueue: (int * int) list) = 
        match cardsQueue with 
        | [] -> acc
        | c::cs -> 
            let (cardNumber, winningNumbersCount) = c
            match winningNumbersCount with 
            | 0 -> countCards (acc + 1) cs
            | n -> 
                let copies = cards[cardNumber..cardNumber + n - 1]
                countCards (acc + 1) (copies @ cs)
                
    
    cards |> countCards 0 |> int64
    
let day5_1 (input: string list) = 
    /// Split a list into chunks using the specified separator
    /// This takes a list and returns a list of lists (chunks)
    /// that represent individual groups, separated by the given
    /// separator 'v'
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
    
    let parseCategory (categoryStrings: string list) = 
        categoryStrings[1..] |> List.map (
            fun cString -> 
                let row = cString.Split(' ') 
                (row[0] |> int64, row[1] |> int64, row[2] |> int64))
                
    let map input category = 
        match category |> List.tryFind (
            fun (d, s, r) -> s <= input && input <= s + r) with
        | Some (d, s, r) -> d + (input - s) 
        | None -> input
      
    let categoriesStrings = input |> splitBy ""
    let seeds = categoriesStrings[0].[0].Split(':').[1].Split(' ') |> List.ofArray |> List.filter (fun x -> x <> "") |> List.map (fun x -> x |> int64) 
    let categories = categoriesStrings[1..] |> List.map parseCategory
    
    seeds |> List.map (fun s -> categories |> List.fold (fun input category -> category |> map input) s) |> List.min |> int64
    
let day5_2_not_optimal (input: string list) = 
    /// Split a list into chunks using the specified separator
    /// This takes a list and returns a list of lists (chunks)
    /// that represent individual groups, separated by the given
    /// separator 'v'
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
    
    let parseCategory (categoryStrings: string list) = 
        categoryStrings[1..] |> List.map (
            fun cString -> 
                let row = cString.Split(' ') 
                (row[0] |> int64, row[1] |> int64, row[2] |> int64))
                
    let map input category = 
        match category |> List.tryFind (
            fun (d, s, r) -> s <= input && input <= s + r) with
        | Some (d, s, r) -> d + (input - s) 
        | None -> input
        
    let splitList list = List.foldBack (fun x (l,r) -> x::r, l) list ([],[])
    
    
      
    let categoriesStrings = input |> splitBy ""
    let seeds = categoriesStrings[0].[0].Split(':').[1].Split(' ') |> List.ofArray |> List.filter (fun x -> x <> "") |> List.map (fun x -> x |> int64) 
    let (indexes, ranges) = seeds |> splitList 
    // let seeds = List.zip indexes ranges |> List.map (fun (index, range) -> [for i in index..index + range -> i]) |> List.collect (fun x -> x)
    
    
    let categories = categoriesStrings[1..] |> List.map parseCategory
    let getLocation seed = categories |> List.fold (fun input category -> category |> map input) seed
    
    List.zip indexes ranges |> List.map (
        fun (index, range) -> 
            $"{index}, {range}" |> Dump |> ignore 
            let mutable min = Int64.MaxValue
            let mutable iter = index 
            while iter <= index + range do 
                min <- Math.Min(min, getLocation iter)
                iter <- iter + 1L
                
            min
            //let locations = seq { for i in index..index + range -> getLocation i }
            //locations |> Seq.min
            ) |> List.min
    
    //seeds |> List.map (fun s -> categories |> List.fold (fun input category -> category |> map input) s) |> List.min |> uint

let day5_2_optimization_now_fails (input: string list) = 
    /// Split a list into chunks using the specified separator
    /// This takes a list and returns a list of lists (chunks)
    /// that represent individual groups, separated by the given
    /// separator 'v'
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
    
    let parseCategory (categoryStrings: string list) = 
        categoryStrings[1..] |> List.map (
            fun cString -> 
                let row = cString.Split(' ') 
                (row[0] |> int64, row[1] |> int64, row[2] |> int64)) |> ResizeArray<(int64 * int64 * int64)>
            
                
    let map input (category: ResizeArray<_>) = 
        let mutable neededRow = None 
        for row in category do 
            let (d, s, r) = row 
            if s <= input && input <= s + r then
                neededRow <- Some row 
                
          
        match neededRow with 
        | Some (d, s, r) -> d + (input - s) 
        | None -> input
        
        
    let splitList list = List.foldBack (fun x (l,r) -> x::r, l) list ([],[])
    
    
      
    let categoriesStrings = input |> splitBy ""
    let seeds = categoriesStrings[0].[0].Split(':').[1].Split(' ') |> List.ofArray |> List.filter (fun x -> x <> "") |> List.map (fun x -> x |> int64) 
    let (indexes, ranges) = seeds |> splitList 
    // let seeds = List.zip indexes ranges |> List.map (fun (index, range) -> [for i in index..index + range -> i]) |> List.collect (fun x -> x)
    
    
    //let categories = categoriesStrings[1..] |> List.map parseCategory
    //let getLocation seed = categories |> List.fold (fun input category -> category |> map input) seed
    
    let categories = categoriesStrings[1..] |> List.map parseCategory |> ResizeArray<ResizeArray<(int64 * int64 * int64)>>
    let getLocation seed = 
        let mutable input = seed;
        for c in categories do
            input <- map input c 
        
        input
        
    
    List.zip indexes ranges |> List.map (
        fun (index, range) -> 
            $"{index}, {range}" |> Dump |> ignore 
            let mutable min = Int64.MaxValue
            let mutable iter = index 
            while iter <= index + range do 
                min <- Math.Min(min, getLocation iter)
                iter <- iter + 1L
                
            min
            //let locations = seq { for i in index..index + range -> getLocation i }
            //locations |> Seq.min
            ) |> List.min
    
    //seeds |> List.map (fun s -> categories |> List.fold (fun input category -> category |> map input) s) |> List.min |> uint
    
let day6_1 (input: string list) = 
    let times = input[0].Split(':').[1].Split(' ') |> Seq.filter (fun x -> x <> "") |> Seq.map (fun x -> x |> int) |> List.ofSeq
    let distances = input[1].Split(':').[1].Split(' ') |> Seq.filter (fun x -> x <> "") |> Seq.map (fun x -> x |> int) |> List.ofSeq
    let races = List.zip times distances 
    
    let calcWaysToWinRaice (time, distance) =
        let mutable chargeTime = 0
        let mutable winsCount = 0
        while chargeTime <= time do 
            let speed = chargeTime
            if (time - chargeTime) * speed > distance then 
                winsCount <- winsCount + 1
            chargeTime <- chargeTime + 1
                
           
        // 449550
        winsCount
    
    races |> List.map calcWaysToWinRaice |> List.fold (*) 1 |> int64
    
let day6_2 (input: string list) = 
    let time = input[0].Split(':').[1].Split(' ') |> Seq.filter (fun x -> x <> "") |> String.concat "" |> int64
    let distance = input[1].Split(':').[1].Split(' ') |> Seq.filter (fun x -> x <> "") |> String.concat "" |> int64
    
    let calcWaysToWinRace1 time distance =
        let mutable chargeTime = 0L
        let mutable winsCount = 0L
        while chargeTime <= time do 
            let speed = chargeTime
            if (time - chargeTime) * speed > distance then 
                winsCount <- winsCount + 1L
            chargeTime <- chargeTime + 1L
        winsCount 
            
    // NOTE: twise as long compared to simple version with mutations - without seq 4 vs 2 seconds, using List takes 14 seconds, so seq really gives result.
    let calcWaysToWinRace2 time distance =
        let times = seq {for i in 0L..time do i}
        times |> Seq.fold (fun wins t -> if (time - t) * t > distance then wins + 1L else wins) 0L
       
        // 28360140   
    
    calcWaysToWinRace2 time distance |> int64
   
let day7_1 (input: string list) = 
    let cardSet = ['2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A']
    
    let hands = input |> List.map (fun l -> (l.Split(' ').[0], l.Split(' ').[1] |> int))
    
    let detectType (hand: string) = 
        let cardGroups = hand |> List.ofSeq |> List.groupBy (fun x -> x)
        match cardGroups |> List.length with 
        | 5 -> 0 // High card
        | 4 -> 1 // One pair 
        | 3 -> 
            match cardGroups |> List.exists (fun (_, g) -> g |> List.length = 3) with 
            | false -> 2 // Two pairs 
            | true -> 3 // Three of a kind
            
        | 2 ->
            match cardGroups |> List.exists (fun (_, g) -> g |> List.length = 4) with 
            | false -> 4 // Full house
            | true -> 5 // Four of a kind 
        | 1 -> 6 // Five of a kind
    
    let compareCard left right = 
        compare (cardSet |> List.findIndex (fun x -> x = left)) (cardSet |> List.findIndex (fun x -> x = right))
        
    let rec compareHandByCards (left:string) (right:string) = 
        let cardSets = List.zip (left |> Seq.toList) (right |> Seq.toList)
        match cardSets with 
        | [] -> 0 
        | (l, r)::set -> 
            match compareCard l r with 
            | 0 -> compareHandByCards left[1..] right[1..]
            | r -> r
        
        
    let compareHands left right =
        let (lHand, _) = left 
        let (rHand, _) = right
        match compare (detectType lHand) (detectType rHand) with 
        | 0 -> compareHandByCards lHand rHand 
        | x -> x
        
    hands |> List.sortWith compareHands |> List.mapi (fun index (_, bid) -> (index + 1) * bid) |> List.sum |> int64
   
let day7_2 (input: string list) = 
    let cardSet = ['J'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'Q'; 'K'; 'A']
    
    let hands = input |> List.map (fun l -> (l.Split(' ').[0], l.Split(' ').[1] |> int))
    
    let detectType (hand: string) = 
        let detctTypeWithoutJoker (hand: string) = 
            let cardGroups = hand |> List.ofSeq |> List.groupBy (fun x -> x)
            match cardGroups |> List.length with 
            | 5 -> 0 // High card
            | 4 -> 1 // One pair 
            | 3 -> 
                match cardGroups |> List.exists (fun (_, g) -> g |> List.length = 3) with 
                | false -> 2 // Two pairs 
                | true -> 3 // Three of a kind
                
            | 2 ->
                match cardGroups |> List.exists (fun (_, g) -> g |> List.length = 4) with 
                | false -> 4 // Full house
                | true -> 5 // Four of a kind 
            | 1 -> 6 // Five of a kind
        
        // Generate possible combinations without Joker
        let rec detectHands hand = 
            match hand with 
            | "" -> []
            | _ -> 
                let c = hand[0]
                let leftHand = hand[1..]
                match (c, leftHand) with 
                | ('J', "") -> cardSet |> List.map Char.ToString
                | (_, "") -> [c |> Char.ToString]
                | ('J', _) -> detectHands leftHand |> List.collect (fun x -> cardSet |> List.map (fun y -> (y |> Char.ToString) + x))
                | (_, _) -> detectHands leftHand |> List.map (fun x -> (c |> Char.ToString) + x) 
                
                
        hand |> detectHands |> List.map detctTypeWithoutJoker |> List.max    
            
    
    let compareCard left right = 
        compare (cardSet |> List.findIndex (fun x -> x = left)) (cardSet |> List.findIndex (fun x -> x = right))
        
    let rec compareHandByCards (left:string) (right:string) = 
        let cardSets = List.zip (left |> Seq.toList) (right |> Seq.toList)
        match cardSets with 
        | [] -> 0 
        | (l, r)::set -> 
            match compareCard l r with 
            | 0 -> compareHandByCards left[1..] right[1..]
            | r -> r
        
        
    let compareHands left right =
        let (lHand, _) = left 
        let (rHand, _) = right
        match compare (detectType lHand) (detectType rHand) with 
        | 0 -> compareHandByCards lHand rHand 
        | x -> x
        
    hands |> List.sortWith compareHands |> List.mapi (fun index (_, bid) -> (index + 1) * bid) |> List.sum |> int64
        
let day8_1 (input: string list) = 
    let parseNode str = 
        let lToStr l = l |> Array.ofList |> String
        let nodes = str |> Seq.filter (fun x -> x |> Char.IsLetter) |> Seq.toList 
        (nodes[0..2] |> lToStr, nodes[3..5] |> lToStr, nodes[6..8] |> lToStr)

    let instr = input[0] |> Seq.toList
    let nodesList = input[2..] |> List.map (parseNode >> (fun (n, ln, rn) -> (n, (ln, rn)))) 
    let nodes = nodesList |> Map.ofList
    
    let rec findEnd node ins steps = 
        match (node, ins) with 
        | "ZZZ", _ -> steps
        | n, [] -> findEnd n instr steps
        | _, x::xs ->  
            let (l, r) = nodes[node]
            match x with 
            | 'L' -> findEnd l xs (steps + 1L)
            | 'R' -> findEnd r xs (steps + 1L)
    
    findEnd "AAA" instr 0 
    
 
let day8_2 (input: string list) = 
    let parseNode str = 
        let lToStr l = l |> Array.ofList |> String |> string
        let nodes = str |> Seq.filter (fun x -> (x |> Char.IsLetter) || (x |> Char.IsDigit)) |> Seq.toList 
        (nodes[0..2] |> lToStr, nodes[3..5] |> lToStr, nodes[6..8] |> lToStr)

    let instr = input[0] |> Seq.toList
    let nodesList = input[2..] |> List.map (parseNode >> (fun (n, ln, rn) -> (n, (ln, rn)))) 
    let nodes = nodesList |> Map.ofList
    
    let rec findEnd (node: string) ins steps = 
        match node[2] = 'Z' with 
        | true -> steps 
        | false -> 
            match ins with 
            | [] -> findEnd node instr steps 
            | x::xs ->  
                let (l, r) = nodes[node]
                match x with 
                | 'L' -> findEnd l xs (steps + 1L) 
                | 'R' -> findEnd r xs (steps + 1L)
       
    let rec gcd (x:int64) (y:int64) = if y = 0 then abs x else gcd y (x % y)
    let lcm (x:int64) (y:int64) = x * y / (gcd x y)
    
    let startNodes = nodesList |> List.map (fun (n, _) -> n) |> List.filter (fun n -> n[2] = 'A')
    startNodes |> List.map (fun n -> findEnd n instr 0L) |> List.fold (fun s steps -> lcm s steps) 1L 
    
let solvers = [
    day1_1;
    day1_2;
    day2_1;
    day2_2;
    day3_1;
    day3_2;
    day4_1;
    day4_2;   
    day5_1;
    day5_2_not_optimal;
    day6_1;
    day6_2;
    day7_1;
    day7_2;
    day8_1;
    day8_2
]

let main() = 
    let days = [1; 2; 3; 4; 5; 6; 7; 8]
    let day = Util.ReadLine("Day", days |> List.last, days)
    let task = Util.ReadLine("Task", 1, [1; 2]);
    
    $"Solving for day {day}, task {task}" |> Dump |> ignore 
    let solver = solvers[(day - 1) * 2 + task - 1]    
    
    let result = day |> Utils.Core.readInputLines (Path.GetDirectoryName(Util.CurrentQueryPath)) |> solver 
    $"Result: {result}" |> Dump |> ignore

main ()





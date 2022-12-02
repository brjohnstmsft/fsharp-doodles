open System
open System.IO

type Elf = { Calories : int list }
with member this.GetTotalCalories() = List.sum this.Calories

type ElvesInProgress =
    { CompleteElves : Elf list;
      CurrentElf : Elf }

type ElfCollector =
    | InProgress of ElvesInProgress
    | Complete of Elf list
with
    static member NoElves = InProgress { CompleteElves = []; CurrentElf = { Calories = [] } }

    member this.AddCalories calories =
        match this with
        | InProgress p -> InProgress { p with CurrentElf = { p.CurrentElf with Calories = calories::p.CurrentElf.Calories } }
        | Complete elves -> InProgress { CompleteElves = elves; CurrentElf = { Calories = [calories] } }

    member this.Result =
        match this with
        | InProgress { CompleteElves = elves; CurrentElf = elf } -> (elf::elves)
        | Complete elves -> elves

let readLine acc line =
    if String.IsNullOrWhiteSpace(line) then
        match acc with
        | InProgress { CompleteElves = elves; CurrentElf = elf } -> Complete (elf::elves)
        | Complete _ -> acc
    else int line |> acc.AddCalories

let readElves path =
    let elfCollector =
        File.ReadAllLines(path)
        |> Array.fold readLine ElfCollector.NoElves

    elfCollector.Result

let rankedElfCalories =
    readElves "input.txt"
    |> List.map (fun e -> e.GetTotalCalories())
    |> List.sortDescending

let maxCalorieCount = rankedElfCalories |> List.head

printfn "Max elf calories: %d" maxCalorieCount

let topThreeElfCalories =
    rankedElfCalories
    |> List.take 3
    |> List.sum

printfn "Calories of top three elves: %d" topThreeElfCalories

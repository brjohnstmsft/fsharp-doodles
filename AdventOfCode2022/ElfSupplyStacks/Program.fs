open System
open System.IO
open System.Text.RegularExpressions

type Step = { Count: int; From: int; To: int }
with
    static member Parse(s) =
        let m = Regex.Match(s, @"move (?<Count>\d+) from (?<From>\d+) to (?<To>\d+)")
        let getAsInt (groupName: string) = m.Groups[groupName].Captures[0].Value |> int
        { Count = getAsInt "Count"; From = (getAsInt "From") - 1; To = (getAsInt "To") - 1 }

    member this.ExecuteOneByOne(stacks: char list[]) =
        let mutable fromStack = stacks[this.From]
        let mutable toStack = stacks[this.To]

        for _ = 1 to this.Count do
            let c = List.head fromStack
            fromStack <- List.tail fromStack
            toStack <- c::toStack

        stacks[this.From] <- fromStack
        stacks[this.To] <- toStack

    member this.ExecuteMany(stacks: char list[]) =
        let fromStack = stacks[this.From]
        let toStack = stacks[this.To]

        let moved = List.take this.Count fromStack

        stacks[this.From] <- List.skip this.Count fromStack
        stacks[this.To] <- moved @ toStack

let lines = File.ReadAllLines("input.txt")

let initialStateLines = lines |> Array.takeWhile (fun l -> not (String.IsNullOrWhiteSpace(l)))

let initialStateMatrix =
    initialStateLines
    |> Array.map (fun l -> l.ToCharArray())
    |> array2D

let stacks =
    [|
        for col = 0 to initialStateMatrix.GetLength(1) - 1 do
            let lastRow = initialStateMatrix.GetLength(0) - 1
            if Char.IsDigit initialStateMatrix[lastRow,col] then
                [
                    for row = 0 to lastRow - 1 do
                        let c = initialStateMatrix[row,col]
                        if Char.IsLetter c then c
                ]
    |]

let steps = lines[initialStateLines.Length + 1..] |> Array.map Step.Parse

let stacksPartOne = Array.copy stacks

for step in steps do
    step.ExecuteOneByOne(stacksPartOne)
    
let topsPartOne =
    stacksPartOne
    |> Array.map List.head
    |> String

printfn $"Top of stacks after steps by CraneMover 9000: {topsPartOne}"

let stacksPartTwo = Array.copy stacks

for step in steps do
    step.ExecuteMany(stacksPartTwo)
    
let topsPartTwo =
    stacksPartTwo
    |> Array.map List.head
    |> String

printfn $"Top of stacks after steps by CraneMover 9001: {topsPartTwo}"

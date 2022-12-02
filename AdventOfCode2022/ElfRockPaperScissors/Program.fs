open System.IO

type Outcome = Win | Lose | Draw
with
    static member Parse(c) =
        match c with
        | 'X' -> Lose
        | 'Y' -> Draw
        | 'Z' -> Win
        | _ -> invalidArg (nameof c) "Outcome has to be X, Y, or Z"

    member this.Score =
        match this with
        | Win -> 6
        | Lose -> 0
        | Draw -> 3

type Shape = Rock | Paper | Scissors
with
    static member ForOpponent(c) =
        match c with
        | 'A' -> Rock
        | 'B' -> Paper
        | 'C' -> Scissors
        | _ -> invalidArg (nameof c) "Opponent's shape has to be A, B, or C"

    static member ForPlayer(c) =
        match c with
        | 'X' -> Rock
        | 'Y' -> Paper
        | 'Z' -> Scissors
        | _ -> invalidArg (nameof c) "Player's shape has to be X, Y, or Z"

    member this.Play(shape) =
        match this, shape with
        | Paper, Rock -> Win
        | Rock, Scissors -> Win
        | Scissors, Paper -> Win
        | _, _ when this = shape -> Draw
        | _, _ -> Lose

    member this.RequiredShapeForPlayer(desiredOutcome) =
        match desiredOutcome with
        | Win ->
            match this with
            | Rock -> Paper
            | Paper -> Scissors
            | Scissors -> Rock
        | Draw -> this
        | Lose ->
            match this with
            | Rock -> Scissors
            | Paper -> Rock
            | Scissors -> Paper

    member this.Score =
        match this with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

let parseShapes (s: string) =
    let opponentShape = Shape.ForOpponent(s[0])
    let playerShape = Shape.ForPlayer(s[2])
    (opponentShape, playerShape)

let scoreRound (opponentShape: Shape, playerShape: Shape) =
    let outcome = playerShape.Play(opponentShape)
    let shapeScore = playerShape.Score
    let outcomeScore = outcome.Score
    shapeScore + outcomeScore

let lines = File.ReadAllLines("input.txt")

let totalFirstScore =
    lines
    |> Array.map (parseShapes >> scoreRound)
    |> Array.sum

printfn "Total first score is: %d" totalFirstScore

let parseRound (s: string) =
    let opponentShape = Shape.ForOpponent(s[0])
    let desiredOutcome = Outcome.Parse(s[2])
    let playerShape = opponentShape.RequiredShapeForPlayer(desiredOutcome)
    (opponentShape, playerShape)

let totalSecondScore =
    lines
    |> Array.map (parseRound >> scoreRound)
    |> Array.sum

printfn "Total second score is: %d" totalSecondScore

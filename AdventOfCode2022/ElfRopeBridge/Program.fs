open System.IO

type Direction = Up | Down | Left | Right

let parseDirection s =
    match s with
    | "U" -> Up
    | "D" -> Down
    | "L" -> Left
    | "R" -> Right
    | _ -> invalidArg (nameof s) "Invalid input"

let parseMove (s: string) =
    let tokens = s.Split(' ')
    (parseDirection tokens[0], int tokens[1])

let move direction (x, y) =
    match direction with
    | Up -> (x, y + 1)
    | Down -> (x, y - 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

let getSurrounds coord =
    seq {
        coord |> move Up
        coord |> move Down
        coord |> move Left
        coord |> move Right
        coord |> move Up |> move Left
        coord |> move Up |> move Right
        coord |> move Down |> move Left
        coord |> move Down |> move Right
    }

let getFollowOptions coord =
    seq {
        // Right in the middle
        (coord, [])
        // Cardinal directions
        (coord |> move Up |> move Up, [Up])
        (coord |> move Down |> move Down, [Down])
        (coord |> move Left |> move Left, [Left])
        (coord |> move Right |> move Right, [Right])
        // Points adjacent to cardinal directions
        (coord |> move Up |> move Up |> move Left, [Up; Left])
        (coord |> move Up |> move Up |> move Right, [Up; Right])
        (coord |> move Down |> move Down |> move Left, [Down; Left])
        (coord |> move Down |> move Down |> move Right, [Down; Right])
        (coord |> move Left |> move Left |> move Up, [Left; Up])
        (coord |> move Left |> move Left |> move Down, [Left; Down])
        (coord |> move Right |> move Right|> move Up, [Right; Up])
        (coord |> move Right |> move Right |> move Down, [Right; Down])
        // Corners
        (coord |> move Up |> move Left |> move Up |> move Left, [Up; Left])
        (coord |> move Up |> move Right |> move Up |> move Right, [Up; Right])
        (coord |> move Down |> move Left |> move Down |> move Left, [Down; Left])
        (coord |> move Down |> move Right |> move Down |> move Right, [Down; Right])
    }

let follow head tail =
    if tail |> getSurrounds |> Seq.contains head then
        tail
    else
        let (_, moves) =
            tail
            |> getFollowOptions
            |> Seq.filter (fun (coord, _) -> coord = head)
            |> Seq.exactlyOne

        moves |> List.fold (fun coord direction -> move direction coord) tail

let executeMove direction moveHistory _ =
    match moveHistory with
    | (head::rest)::_ ->
        let newHead = head |> move direction
        let moveOneLink (lastHead, newRope) link =
            let newLink = link |> follow lastHead
            (newLink, newLink::newRope)

        let (_, newRest) = rest |> List.fold moveOneLink (newHead, [])
        (newHead::(List.rev newRest))::moveHistory
    | _ -> failwith "Invalid input"

let executeMoves moveHistory (direction, count) =
    [1..count] |> List.fold (executeMove direction) moveHistory

let getUniqueTailPositions length moves =
    let initialPositions = [List.init length (fun _ -> (0, 0))]    // Start at (0, 0) since it's arbitrary anyway.
    let moveHistory = moves |> Array.fold executeMoves initialPositions
    let tailCoords = moveHistory |> List.map (fun ropes -> ropes |> List.rev |> List.head)
    tailCoords |> List.distinct |> List.length

let lines = File.ReadAllLines("input.txt")
let moves = lines |> Array.map parseMove

printfn $"Part 1: Unique tail positions: {moves |> getUniqueTailPositions 2}"

printfn $"Part 1: Unique tail positions: {moves |> getUniqueTailPositions 10}"

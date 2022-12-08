open System
open System.IO

type TreeVisibility = Visible | Hidden | Unknown

type Tree = { Height: int; mutable Visibility: TreeVisibility; mutable Distances: int * int * int * int }
with
    static member Parse(c) = { Height = int c - int '0'; Visibility = Unknown; Distances = (0, 0, 0, 0) }

    member this.ScenicScore =
        let (x1, x2, x3, x4) = this.Distances
        x1 * x2 * x3 * x4

type Forest = { Grid: Tree[,] }
with
    member this.LastColumn = this.Grid.GetLength(1) - 1
    member this.LastRow = this.Grid.GetLength(0) - 1

type TreePosition = { Row: int; Column: int }

type TreeRef = { Pos: TreePosition; Forest: Forest }

type Direction = Up | Down | Left | Right

module Tree =

    let get tree = tree.Forest.Grid[tree.Pos.Row, tree.Pos.Column]

    let getHeight tree = (get tree).Height

    let setVisibility visibility tree =
        let currentVisibility = &(get tree).Visibility
        match currentVisibility, visibility with
        | _, Unknown -> ()      // Can't set anything back to Unknown
        | Visible, _ -> ()      // Once a tree is deemed visible, it can't be hidden from another direction
        | _ -> currentVisibility <- visibility

    let setViewDistance distance direction tree =
        let currentDistances = &(get tree).Distances
        let (x1, x2, x3, x4) = currentDistances
        match direction with
        | Up -> currentDistances <- (distance, x2, x3, x4)
        | Down -> currentDistances <- (x1, distance, x3, x4)
        | Left -> currentDistances <- (x1, x2, distance, x4)
        | Right -> currentDistances <- (x1, x2, x3, distance)

module Forest =

    let tryAdvance pos direction (forest: Forest) =
        match direction, pos with
        | Up,       { Row = 0;      Column = _ } -> None
        | Down,     { Row = row;    Column = _ } when row = forest.LastRow -> None
        | Left,     { Row = _;      Column = 0 } -> None
        | Right,    { Row = _;      Column = col } when col = forest.LastColumn -> None
        | Up,       { Row = row;    Column = _ } -> Some { pos with Row = row - 1 }
        | Down,     { Row = row;    Column = _ } -> Some { pos with Row = row + 1 }
        | Left,     { Row = _;      Column = col } -> Some { pos with Column = col - 1 }
        | Right,    { Row = _;      Column = col } -> Some { pos with Column = col + 1 }

    let getInitialPosition index direction (forest: Forest) =
        match direction with
        | Up ->     { Row = forest.LastRow; Column = index }
        | Down ->   { Row = 0;              Column = index }
        | Left ->   { Row = index;          Column = forest.LastColumn }
        | Right ->  { Row = index;          Column = 0 }

    let traverseSlice index (direction: Direction) (forest: Forest) =
        let mutable pos = getInitialPosition index direction forest
        let mutable finished = false
        seq {
            while not finished do
                { Pos = pos; Forest = forest }

                match tryAdvance pos direction forest with
                | Some newPos -> pos <- newPos
                | None -> finished <- true
        }

    let countBy predicate forest =
        let mutable count = 0
        let counter tree = if predicate tree then count <- count + 1 else ()
        forest.Grid |> Array2D.iter counter
        count

    let maxBy selector forest =
        let mutable max = None
        let compare tree =
            let n = selector tree
            match max with
            | Some m when n > m -> max <- Some n
            | None -> max <- Some n
            | _ -> ()

        forest.Grid |> Array2D.iter compare
        max.Value

let forest =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line.ToCharArray() |> Array.map Tree.Parse)
    |> array2D
    |> (fun g -> { Grid = g })

let determineStatsForSlice index direction =
    let determineStatsForTree (maxHeight, lastHeights) tree =
        let treeHeight = Tree.getHeight tree
        
        let visibility = if treeHeight > maxHeight then Visible else Hidden
        Tree.setVisibility visibility tree
        
        let distance =
            let nearestTallTree =
                lastHeights |> List.tryFindIndex (fun h -> treeHeight <= h)

            match nearestTallTree with
            | Some i -> i + 1
            | None -> List.length lastHeights

        Tree.setViewDistance distance direction tree
        
        (Math.Max(maxHeight, treeHeight), treeHeight::lastHeights)

    forest
    |> Forest.traverseSlice index direction
    |> Seq.fold determineStatsForTree (-1, [])  // Start with negative height so edges are always visible.
    |> ignore   // Discard the accumulated state since we're only interested in the side effects.

for row = 0 to forest.LastRow do
    for direction in [Left; Right] do
        determineStatsForSlice row direction

for column = 0 to forest.LastColumn do
    for direction in [Up; Down] do
        determineStatsForSlice column direction

let countOfVisibleTrees = forest |> Forest.countBy (fun tree -> tree.Visibility = Visible)

printfn $"Number of visible trees: {countOfVisibleTrees}"

let maxScenicScore = forest |> Forest.maxBy (fun tree -> tree.ScenicScore)

printfn $"Max scenic score: {maxScenicScore}"

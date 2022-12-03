open System
open System.IO

let findCommonItemType sacks =
    sacks
    |> Seq.map (fun s -> Set.ofArray s)
    |> Set.intersectMany
    |> Seq.exactlyOne

let findSplitItem (s: string) =
    let (firstCompartment, secondCompartment) = s.ToCharArray() |> Array.splitAt (s.Length / 2)
    [firstCompartment; secondCompartment] |> findCommonItemType

let prioritize c =
    if Char.IsLower c then (int c) - (int 'a') + 1
    else (int c) - (int 'A') + 27

let lines = File.ReadAllLines("input.txt")

let prioritiesPerElf = lines |> Array.map (findSplitItem >> prioritize)

printfn $"Sum of priorities of items split across compartments: {Array.sum prioritiesPerElf}"

let findBadge (elfGroup: string[]) =
    elfGroup
    |> Array.map (fun s -> s.ToCharArray())
    |> findCommonItemType

let prioritiesPerGroup =
    lines
    |> Seq.chunkBySize 3
    |> Seq.map (findBadge >> prioritize)

printfn $"Sum of priorities of badge items across groups: {Seq.sum prioritiesPerGroup}"

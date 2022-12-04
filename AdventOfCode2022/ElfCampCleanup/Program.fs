open System.IO

type SectionRange =
    { Start: int
      End: int }
with
    static member Parse(s: string) =
        let parts = s.Split('-')
        { Start = parts[0] |> int; End = parts[1] |> int }

    member this.FullyContains({ Start = otherStart; End = otherEnd }) =
        this.Start <= otherStart && otherEnd <= this.End

    member this.IsDisjointWith({ Start = otherStart; End = otherEnd }) =
        this.End < otherStart || otherEnd < this.Start

    member this.Overlaps(other) = not (this.IsDisjointWith(other))

let parseLine (s: string) =
    let ranges = s.Split(',')
    (SectionRange.Parse(ranges[0]), SectionRange.Parse(ranges[1]))

let lines = File.ReadAllLines("input.txt")

let allAssignments = Array.map parseLine lines

let fullyOverlappingAssignments =
    allAssignments
    |> Array.filter (fun (firstRange, secondRange) -> firstRange.FullyContains(secondRange) || secondRange.FullyContains(firstRange))

printfn $"Input has {fullyOverlappingAssignments.Length} fully overlapping assignments."

let allOverlappingAssignments =
    allAssignments
    |> Array.filter (fun (firstRange, secondRange) -> firstRange.Overlaps(secondRange))

printfn $"Input has {allOverlappingAssignments.Length} overlapping assignments."

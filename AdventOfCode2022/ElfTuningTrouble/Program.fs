open System
open System.IO

let findUniqueSequence desiredSize (buffer: char[]) =
    let mutable start = 0
    let mutable end' = 0
    let mutable found = false
    while end' < buffer.Length && not found do
        let newChar = buffer[end']
        let length = end' - start + 1
        let dupIndex = Array.IndexOf(buffer, newChar, start, length - 1)
        if dupIndex <> -1 then
            start <- dupIndex + 1
        else if length = desiredSize then
            found <- true

        end' <- end' + 1

    if not found then failwith "Invalid buffer"
    else end'

let findHeader = findUniqueSequence 4
let findMessage = findUniqueSequence 14

let buffer = File.ReadAllText("input.txt").ToCharArray()

printfn $"Found header after reading {findHeader buffer} characters."

printfn $"Found message after reading {findMessage buffer} characters."

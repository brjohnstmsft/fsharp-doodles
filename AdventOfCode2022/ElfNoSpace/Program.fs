open System.IO

type Command =
    | EnterDirectory of string
    | LeaveDirectory
    | ListDirectory
with
    static member TryParse(s: string) =
        let tokens = s.Split(' ')
        if tokens[0] <> "$" then None
        else
            match tokens[1] with
            | "cd" when tokens[2] = ".." -> Some LeaveDirectory
            | "cd" -> EnterDirectory tokens[2] |> Some
            | "ls" -> Some ListDirectory
            | _ -> invalidArg (nameof s) "Invalid command"

type FileInfo = { Size: int; FileName: string }

type Item =
    | File of FileInfo
    | Directory of string
with
    static member Parse(s: string) =
        let tokens = s.Split(' ')
        match tokens[0] with
        | "dir" -> Directory tokens[1]
        | _ -> File { Size = int tokens[0]; FileName = tokens[1] }

type SessionInput =
    | Command of Command
    | Listing of Item
with
    static member Parse(s: string) =
        match Command.TryParse(s) with
        | Some command -> Command command
        | None -> Item.Parse(s) |> Listing

type DirectoryNode = { DirectoryName: string; Children: Map<string, Node> }
with
    static member Root = { DirectoryName = "/"; Children = Map.empty }
and Node =
    | FileNode of FileInfo
    | DirectoryNode of DirectoryNode

[<AutoOpen>]
module FileSystem =

    let rec createPath f path dirNode =
        let recurse childPath childDirNode =
            let newOrUpdateChildDirNode = childDirNode |> createPath f childPath
            let key = newOrUpdateChildDirNode.DirectoryName
            let value = DirectoryNode newOrUpdateChildDirNode
            { dirNode with Children = dirNode.Children |> Map.add key value }

        match path with
        | [] -> f dirNode
        | name::subPath ->
            match dirNode.Children.TryFind(name) with
            | Some (FileNode _) -> invalidArg (nameof path) "A file exists in the middle of the path."
            | Some (DirectoryNode existingSubDirNode) -> existingSubDirNode |> recurse subPath
            | None -> { DirectoryName = name; Children = Map.empty } |> recurse subPath

    let ensurePath = createPath id

    let createOrUpdateFile fileInfo path dirNode =
        let addOrUpdateFile = function
            | Some (FileNode _) -> FileNode fileInfo |> Some
            | Some (DirectoryNode _) -> invalidArg (nameof path) "A directory with that name already exists."
            | None -> FileNode fileInfo |> Some

        let addFileEntryToDirectory dir =
            { dir with Children = dir.Children |> Map.change fileInfo.FileName addOrUpdateFile }

        createPath addFileEntryToDirectory path dirNode

    let traverse fFile fDir dirNode =
        let rec doTraverse node =
            match node with
            | FileNode file -> fFile file
            | DirectoryNode dir ->
                let results = dir.Children.Values |> Seq.map doTraverse
                fDir results dir

        DirectoryNode dirNode |> doTraverse

    let chooseDirectories chooseFunc dirNode =
        let onFile _ = []
        let onDir resultSeq dir =
            let results = Seq.concat resultSeq |> List.ofSeq
            match chooseFunc dir with
            | Some result -> result::results
            | None -> results
        
        dirNode |> traverse onFile onDir
    
    let getTotalSize dirNode =
        let getFileSize file = file.Size
        let getDirSize sizes _ = Seq.sum sizes
        dirNode |> traverse getFileSize getDirSize

type Session = { FileSystem: DirectoryNode; CurrentDirectoryStack: string list }
with
    static member Initial = { FileSystem = DirectoryNode.Root; CurrentDirectoryStack = [] }

    member this.CurrentDirectoryPath = List.rev this.CurrentDirectoryStack

    member this.Execute(input) =
        match input with
        | Command (EnterDirectory directoryName) ->
            match directoryName with
            | "/" -> { this with CurrentDirectoryStack = [] }
            | _ -> { this with CurrentDirectoryStack = directoryName::this.CurrentDirectoryStack }
        | Command LeaveDirectory ->
            match this.CurrentDirectoryStack with
            | _::ds -> { this with CurrentDirectoryStack = ds }
            | [] -> this
        | Command ListDirectory -> this
        | Listing (File file) -> { this with FileSystem = this.FileSystem |> createOrUpdateFile file this.CurrentDirectoryPath }        
        | Listing (Directory directoryName) ->
            let directoryPath = directoryName::this.CurrentDirectoryStack |> List.rev
            { this with FileSystem = this.FileSystem |> ensurePath directoryPath }

let processLine (session: Session) line = SessionInput.Parse(line) |> session.Execute

let lines = File.ReadAllLines("input.txt")

let session = lines |> Array.fold processLine Session.Initial

let sizeOfSmallDirectories =
    let getTotalSizeIfSmall dirNode =
        let totalDirSize = getTotalSize dirNode
        if totalDirSize <= 100_000 then Some totalDirSize else None

    session.FileSystem |> chooseDirectories getTotalSizeIfSmall |> List.sum

printfn $"Total size of all directories less than 100000 bytes: {sizeOfSmallDirectories}"

let usedSpace = getTotalSize session.FileSystem
let freeSpace = 70_000_000 - usedSpace

let candidateForDeletion =
    let getIfCandidateForDeletion dirNode =
        let totalDirSize = getTotalSize dirNode
        if totalDirSize + freeSpace >= 30_000_000 then Some totalDirSize else None

    session.FileSystem |> chooseDirectories getIfCandidateForDeletion |> List.min

printfn $"Candidate for deletion is of size {candidateForDeletion} bytes."

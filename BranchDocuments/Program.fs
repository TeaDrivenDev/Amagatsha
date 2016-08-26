open System
open System.Collections.Generic
open System.IO

[<Literal>]
let StorageFileName = "branchdocuments";

[<Literal>]
let DocumentWindowPositionsMcdfKey = "DocumentWindowPositions"

let asSnd first second = first, second

let toDictionary data =
    let dictionary = Dictionary<_, _>()

    data
    |> Seq.iter (fun (key, value) -> dictionary.[key] <- value)

    dictionary

let prefixFileName prefix filePath =
    Path.Combine(Path.GetDirectoryName(filePath), prefix + Path.GetFileName(filePath))

let (|KeyValuePair|) (kvp : KeyValuePair<_, _>) = kvp.Key, kvp.Value

module Solution =
    let findSuo directory solutionName =
        [
            Path.Combine(directory, ".vs", solutionName, "v14", ".suo")
            Path.Combine(directory, sprintf "%s.v12.suo" solutionName)
        ]
        |> List.tryFind File.Exists

    let findSolutions directory = Directory.EnumerateFiles(directory, "*.sln")

    let getBranchName path =
        let directory =
            if File.GetAttributes path &&& FileAttributes.Directory = FileAttributes.Directory
            then path
            else Path.GetDirectoryName path

        try
            directory |> Fake.Git.Information.getBranchName |> Some
        with :? ArgumentException -> None

    let splitPath solutionPath =
        Path.GetDirectoryName solutionPath, Path.GetFileNameWithoutExtension solutionPath

module Mcdf =
    let readStream (path : string) streamName =
        use file = new OpenMcdf.CompoundFile(path)

        let stream = file.RootStorage.GetStream streamName

        let data = stream.GetData()
        file.Close()

        data

    let readSolutionDocuments suoFilepath = readStream suoFilepath DocumentWindowPositionsMcdfKey

    let replaceStream (path : string) streamName data =
        use file = new OpenMcdf.CompoundFile(path,
                                             OpenMcdf.CFSUpdateMode.Update,
                                             OpenMcdf.CFSConfiguration.Default)

        let stream = file.RootStorage.GetStream streamName

        data |> Array.length |> int64 |> stream.Resize

        stream.Write(data, 0L)

        let tempFilePath = path |> prefixFileName "x"
        file.Save tempFilePath
        file.Close()

        File.Delete path
        File.Move(tempFilePath, path)

module Storage = 
    let getSolutionStorageFileName solutionName = sprintf "%s.%s" solutionName StorageFileName

    let readSettings directory solutionName =
        let storageFileName = Path.Combine(directory, getSolutionStorageFileName solutionName)

        if File.Exists storageFileName
        then File.ReadAllLines storageFileName
             |> Array.map (fun s ->
                let [| key; data |] = s.Split(':')
                key, Convert.FromBase64String data)
        else [| |]
        |> toDictionary

    let writeSettings directory solutionName (data : IDictionary<_, byte array>) =
        let storageFileName = Path.Combine(directory, getSolutionStorageFileName solutionName)

        data
        |> Seq.map (fun (KeyValuePair (key, value)) ->
            value |> Convert.ToBase64String |> sprintf "%s:%s" key)
        |> asSnd storageFileName
        |> File.WriteAllLines

    let update directory solutionName branch suoFileName (settings : IDictionary<_, _>) =
        let documents = Mcdf.readSolutionDocuments suoFileName
        settings.[branch] <- documents

        writeSettings directory solutionName settings

    let restore directory solutionName branch suoFileName (settings : IDictionary<_, _>) =
        match settings.TryGetValue branch with
        | true, data -> data |> Mcdf.replaceStream suoFileName DocumentWindowPositionsMcdfKey
        | false, _->
            printfn
                "No document window data found for solution '%s' and branch '%s'"
                solutionName
                branch

    let withSettings action branch solutionPath =
        let directory, solutionName = Solution.splitPath solutionPath

        match Solution.findSuo directory solutionName with
        | Some suoFileName ->
            readSettings directory solutionName
            |> action directory solutionName branch suoFileName
        | None -> printfn "No .suo file found for solution '%s'" solutionName

open Storage

[<EntryPoint>]
let main argv = 
    let directory = Environment.CurrentDirectory

    match Solution.getBranchName directory with
    | Some branch ->
        match argv with
        | [| "save" |] -> Some (update, "Saved")
        | [| "restore" |] -> Some (restore, "Restored")
        | _ -> None
        |> function
            | Some (action, message) ->
                directory
                |> Solution.findSolutions
                |> Seq.iter (withSettings action branch)

                printfn "%s document windows for branch '%s'." message branch
            | None -> printfn "Unknown option '%A'" argv
    | None -> printfn "Directory not under Git version control"

    0

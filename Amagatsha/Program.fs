open System
open System.Collections.Generic
open System.IO

[<Literal>]
let StorageFileSuffix = "branchdocuments";

[<Literal>]
let DocumentWindowPositionsMcdfKey = "DocumentWindowPositions"


[<AutoOpen>]
module Prelude =
    let asSnd first second = first, second

    let toDictionary data =
        let dictionary = Dictionary<_, _>()

        data
        |> Seq.iter (fun (key, value) -> dictionary.[key] <- value)

        dictionary

    let prefixFileName prefix filePath =
        Path.Combine(Path.GetDirectoryName(filePath), prefix + Path.GetFileName(filePath))

    let suffixFilePath suffix filePath = sprintf "%s.%s" filePath suffix

    let (|KeyValuePair|) (kvp : KeyValuePair<_, _>) = kvp.Key, kvp.Value

[<AutoOpen>]
module Infrastructure =
    open Argu

    type CliArgs =
        | [<CliPrefix(CliPrefix.None)>] Save
        | [<CliPrefix(CliPrefix.None)>] Restore
        | [<CliPrefix(CliPrefix.None)>] Cleanup of daysToKeep:int
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Save -> "Backup the document window settings from the most recently updated .suo file"
                | Restore -> "Restore document window settings for the current branch to all supported .suo files"
                | Cleanup _ -> "Remove saved document window settings older than the given number of days"

    type BranchName = BranchName of string
    type Timestamp = Timestamp of string
    type DocumentData = DocumentData of byte []

    type Result =
        | Saved of string
        | Restored of string list
        | Removed of int
        | NoSuos
        | NoDocumentData
        with
        static member GetMessage (BranchName branch) solutionName result =
            match result with
                | Saved version ->
                    sprintf "Backed up document windows for '%s' from Visual Studio %s" solutionName version
                | Restored versions ->
                    sprintf "Restored document windows for '%s' to Visual Studio %s" solutionName (String.concat ", " versions)
                | Removed count ->
                    sprintf "Removed document window data for %i branches for '%s'" count solutionName
                | NoSuos ->
                    sprintf "No .suo files found for %s.sln" solutionName
                | NoDocumentData ->
                    sprintf "No saved document window data found for '%s' on branch '%s'" solutionName branch

module Solution =
    let findSuos directory solutionName =
        [
            Path.Combine(directory, ".vs", solutionName, "v15", ".suo"), "2017"
            Path.Combine(directory, ".vs", solutionName, "v14", ".suo"), "2015"
            // Path.Combine(directory, sprintf "%s.v12.suo" solutionName), "2013"
        ]
        |> List.filter (fst >> File.Exists)

    let findSolutions directory = Directory.EnumerateFiles(directory, "*.sln")

    let getBranchName path =
        let directory =
            if File.GetAttributes path &&& FileAttributes.Directory = FileAttributes.Directory
            then path
            else Path.GetDirectoryName path

        try
            directory |> Fake.Git.Information.getBranchName |> BranchName |> Some
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
    let getSolutionStorageFileName solutionName = suffixFilePath StorageFileSuffix solutionName

    [<Literal>]
    let TimestampFormat = "yyyyMMdd"

    let toTimestamp (dateTime : DateTime) = dateTime.ToString TimestampFormat |> Timestamp

    let parseTimestamp s =
        DateTime.ParseExact(s, TimestampFormat, Globalization.CultureInfo.InvariantCulture)

    let readWindowSettings directory solutionName =
        let storageFileName = Path.Combine(directory, getSolutionStorageFileName solutionName)

        if File.Exists storageFileName
        then File.ReadAllLines storageFileName
             |> Array.map (fun s ->
                let key, timestamp, data =
                    match s.Split ':' with
                    | [| key; data |] -> BranchName key, toTimestamp DateTime.Now, data
                    | [| key; timestamp; data |] -> BranchName key, Timestamp timestamp, data
                    | _ -> failwith "Error in data"

                key, (timestamp, data |> Convert.FromBase64String |> DocumentData))
        else [| |]
        |> toDictionary

    let writeWindowSettings directory solutionName (data : IDictionary<_, _>) =
        let storageFileName = Path.Combine(directory, getSolutionStorageFileName solutionName)

        data
        |> Seq.map (fun (KeyValuePair (BranchName key, (Timestamp timestamp, DocumentData value))) ->
            value |> Convert.ToBase64String |> sprintf "%s:%s:%s" key timestamp)
        |> asSnd storageFileName
        |> File.WriteAllLines

    let backupToStorage directory solutionName branch suos (settings : IDictionary<_, _>) =
        let suoFileName, version =
            suos
            |> List.map (fun (file, version) -> FileInfo file, version)
            |> List.sortByDescending (fun (fi, _) -> fi.LastWriteTime)
            |> List.head
            |> fun (fi, version) -> fi.FullName, version

        let documents = Mcdf.readSolutionDocuments suoFileName
        settings.[branch] <- (toTimestamp DateTime.Now, DocumentData documents)

        writeWindowSettings directory solutionName settings
        Saved version

    let restoreToSuo directory solutionName branch suos (settings : IDictionary<_, _>) =
        match settings.TryGetValue branch with
        | true, (_, DocumentData data) ->
            match suos with
            | [] -> NoSuos
            | _ ->
                suos
                |> List.map (fun (suoFileName, version) ->
                    data |> Mcdf.replaceStream suoFileName DocumentWindowPositionsMcdfKey
                    
                    version)
                |> Restored
        | false, _-> NoDocumentData

    let cleanupStorage daysToKeep directory solutionName _ _ (settings : IDictionary<_, _>) =
        let oldBranches =
            settings
            |> Seq.filter (fun (KeyValuePair (BranchName key, (Timestamp timestamp, data))) ->
                (DateTime.Now - parseTimestamp timestamp).Days > daysToKeep)
            |> Seq.toList

        oldBranches |> List.iter (settings.Remove >> ignore)
        writeWindowSettings directory solutionName settings

        Removed oldBranches.Length

    let withSettings action branch solutionPath =
        let directory, solutionName = Solution.splitPath solutionPath

        let suos = Solution.findSuos directory solutionName

        readWindowSettings directory solutionName
        |> action directory solutionName branch suos

open Storage

[<EntryPoint>]
let main argv = 
    let directory = Environment.CurrentDirectory

    let argumentParser = Argu.ArgumentParser.Create<CliArgs>()

    match argumentParser.ParseCommandLine(argv, ignoreUnrecognized = true).GetAllResults() with
    | [ arg ] ->
        match Solution.getBranchName directory with
        | Some branch ->
            match arg with
            | Save -> backupToStorage
            | Restore -> restoreToSuo
            | Cleanup daysToKeep -> cleanupStorage daysToKeep
            |> fun action ->
                directory
                |> Solution.findSolutions
                |> Seq.iter (fun solutionName ->
                    solutionName
                    |> withSettings action branch
                    |> Result.GetMessage branch (Path.GetFileNameWithoutExtension solutionName)
                    |> printfn "%s")
        | None -> printfn "Directory not under Git version control"
    | _ -> printfn "%s" (argumentParser.PrintUsage())

    0

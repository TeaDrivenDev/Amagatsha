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
        Path.Combine(Path.GetDirectoryName filePath, prefix + Path.GetFileName filePath)

    let suffixFilePath suffix filePath = sprintf "%s.%s" filePath suffix

    let (|KeyValuePair|) (kvp : KeyValuePair<_, _>) = kvp.Key, kvp.Value

[<AutoOpen>]
module Infrastructure =
    open Argu
    open Fake.Git

    let traceColored color (s:string) = 
        let curColor = Console.ForegroundColor
        if curColor <> color then Console.ForegroundColor <- color
        use textWriter = 
            match color with
            | ConsoleColor.Red -> Console.Error
            | ConsoleColor.Yellow -> Console.Out
            | _ -> Console.Out

        textWriter.WriteLine s
        if curColor <> color then Console.ForegroundColor <- curColor

    [<CliPrefix(CliPrefix.Dash)>]
    type ListArgs  =
        | A
        | D
        | O
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | A -> "List branches alphabetically"
                | D -> "List branches by last write date"
                | O -> "List branches in original order"

    type AmagatshaExiter() =
        interface IExiter with
            member __.Name = "Amagatsha exiter"
            member __.Exit (msg, code) =
                if code = ErrorCode.HelpText then
                    printfn "%s" msg ; exit 0
                else traceColored ConsoleColor.Red msg ; exit 1

    [<CliPrefix(CliPrefix.None)>]
    type CliArgs =
        | List of ParseResults<ListArgs>
        | Save
        | SavePrevious
        | Restore
        | Cleanup of daysToKeep:int
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | List _ -> "List branches for which saved document window data exists"
                | Save -> "Backup the document window settings from the most recently updated .suo file"
                | SavePrevious ->
                    "Backup the document window settings from the most recently updated .suo file for the last previously checked out branch"
                | Restore -> "Restore document window settings for the current branch to all supported .suo files"
                | Cleanup _ -> "Remove saved document window settings older than the given number of days"

    type BranchName = BranchName of string
    type Timestamp = Timestamp of string
    type DocumentData = DocumentData of byte []

    type Result =
        | List of string list
        | Saved of branchName:BranchName * vsVersion:string
        | Restored of vsVersions:string list
        | Removed of count:int
        | NoSuos
        | NoDocumentData of BranchName
        | NoPreviousBranch
        with
        static member GetMessage solutionName result =
            match result with
            | List items ->
                sprintf "Saved document window data for solution '%s' exists for the following branches:" solutionName
                :: items
                |> String.concat Environment.NewLine
            | Saved (BranchName branch, vsVersion) ->
                sprintf "Backed up document windows for '%s' on branch '%s' from Visual Studio %s" solutionName branch vsVersion
            | Restored vsVersions ->
                sprintf "Restored document windows for '%s' to Visual Studio %s" solutionName (String.concat ", " vsVersions)
            | Removed count ->
                sprintf "Removed document window data for %i branches for '%s'" count solutionName
            | NoSuos ->
                sprintf "No .suo files found for %s.sln" solutionName
            | NoDocumentData (BranchName branch) ->
                sprintf "No saved document window data found for '%s' on branch '%s'" solutionName branch
            | NoPreviousBranch ->
                sprintf "No previously checked out branch found"

    let getPreviousBranchName directory =
        let branches =
            "rev-parse --abbrev-ref @{-1}"
            |> getGitResult directory
            |> Seq.toList

        branches
        |> function
            | [ "@{-1}" ] -> None
            | [ branch ] -> branch |> BranchName |> Some
            | _ -> None

    let printVersion () =
        let productName, version =
            let assembly = System.Reflection.Assembly.GetExecutingAssembly()

            let fileVersionInfo =
                System.Diagnostics.FileVersionInfo.GetVersionInfo assembly.Location

            fileVersionInfo.ProductName, assembly.GetName().Version

        printfn "%s v%i.%i.%i" productName version.Major version.Minor version.Build

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
        Saved (branch, version)

    let backupForPreviousBranch directory solutionName _ suos (settings : IDictionary<_, _>) =
        getPreviousBranchName directory
        |> Option.map (fun branch ->
            backupToStorage directory solutionName branch suos settings)
        |> Option.defaultValue NoPreviousBranch

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
        | false, _-> NoDocumentData branch

    let cleanupStorage daysToKeep directory solutionName _ _ (settings : IDictionary<_, _>) =
        let oldBranches =
            settings
            |> Seq.filter (fun (KeyValuePair (BranchName key, (Timestamp timestamp, data))) ->
                (DateTime.Now - parseTimestamp timestamp).Days > daysToKeep)
            |> Seq.toList

        oldBranches |> List.iter (settings.Remove >> ignore)
        writeWindowSettings directory solutionName settings

        Removed oldBranches.Length

    let list option directory solutionName _ _ (settings : IDictionary<_, _>) =
        let sort =
            match option with
            | A -> Seq.sortBy snd
            | D -> Seq.sortBy fst
            | O -> id

        settings
        |> Seq.map (fun (KeyValuePair (branch, (timestamp, _))) -> timestamp, branch)
        |> sort
        |> Seq.map (fun (Timestamp timestamp, BranchName branch) -> sprintf "%s\t%s" timestamp branch)
        |> Seq.toList
        |> List

    let withSettings action branch solutionPath =
        let directory, solutionName = Solution.splitPath solutionPath

        let suos = Solution.findSuos directory solutionName

        match suos with
        | [] -> NoSuos
        | _ ->
            readWindowSettings directory solutionName
            |> action directory solutionName branch suos

open Storage

[<EntryPoint>]
let main argv =
    printVersion ()

    let directory = Environment.CurrentDirectory

    let argumentParser =
        Argu.ArgumentParser.Create<CliArgs>(helpTextMessage = "Help requested",
                                            errorHandler = AmagatshaExiter())

    match argumentParser.ParseCommandLine(argv).GetAllResults() with
    | [ arg ] ->
        match Solution.getBranchName directory with
        | Some branch ->
            match arg with
            | CliArgs.List args ->
                match args.GetAllResults() with
                | head :: _ -> head
                | [] -> ListArgs.A
                |> Storage.list
            | Save -> backupToStorage
            | SavePrevious -> backupForPreviousBranch
            | Restore -> restoreToSuo
            | Cleanup daysToKeep -> cleanupStorage daysToKeep
            |> fun action ->
                directory
                |> Solution.findSolutions
                |> Seq.map (fun solutionName ->
                    solutionName
                    |> withSettings action branch
                    |> Result.GetMessage (Path.GetFileNameWithoutExtension solutionName))
                |> String.concat "\n\n"
                |> printfn "%s"
        | None -> printfn "Directory not under Git version control"
    | _ -> printfn "%s" (argumentParser.PrintUsage())

    0

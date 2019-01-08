open System
open System.Collections.Generic
open System.IO

[<Literal>]
let StorageFileSuffix = "branchdocuments"

let systemCollectionsImmutableDummyReference = System.Collections.Immutable.ImmutableArray()

[<AutoOpen>]
module Prelude =
    let asFst second first = first, second
    let asSnd first second = first, second

    let toDictionary data =
        let dictionary = Dictionary<_, _>()

        data
        |> Seq.iter (fun (key, value) -> dictionary.[key] <- value)

        dictionary

    let tryGetValueOrDefault key defaultValue (dictionary : IDictionary<_, _>) =
        match dictionary.TryGetValue key with
        | true, value -> value
        | false, _ -> defaultValue

    let prefixFileName prefix filePath =
        Path.Combine(Path.GetDirectoryName filePath, prefix + Path.GetFileName filePath)

    let suffixFilePath suffix filePath = sprintf "%s.%s" filePath suffix

    let (|KeyValuePair|) (kvp : KeyValuePair<_, _>) = kvp.Key, kvp.Value

[<AutoOpen>]
module Domain =
    type BranchName = BranchName of string

    [<CLIMutable>]
    type BranchData =
        {
            Id : int
            BranchName : string
            LastWriteTime : DateTime
            DocumentWindowPositions : byte []
            DebuggerBreakpoints : byte []
            BookmarkState : byte []
        }

    type DirectoryPath = DirectoryPath of string
    type SolutionName = SolutionName of string
    type SolutionPath = SolutionPath of string
        with static member SolutionName s =
                match s with
                | SolutionPath path ->
                    path
                    |> Path.GetFileNameWithoutExtension
                    |> SolutionName
    type SuoPath = SuoPath of string
    type VsVersion = VsVersion of string
    type McdfKey = McdfKey of string

    type ActionResult =
        | List of string list
        | Saved of BranchName * VsVersion
        | Restored of VsVersion list
        | Removed of count:int
        | NoSuos
        | NoDocumentData of BranchName
        | NoPreviousBranch
        with
        static member GetMessage (SolutionName solutionName) result =
            match result with
            | List items ->
                sprintf "Saved document window data for solution '%s' exists for the following branches:" solutionName
                :: items
                |> String.concat Environment.NewLine
            | Saved (BranchName branch, VsVersion vsVersion) ->
                sprintf "Backed up document windows for '%s' on branch '%s' from Visual Studio %s" solutionName branch vsVersion
            | Restored vsVersions ->
                vsVersions
                |> List.map (fun (VsVersion vsVersion) -> vsVersion)
                |> String.concat ", "
                |> sprintf "Restored document windows for '%s' to Visual Studio %s" solutionName
            | Removed count ->
                sprintf "Removed document window data for %i branches for '%s'" count solutionName
            | NoSuos ->
                sprintf "No .suo files found for %s.sln" solutionName
            | NoDocumentData (BranchName branch) ->
                sprintf "No saved document window data found for '%s' on branch '%s'" solutionName branch
            | NoPreviousBranch ->
                sprintf "No previously checked out branch found"

    type WindowSettings = IDictionary<BranchName, BranchData>

    type Result = { ActionResult : ActionResult; WindowSettings : WindowSettings option }

    type OperationForSingleSolution =
        DirectoryPath -> BranchName -> (SuoPath * VsVersion) list -> WindowSettings -> Result

[<AutoOpen>]
module Infrastructure =
    open Argu

    let printVersion () =
        let productName, version =
            let assembly = System.Reflection.Assembly.GetExecutingAssembly()

            let fileVersionInfo =
                System.Diagnostics.FileVersionInfo.GetVersionInfo assembly.Location

            fileVersionInfo.ProductName, assembly.GetName().Version

        printfn "%s v%i.%i.%i" productName version.Major version.Minor version.Build

    let traceColored color (s : string) =
        let curColor = Console.ForegroundColor
        if curColor <> color then Console.ForegroundColor <- color
        use textWriter =
            match color with
            | ConsoleColor.Red -> Console.Error
            | ConsoleColor.Yellow -> Console.Out
            | _ -> Console.Out

        textWriter.WriteLine s
        if curColor <> color then Console.ForegroundColor <- curColor

    type AmagatshaExiter() =
        interface IExiter with
            member __.Name = "Amagatsha exiter"
            member __.Exit (msg, code) =
                if code = ErrorCode.HelpText then
                    printfn "%s" msg ; exit 0
                else traceColored ConsoleColor.Red msg ; exit 1

    [<CliPrefix(CliPrefix.Dash)>]
    type ListArgs  =
        | A
        | D
        | O
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | A -> "List branches alphabetically (this is the default)"
                | D -> "List branches by last write date"
                | O -> "List branches in original order"

    [<CliPrefix(CliPrefix.Dash)>]
    type CleanupArgs =
        | B
        | D of daysToKeep:int
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | B -> "Keep only data for branches that still exist locally (this is the default)"
                | D _ -> "Keep only data for branches that have been updated in the given number of days"

    [<CliPrefix(CliPrefix.None)>]
    type CliArgs =
        | List of ParseResults<ListArgs>
        | Save
        | SavePrevious
        | Restore
        | Cleanup of ParseResults<CleanupArgs>
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | List _ -> "List branches for which saved document window data exists"
                | Save -> "Backup the document window settings from the most recently updated .suo file"
                | SavePrevious ->
                    "Backup the document window settings from the most recently updated .suo file for the last previously checked out branch"
                | Restore -> "Restore document window settings for the current branch to all supported .suo files"
                | Cleanup _ -> "Remove obsolete branch data"

module Solution =
    open Fake.Tools

    let findSuos (DirectoryPath directory) (SolutionName solutionName) =
        [
            Path.Combine(directory, ".vs", solutionName, "v15", ".suo"), VsVersion "2017"
            Path.Combine(directory, ".vs", solutionName, "v14", ".suo"), VsVersion "2015"
            // Path.Combine(directory, sprintf "%s.v12.suo" solutionName), "2013"
        ]
        |> List.filter (fst >> File.Exists)
        |> List.map (fun (path, version) -> SuoPath path, version)

    let findSolutions (DirectoryPath directory) =
        Directory.EnumerateFiles(directory, "*.sln")
        |> Seq.map SolutionPath

    let getBranchName (DirectoryPath path) =
        let directory =
            if File.GetAttributes path &&& FileAttributes.Directory = FileAttributes.Directory
            then path
            else Path.GetDirectoryName path

        try
            directory |> Git.Information.getBranchName |> BranchName |> Some
        with :? ArgumentException -> None

    let getPreviousBranchName (DirectoryPath directory) =
        let branches =
            "rev-parse --abbrev-ref @{-1}"
            |> Git.CommandHelper.getGitResult directory
            |> Seq.toList

        branches
        |> function
            | [ "@{-1}" ] -> None
            | [ branch ] -> branch |> BranchName |> Some
            | _ -> None

    let splitPath (SolutionPath solutionPath) =
        DirectoryPath (Path.GetDirectoryName solutionPath),
        SolutionName (Path.GetFileNameWithoutExtension solutionPath)

module Mcdf =
    open OpenMcdf

    type StreamKind =
        | DocumentWindows
        | DebuggerBreakpoints
        | BookmarkState
        with
            member this.Key =
                match this with
                | DocumentWindows -> "DocumentWindowPositions"
                | DebuggerBreakpoints -> "DebuggerBreakpoints"
                | BookmarkState -> "BookmarkState"

    let private readStreams (SuoPath path) (streamKinds : StreamKind list) =
        use file = new CompoundFile(path)

        let data =
            streamKinds
            |> List.map (fun streamKind ->
                let stream = file.RootStorage.GetStream streamKind.Key

                streamKind, stream.GetData())

        file.Close()

        data

    let readSolutionData suoFilepath =
        readStreams suoFilepath [ DocumentWindows; DebuggerBreakpoints; BookmarkState ]

    let private replaceStreams (SuoPath path) streamData =
        use file = new CompoundFile(path, CFSUpdateMode.Update, CFSConfiguration.Default)

        streamData
        |> List.iter (fun (streamKind : StreamKind, data : byte []) ->
            if data.Length > 0
            then
                let stream = file.RootStorage.GetStream streamKind.Key

                data |> Array.length |> int64 |> stream.Resize

                stream.Write(data, 0L))

        let tempFilePath = path |> prefixFileName "x"
        file.Save tempFilePath
        file.Close()

        File.Delete path
        File.Move(tempFilePath, path)

    let writeSolutionData suoPath branchData =
        [
            DocumentWindows, branchData.DocumentWindowPositions
            DebuggerBreakpoints, branchData.DebuggerBreakpoints
            BookmarkState, branchData.BookmarkState
        ]
        |> replaceStreams suoPath

module Storage =
    open LiteDB
    open LiteDB.FSharp

    let private getSolutionStorageFileName (SolutionName solutionName) =
        suffixFilePath StorageFileSuffix solutionName

    [<CLIMutable>]
    type Metadata =
        {
            Id : int
            Version : int
            LastWriteTime : DateTime
        }

    [<Literal>]
    let TimestampFormat = "yyyyMMdd"

    [<RequireQualifiedAccess>]
    module Legacy =
        let (|ParseTimestamp|) s =
            DateTime.ParseExact(s, TimestampFormat, Globalization.CultureInfo.InvariantCulture)

        let readDataFile storageFilePath =
            File.ReadAllLines storageFilePath
            |> Array.map (fun s ->
            let key, timestamp, data =
                match s.Split ':' with
                | [| key; data |] -> key, DateTime.Now, data
                | [| key; ParseTimestamp timestamp; data |] ->
                    key, timestamp, data
                | [| key; ParseTimestamp timestamp; protection; data |] ->
                    key, timestamp, data
                | _ -> failwith "Error in data"

            BranchName key,
            {
                Id = 0
                BranchName = key
                LastWriteTime = timestamp
                DocumentWindowPositions = Convert.FromBase64String data
                DebuggerBreakpoints = [| |]
                BookmarkState = [| |]
            })

    [<Literal>]
    let DatabaseVersion = 1

    let openDatabase (storageFilePath : string) =
        new LiteDatabase(storageFilePath, FSharpBsonMapper())

    let readDatabase (database : LiteDatabase) =
        database.GetCollection<BranchData>().FindAll()
        |> Seq.map (fun branchData -> BranchName branchData.BranchName, branchData)
        |> Seq.toArray

    let readWindowSettings (DirectoryPath directory) solutionName =
        let storageFilePath = Path.Combine(directory, getSolutionStorageFileName solutionName)

        if File.Exists storageFilePath
        then
            use database = openDatabase storageFilePath

            try
                database.CollectionExists("Metadata") |> ignore
                readDatabase database
            with
            | :? LiteException as ex -> Legacy.readDataFile storageFilePath
        else [| |]
        |> toDictionary

    let writeDataFile storageFilePath (data : IDictionary<_, BranchData>) =
        use database =
            let database = openDatabase storageFilePath

            try
                database.CollectionExists("Metadata") |> ignore
                database
            with
            | :? LiteException as ex ->
                database.Dispose()

                File.Delete storageFilePath

                let database = openDatabase storageFilePath
                database

        let metadataCollection = database.GetCollection<Metadata>()

        let metadataId =
            metadataCollection.FindAll()
            |> Seq.tryHead
            |> Option.map (fun metadata -> metadata.Id)

        let metadata =
            {
                Id = metadataId |> Option.defaultValue 0
                Version = DatabaseVersion
                LastWriteTime = DateTime.Now
            }

        metadataCollection.Upsert metadata |> ignore

        let branchDataCollection = database.GetCollection<BranchData>()
        branchDataCollection.Delete(fun _ -> true) |> ignore

        data
        |> Seq.map (fun (KeyValuePair (_, branchData)) -> { branchData with Id = 0 })
        |> branchDataCollection.InsertBulk
        |> ignore

    let writeWindowSettings (DirectoryPath directory) solutionName (data : IDictionary<_, _>) =
        Path.Combine(directory, getSolutionStorageFileName solutionName)
        |> asFst data
        ||> writeDataFile

    let backupToStorage directory (BranchName branchName as branch) suos (settings : IDictionary<_, _>) =
        let suoFileName, version =
            suos
            |> List.map (fun (SuoPath file, version) -> FileInfo file, version)
            |> List.sortByDescending (fun (fi, _) -> fi.LastWriteTime)
            |> List.head
            |> fun (fi, version) -> SuoPath fi.FullName, version

        let solutionData = Mcdf.readSolutionData suoFileName |> dict

        settings.[branch] <-
            {
                Id = 0
                BranchName = branchName
                LastWriteTime = DateTime.Now
                DocumentWindowPositions =
                    solutionData |> tryGetValueOrDefault Mcdf.DocumentWindows [| |]
                DebuggerBreakpoints =
                    solutionData |> tryGetValueOrDefault Mcdf.DebuggerBreakpoints [| |]
                BookmarkState =
                    solutionData |> tryGetValueOrDefault Mcdf.BookmarkState [| |]
            }

        { ActionResult = Saved (branch, version); WindowSettings = Some settings }

    let backupForPreviousBranch directory _ suos (settings : IDictionary<_, _>) =
        Solution.getPreviousBranchName directory
        |> Option.map (fun branch ->
            backupToStorage directory branch suos settings)
        |> Option.defaultValue { ActionResult = NoPreviousBranch; WindowSettings = None }

    let restoreToSuo directory branch suos (settings : IDictionary<_, _>) =
        {
            ActionResult =
                match settings.TryGetValue branch with
                | true, branchData ->
                    match suos with
                    | [] -> NoSuos
                    | _ ->
                        suos
                        |> List.map (fun (suoFileName, version) ->
                            Mcdf.writeSolutionData suoFileName branchData

                            version)
                        |> Restored
                | false, _-> NoDocumentData branch
            WindowSettings = None
        }

    let cleanupStorage args (DirectoryPath directory) _ _ (settings : IDictionary<_, _>) =
        let obsolete =
            match args with
            | B ->
                let localBranches = Fake.Tools.Git.Branches.getLocalBranches directory

                (fun branchData -> localBranches |> List.contains branchData.BranchName |> not)
            | D daysToKeep ->
                (fun branchData -> (DateTime.Now - branchData.LastWriteTime).Days > daysToKeep)

        let obsoleteBranches =
            settings
            |> Seq.filter (fun (KeyValuePair (_, branchData)) -> obsolete branchData)
            |> Seq.toList

        obsoleteBranches |> List.iter (settings.Remove >> ignore)

        { ActionResult = Removed obsoleteBranches.Length; WindowSettings = Some settings }

    let list listOption directory _ _ (settings : IDictionary<_, _>) =
        let toTimestamp (dateTime : DateTime) = dateTime.ToString TimestampFormat

        let sort =
            match listOption with
            | A -> Seq.sortBy (fun branchData -> branchData.BranchName)
            | ListArgs.D -> Seq.sortBy (fun branchData -> branchData.LastWriteTime)
            | O -> id

        {
            ActionResult =
                settings
                |> Seq.map (fun (KeyValuePair (_, branchData)) -> branchData)
                |> sort
                |> Seq.map (fun branchData ->
                    sprintf "%s\t%s" (toTimestamp branchData.LastWriteTime) branchData.BranchName)
                |> Seq.toList
                |> ActionResult.List
            WindowSettings = None
        }

    let withSettings (action : OperationForSingleSolution) branch solutionPath =
        let directory, solutionName = Solution.splitPath solutionPath

        let suos = Solution.findSuos directory solutionName

        match suos with
        | [] -> NoSuos
        | _ ->
            let result =
                readWindowSettings directory solutionName
                |> action directory branch suos

            result.WindowSettings
            |> Option.iter (writeWindowSettings directory solutionName)

            result.ActionResult

open Storage

[<EntryPoint>]
let main argv =
    printVersion ()

    let directory = DirectoryPath Environment.CurrentDirectory

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
            | Cleanup args ->
                match args.GetAllResults() with
                | head :: _ -> head
                | [] -> B
                |> cleanupStorage
            |> fun action ->
                directory
                |> Solution.findSolutions
                |> Seq.map (fun solutionPath ->
                    solutionPath
                    |> withSettings action branch
                    |> ActionResult.GetMessage (SolutionPath.SolutionName solutionPath))
                |> String.concat "\n\n"
                |> printfn "%s"
        | None -> printfn "Directory not under Git version control"
    | _ -> printfn "%s" (argumentParser.PrintUsage())

    0

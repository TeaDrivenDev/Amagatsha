open System
open System.Collections.Generic
open System.IO

[<Literal>]
let StorageFileSuffix = "branchdocuments";

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
module Domain =
    type BranchName = BranchName of string
    type Timestamp = Timestamp of string
    type Protection = Protected | NotProtected
        with static member Parse s =
                match s with
                | "x" -> Protected
                | _ -> NotProtected
             override p.ToString() =
                match p with
                | Protected -> "x"
                | NotProtected -> "o"
    type DocumentData = DocumentData of byte []

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

    type WindowSettings = IDictionary<BranchName, (Timestamp * Protection * DocumentData)>

    type OperationForAllSolutions =
        DirectoryPath -> SolutionName -> BranchName -> (SuoPath * VsVersion) list -> WindowSettings -> ActionResult

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
                | A -> "List branches alphabetically"
                | D -> "List branches by last write date"
                | O -> "List branches in original order"

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

module Solution =
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
            directory |> Fake.Git.Information.getBranchName |> BranchName |> Some
        with :? ArgumentException -> None

    let getPreviousBranchName (DirectoryPath directory) =
        let branches =
            "rev-parse --abbrev-ref @{-1}"
            |> Fake.Git.CommandHelper.getGitResult directory
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
    let documentWindowPositionsMcdfKey = McdfKey "DocumentWindowPositions"

    let readStream (SuoPath path) (McdfKey streamName) =
        use file = new OpenMcdf.CompoundFile(path)

        let stream = file.RootStorage.GetStream streamName

        let data = stream.GetData()
        file.Close()

        DocumentData data

    let readSolutionDocuments suoFilepath = 
        readStream suoFilepath documentWindowPositionsMcdfKey

    let replaceStream (SuoPath path) (McdfKey streamName) (DocumentData data) =
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
    let private getSolutionStorageFileName (SolutionName solutionName) =
        suffixFilePath StorageFileSuffix solutionName

    [<Literal>]
    let TimestampFormat = "yyyyMMdd"

    let toTimestamp (dateTime : DateTime) = dateTime.ToString TimestampFormat |> Timestamp

    let parseTimestamp (Timestamp s) =
        DateTime.ParseExact(s, TimestampFormat, Globalization.CultureInfo.InvariantCulture)

    let readWindowSettings (DirectoryPath directory) solutionName =
        let storageFilePath = Path.Combine(directory, getSolutionStorageFileName solutionName)

        if File.Exists storageFilePath
        then File.ReadAllLines storageFilePath
             |> Array.map (fun s ->
                let key, timestamp, protection, data =
                    match s.Split ':' with
                    | [| key; data |] -> BranchName key, toTimestamp DateTime.Now, NotProtected, data
                    | [| key; timestamp; data |] ->
                        BranchName key, Timestamp timestamp, NotProtected, data
                    | [| key; timestamp; protection; data |] ->
                        BranchName key, Timestamp timestamp, Protection.Parse protection, data
                    | _ -> failwith "Error in data"

                key, (timestamp, protection, data |> Convert.FromBase64String |> DocumentData))
        else [| |]
        |> toDictionary

    let writeWindowSettings (DirectoryPath directory) solutionName (data : IDictionary<_, _>) =
        let storageFileName = Path.Combine(directory, getSolutionStorageFileName solutionName)

        data
        |> Seq.map (fun (KeyValuePair (BranchName key, (Timestamp timestamp, protection, DocumentData value))) ->
            value |> Convert.ToBase64String |> sprintf "%s:%s:%s:%s" key timestamp (protection.ToString()))
        |> asSnd storageFileName
        |> File.WriteAllLines

    let backupToStorage directory solutionName branch suos (settings : IDictionary<_, _>) =
        let suoFileName, version =
            suos
            |> List.map (fun (SuoPath file, version) -> FileInfo file, version)
            |> List.sortByDescending (fun (fi, _) -> fi.LastWriteTime)
            |> List.head
            |> fun (fi, version) -> SuoPath fi.FullName, version

        let documents = Mcdf.readSolutionDocuments suoFileName

        let protection =
            match settings.TryGetValue branch with
            | true, (_, protection, _) -> protection
            | false, _ -> NotProtected

        settings.[branch] <- (toTimestamp DateTime.Now, protection, documents)

        writeWindowSettings directory solutionName settings
        Saved (branch, version)

    let backupForPreviousBranch directory solutionName _ suos (settings : IDictionary<_, _>) =
        Solution.getPreviousBranchName directory
        |> Option.map (fun branch ->
            backupToStorage directory solutionName branch suos settings)
        |> Option.defaultValue NoPreviousBranch

    let restoreToSuo directory solutionName branch suos (settings : IDictionary<_, _>) =
        match settings.TryGetValue branch with
        | true, (_, _, data) ->
            match suos with
            | [] -> NoSuos
            | _ ->
                suos
                |> List.map (fun (suoFileName, version) ->
                    data
                    |> Mcdf.replaceStream suoFileName Mcdf.documentWindowPositionsMcdfKey
                    
                    version)
                |> Restored
        | false, _-> NoDocumentData branch

    let cleanupStorage daysToKeep directory solutionName _ _ (settings : IDictionary<_, _>) =
        let oldBranches =
            settings
            |> Seq.filter (fun (KeyValuePair (BranchName key, (timestamp, protection, data))) ->
                protection <> Protected && (DateTime.Now - parseTimestamp timestamp).Days > daysToKeep)
            |> Seq.toList

        oldBranches |> List.iter (settings.Remove >> ignore)
        writeWindowSettings directory solutionName settings

        Removed oldBranches.Length

    let list listOption directory solutionName _ _ (settings : IDictionary<_, _>) =
        let sort =
            match listOption with
            | A -> Seq.sortBy (fun (_, _, t) -> t)
            | D -> Seq.sortBy (fun (f, _, _) -> f)
            | O -> id

        settings
        |> Seq.map (fun (KeyValuePair (branch, (timestamp, protection, _))) ->
            timestamp, protection, branch)
        |> sort
        |> Seq.map (fun (Timestamp timestamp, protection, BranchName branch) ->
            let protection = match protection with Protected -> "Protected" | NotProtected -> ""
            sprintf "%s   %-12s %s" timestamp protection branch)
        |> Seq.toList
        |> ActionResult.List

    let withSettings (action : OperationForAllSolutions) branch solutionPath =
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
            | Cleanup daysToKeep -> cleanupStorage daysToKeep
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

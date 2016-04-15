open System
open System.Collections.Generic
open System.IO
open System.Text

[<Literal>]
let StorageFileName = "branchdocuments";

[<Literal>]
let DocumentWindowPositionsMcdfKey = "DocumentWindowPositions"

let toDictionary data =
    let dictionary = Dictionary<_, _>()

    data
    |> Seq.iter (fun (key, value) -> dictionary.[key] <- value)

    dictionary
    
let prefixFileName prefix filePath =
    Path.Combine(Path.GetDirectoryName(filePath), prefix + Path.GetFileName(filePath))

module Solution =
    let findSuo directory solutionName =
        [
            Path.Combine(directory, ".vs", solutionName, "v14", ".suo")
            Path.Combine(directory, sprintf "%s.v12.suo" solutionName)
        ]
        |> List.tryFind File.Exists

    let findSolutions directory =
        Directory.EnumerateFiles(directory, "*.sln")

    let getBranchName path =
        let directory =
            if File.GetAttributes path &&& FileAttributes.Directory = FileAttributes.Directory
            then path
            else Path.GetDirectoryName path

        try
            directory
            |> Fake.Git.Information.getBranchName
            |> Some
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

    let readSolutionDocuments suoFilepath =
        readStream suoFilepath DocumentWindowPositionsMcdfKey

    let replaceStream (path : string) streamName data =
        use file = new OpenMcdf.CompoundFile(path, OpenMcdf.CFSUpdateMode.Update, OpenMcdf.CFSConfiguration.Default)

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
                key, data |> Convert.FromBase64String)
        else [| |]
        |> toDictionary

    let writeSettings directory solutionName (data : IDictionary<_, byte array>) =
        let storageFileName = Path.Combine(directory, getSolutionStorageFileName solutionName)

        data
        |> Seq.map (fun kvp ->
            kvp.Value
            |> Convert.ToBase64String
            |> sprintf "%s:%s" kvp.Key)
        |> (fun data -> File.WriteAllLines(storageFileName, data))

    let updateSettings branch solutionPath =
        let parts = Solution.splitPath solutionPath

        match Solution.findSuo <|| parts with
        | Some suo ->
            let documents = Mcdf.readSolutionDocuments suo

            let settings = readSettings <|| parts
            settings.[branch] <- documents

            (writeSettings <|| parts) settings
        | None -> printfn "No .suo file found for solution '%s'" (snd parts)

    let restoreSettings branch solutionPath =
        let parts = Solution.splitPath solutionPath

        match Solution.findSuo <|| parts with
        | Some suo ->
            let settings = readSettings <|| parts
            
            match settings.TryGetValue branch with
            | true, data ->
                data
                |> Mcdf.replaceStream suo DocumentWindowPositionsMcdfKey
            | false, _-> printfn "No document window data found for solution '%s' and branch '%s'" (snd parts) branch
        | None -> printfn "No .suo file found for solution '%s'" (snd parts)

[<EntryPoint>]
let main argv = 
    let directory = Environment.CurrentDirectory

    match Solution.getBranchName directory with
    | Some branch -> 
        match argv with
        | [| "save" |] ->
            directory
            |> Solution.findSolutions
            |> Seq.iter (Storage.updateSettings branch)

            printfn "Saved document windows for branch '%s'." branch
        | [| "restore" |] ->
            directory
            |> Solution.findSolutions
            |> Seq.iter (Storage.restoreSettings branch)
            
            printfn "Restored document windows for branch '%s'." branch
        | _ -> printfn "Unknown option '%A'" argv
    | None -> printfn "Directory not under Git version control"

    0

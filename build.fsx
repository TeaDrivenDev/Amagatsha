System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

#r "paket:
nuget Fake.Api.GitHub
nuget Fake.Core.ReleaseNotes
nuget Fake.Core.Target
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.DotNet.MSBuild
nuget Fake.IO.FileSystem
nuget Fake.IO.Zip
nuget Fake.Tools.Git
nuget Octokit
//"

#load ".fake/build.fsx/intellisense.fsx"

open Fake.Api
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing
open Fake.IO.Globbing.Operators
open Fake.Tools

let solutionFile  = "Amagatsha.sln"

let gitOwner = "TeaDrivenDev"
let gitHome = "https://github.com/" + gitOwner
let gitName = "Amagatsha"
let gitRaw = Environment.environVarOrDefault "gitRaw" ("https://raw.github.com/" + gitOwner)

let outputDirectory = "bin"

let release = ReleaseNotes.load "RELEASE_NOTES.md"

Target.create "AssemblyInfo" (fun _ ->
    let fileName = !! "**/AssemblyInfo.fs" |> Seq.head

    [
        AssemblyInfo.Title "Amagatsha"
        AssemblyInfo.Product "Amagatsha"
        AssemblyInfo.Copyright "Copyright © TeaDrivenDev 2019"
        AssemblyInfo.ComVisible false
        AssemblyInfo.Version release.AssemblyVersion
    ]
    |> AssemblyInfoFile.createFSharp fileName
)

Target.create "Clean" (fun _ -> Shell.cleanDirs [ outputDirectory ])

Target.create "Build" (fun _ ->
    !! solutionFile
    |> MSBuild.runRelease id "" "Rebuild"
    |> ignore)

Target.create "Pack" (fun _ ->
    let filesToPack =
        {
            BaseDirectory = outputDirectory
            Includes = [ "*.exe"; "*.dll"; "*.config" ]
            Excludes = []
        }
        :> IGlobbingPattern

    Zip.createZipOfIncludes (outputDirectory + "/Amagatsha.zip") "" 5 ["", filesToPack ]
)

Target.create "ReleaseGitHub" (fun _ ->
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    Git.Staging.stageAll ""
    Git.Commit.exec "" (sprintf "Release version %s" release.NugetVersion)
    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion

    // release on GitHub
    GitHub.createClientWithToken (Environment.environVarOrFail "githubtoken")
    |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> GitHub.uploadFile "./bin/Amagatsha.zip"
    |> GitHub.publishDraft
    |> Async.RunSynchronously
)

"AssemblyInfo"
==> "Clean"
==> "Build"
==> "Pack"
==> "ReleaseGitHub"

Target.runOrDefault "Build"

printfn "\nFinished %s\n" (System.DateTime.Now.ToString "HH:mm:ss")

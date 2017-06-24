System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

#r @"packages/build/FAKE/tools/FakeLib.dll"

open System
open System.IO

open Fake
open Fake.Git

let solutionFile  = "Amagatsha.sln"

let gitOwner = "TeaDrivenDev"
let gitHome = "https://github.com/" + gitOwner
let gitName = "Amagatsha"
let gitRaw = environVarOrDefault "gitRaw" ("https://raw.github.com/" + gitOwner)

let outputDirectory = "bin"

let release = File.ReadAllLines "RELEASE_NOTES.md" |> ReleaseNotesHelper.parseReleaseNotes 

Target "AssemblyInfo" (fun _ ->
    let fileName = !! "**/AssemblyInfo.fs" |> Seq.head

    ReplaceAssemblyInfoVersions (fun p ->
        { p with
            OutputFileName = fileName
            AssemblyVersion = release.AssemblyVersion
            AssemblyFileVersion = release.AssemblyVersion + ".*"
            AssemblyInformationalVersion =
                let version = System.Version release.AssemblyVersion

                sprintf "%i.%i" version.Major version.Minor
        })
)

Target "Clean" (fun _ -> CleanDirs [ outputDirectory ])

Target "Build" (fun _ ->
    !! solutionFile
    |> MSBuildRelease "" "Rebuild"
    |> ignore)

Target "Pack" (fun _ ->
    let filesToPack =
        {
            BaseDirectory = outputDirectory
            Includes = [ "*.exe"; "*.dll"; "*.config" ]
            Excludes = []
        }

    ZipOfIncludes (outputDirectory + "/Amagatsha.zip") ["", filesToPack ]
)

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

Target "ReleaseGitHub" (fun _ ->
    let user =
        match getBuildParam "github-user" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "Username: "
    let pw =
        match getBuildParam "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    StageAll ""
    Git.Commit.Commit "" (sprintf "Release version %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion
    
    // release on github
    createClient user pw
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes 
    |> uploadFile "./bin/Amagatsha.zip"
    |> releaseDraft
    |> Async.RunSynchronously
)


"AssemblyInfo"
==> "Clean"
==> "Build"
==> "Pack"
==> "ReleaseGitHub"

RunTargetOrDefault "Build"

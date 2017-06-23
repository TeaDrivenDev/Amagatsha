System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

#r @"packages/build/FAKE/tools/FakeLib.dll"

open System.IO

open Fake

let solutionFile  = "Amagatsha.sln"

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

"AssemblyInfo"
==> "Clean"
==> "Build"

RunTargetOrDefault "Build"
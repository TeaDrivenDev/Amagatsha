System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

#r @"packages/build/FAKE/tools/FakeLib.dll"

open Fake

let solutionFile  = "Amagatsha.sln"

Target "Build" (fun _ ->
    !! solutionFile
    |> MSBuildRelease "" "Rebuild"
    |> ignore)

RunTargetOrDefault "Build"
#r "paket:
nuget FSharp.Core
nuget Fake.DotNet.Cli
nuget Fake.IO.Filesystem
nuget Fake.Core.Target //"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators

// Properties
let buildDir = "./build"

let setMsBuildParams (defaults: MSBuildParams) =
    { defaults with
        Verbosity = Some(Quiet)
        Targets = ["Build"]
        Properties =
        [
            "Optimize", "True"
            "DebugSymbols", "True"
            "Configuration", "Release"
        ]
    }

// ** Define Targets
Target.create "Clean" (fun _ ->
    Trace.log " --- Cleaning Stuff ---"
    Shell.cleanDir buildDir
)

Target.create "Build" (fun _ ->
    !! "./src/fons.sln"
    |> MSBuild.runRelease setMsBuildParams buildDir "Build"
    |> Trace.logItems "AppBuild Output: "
)

Target.create "Deploy" (fun _ ->
    Trace.log " --- Deploying the Library --- "
)

open Fake.Core.TargetOperators

"Clean"
    ==> "Build"
    ==> "Deploy"

Target.runOrDefault "Deploy"

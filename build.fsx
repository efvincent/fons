#r "paket:
nuget FSharp.Core
nuget Fake.DotNet.Cli
nuget Fake.IO.Filesystem
nuget Fake.Core.Target //"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators

// Properties
let buildDir = "./build"

// ** Define Targets
Target.create "Clean" (fun _ ->
    Trace.log " --- Cleaning Stuff ---"
    Shell.cleanDir buildDir
)

Target.create "Build" (fun _ ->
    !! "src/**/*.fsproj"
    |> MSBuild.runRelease (fun bp -> {bp with Properties = [("version", "0.0.3")]}) buildDir "Build"
    |> Trace.logItems "AppBuild-Output: " 
)

Target.create "Deploy" (fun _ ->
    Trace.log " --- Deploying the Library --- "
)

open Fake.Core.TargetOperators

"Clean"
    ==> "Build"
    ==> "Deploy"

Target.runOrDefault "Deploy"

#r "paket:
nuget FSharp.Core
nuget Fake.DotNet.MSBuild
nuget Fake.IO.Filesystem
nuget Fake.Core.Target //"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators

let buildDir = "./src/bin"

// ** Define Targets
Target.create "Clean" (fun _ ->
    Trace.log " --- Cleaning Stuff ---"
    Shell.cleanDir buildDir
)

Target.create "Build" (fun _ ->
    Trace.log " --- Building the Library --- "
    !! "src/**/*.fsproj"
    |> MSBuild.runRelease id buildDir "Build"
    |> Trace.logItems "AppBuild output:"

)

Target.create "Deploy" (fun _ ->
    Trace.log " --- Deploying the Library --- "
)

open Fake.Core.TargetOperators

"Clean"
    ==> "Build"
    ==> "Deploy"

Target.runOrDefault "Deploy"

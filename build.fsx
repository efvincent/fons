#r "paket:
nuget FSharp.Core
nuget Fake.Core.Target //"

open Fake.Core

// ** Define Targets
Target.create "Clean" (fun _ ->
    Trace.log " --- Cleaning Stuff ---"
)

Target.create "Build" (fun _ ->
    Trace.log " --- Building the Library --- "
)

Target.create "Deploy" (fun _ ->
    Trace.log " --- Deploying the Library --- "
)

open Fake.Core.TargetOperators

"Clean"
    ==> "Build"
    ==> "Deploy"

Target.runOrDefault "Deploy"

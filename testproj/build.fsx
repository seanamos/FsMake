#if RELEASE
#r "../FsMake/bin/Release/netcoreapp3.1/FsMake.dll"
#else
#r "../FsMake/bin/Debug/netcoreapp3.1/FsMake.dll"
#endif

open FsMake

let args = fsi.CommandLineArgs

let fail = EnvVar.getOptionAs<int> "FAIL" |> Option.contains 1
let build = Step.create "build" { do! Cmd.createWithArgs "echo" [ "building..." ] |> Cmd.run }
let emptyStep = Step.create "empty" { () }
let emptyStepFail = Step.create "emptyFail" { do! Step.fail "Oh no!" }

Pipelines.create {
    let! buildP =
        Pipeline.create "build" {
            desc "Builds the solution"
            run build
        }

    do!
        Pipeline.create "kitchen-sink" {
            desc "Runs all tests"
            run_maybe emptyStepFail fail
            run build
            run_maybe emptyStep false
            run_maybe emptyStep true
            run_parallel [ emptyStep; build ]
            run_parallel_maybe [ emptyStep; build ] false
            run_parallel_maybe [ emptyStep; build ] true

            run_parallel_maybes {
                run_maybe emptyStep false
                run_maybe emptyStep true
                run emptyStep
                run build
            }

            run_parallel_maybes {
                run emptyStep
                run build
            }
        }

    do!
        Pipeline.create "quick-test" {
            desc "A quick test run"
            run emptyStep
            run_maybe emptyStep false
        }

    do!
        Pipeline.create "empty" {
            desc "Empty, does nothing"
            ()
        }

    default_pipeline buildP
}
|> Pipelines.runWithArgsAndExit args

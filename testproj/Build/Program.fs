open FsMake

let args = System.Environment.GetCommandLineArgs ()
printfn "%A" args

let fail = EnvVar.getOptionAs<int> "FAIL" |> Option.contains 1
let build = Step.create "build" { do! Cmd.createWithArgs "dotnet" [ "build"; "Prog" ] |> Cmd.run }
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
            run build
            run_maybe emptyStepFail fail
            run_maybe emptyStep false
            run_maybe emptyStep true
            run_parallel [ emptyStep; emptyStep ]
            run_parallel_maybe [ emptyStep; emptyStep ] false
            run_parallel_maybe [ emptyStep; emptyStep ] true

            run_parallel_maybes {
                run_maybe emptyStep false
                run_maybe emptyStep true
                run emptyStep
                run emptyStep
            }

            run_parallel_maybes {
                run emptyStep
                run emptyStep
            }
        }

    do!
        Pipeline.create "quick-test" {
            desc "A quick test run"
            run emptyStep
            run emptyStepFail
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

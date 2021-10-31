module FsMake.IntegrationTests.PipelinesTests

open Expecto
open Expecto.Flip
open FsMake

[<Tests>]
let tests =
    testList
        "Pipelines integration tests"
        [
            test "default pipeline is used" {
                let mutable run = false

                let step1 = Step.create "step1" { run <- true }
                let step2 = Step.create "step2" { () }

                let pipelines =
                    Pipelines.create {
                        let! pipeline1 = Pipeline.create "pipeline1" { run step1 }
                        do! Pipeline.create "pipeline2" { run step2 }
                        default_pipeline pipeline1
                    }

                let exitCode = pipelines |> Pipelines.runWithArgs [| "-v"; "disabled" |]

                exitCode |> Expect.equal "Expected exit code to be zero" 0
                run |> Expect.isTrue "Expected step1 to have run"
            }

            test "failure with no default pipeline and no pipeline specified" {
                let step1 = Step.create "step1" { () }

                let pipelines = Pipelines.create { do! Pipeline.create "pipeline1" { run step1 } }

                let exitCode = pipelines |> Pipelines.runWithArgs [| "-v"; "disabled" |]

                (exitCode, 0) |> Expect.isGreaterThan "Expected exit code to be non-zero"
            }

            test "specified pipeline is used" {
                let mutable run = false

                let step1 = Step.create "step1" { () }
                let step2 = Step.create "step2" { run <- true }

                let pipelines =
                    Pipelines.create {
                        do! Pipeline.create "pipeline1" { run step1 }
                        do! Pipeline.create "pipeline2" { run step2 }
                    }

                let exitCode = pipelines |> Pipelines.runWithArgs [| "pipeline2"; "-v"; "disabled" |]

                exitCode |> Expect.equal "Expected exit code to be zero" 0
                run |> Expect.isTrue "Expected step2 to have run"
            }
        ]

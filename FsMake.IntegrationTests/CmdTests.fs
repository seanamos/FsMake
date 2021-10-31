module FsMake.IntegrationTests.CmdTests

open Expecto
open Expecto.Flip
open FsMake

[<Tests>]
let tests =
    testList
        "Cmd Integration Tests"
        [
            test "zero exit code is success" {
                let step1 =
                    Step.create "step1" {
                        do!
                            Cmd.createWithArgs "dotnet" [ "--help" ]
                            |> Cmd.redirectOutput Cmd.Redirect
                            |> Cmd.run
                    }

                let pipelines =
                    Pipelines.create {
                        let pipeline1 = Pipeline.create "pipeline1" { run step1 }
                        default_pipeline pipeline1
                    }

                let exitCode = pipelines |> Pipelines.runWithArgs [| "-v"; "disabled" |]

                exitCode |> Expect.equal "Expected exit code to be 0" 0
            }

            test "non-zero exit code is failure" {
                let step1 =
                    Step.create "exitZero" {
                        do!
                            Cmd.createWithArgs "dotnet" [ "does-not-exist" ]
                            |> Cmd.redirectOutput Cmd.Redirect
                            |> Cmd.run
                    }

                let pipelines =
                    Pipelines.create {
                        let pipeline1 = Pipeline.create "pipeline1" { run step1 }
                        default_pipeline pipeline1
                    }

                let exitCode = pipelines |> Pipelines.runWithArgs [| "-v"; "disabled" |]

                (exitCode, 0) |> Expect.isGreaterThan "Expected exit code to be non-zero"
            }
        ]

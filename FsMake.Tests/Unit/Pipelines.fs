module FsMake.Tests.Unit.Pipelines

open Expecto
open FsMake
open FsMake.Tests

let tests =
    testList
        "Pipelines tests"
        [
            test "builder do! adds pipeline" {
                let step = Step.create "empty" { () }
                let pipeline = Pipeline.create "test" { run step }

                let pipelines = Pipelines.create { do! pipeline }

                let pipelinesVal = pipelines.Pipelines
                let pipelinesExpr = <@ pipelinesVal @>

                let pipelinesVal = [ pipeline ]
                let expectExpr = <@ pipelinesVal @>

                teste <@ pipelinesExpr.ToString () = expectExpr.ToString () @>
            }

            test "builder let! adds pipeline" {
                let step = Step.create "empty" { () }
                let pipeline = Pipeline.create "test" { run step }

                let pipelines =
                    Pipelines.create {
                        let! _ = pipeline

                        ()
                    }

                let pipelinesVal = pipelines.Pipelines
                let pipelinesExpr = <@ pipelinesVal @>

                let pipelinesVal = [ pipeline ]
                let expectExpr = <@ pipelinesVal @>

                teste <@ pipelinesExpr.ToString () = expectExpr.ToString () @>
            }

            test "default_pipeline sets default pipeline" {
                let step = Step.create "empty" { () }
                let pipeline = Pipeline.create "test" { run step }

                let pipelines = Pipelines.create { default_pipeline pipeline }

                let defaultVal = pipelines.Default
                let defaultExpr = <@ defaultVal @>

                let defaultVal = Some pipeline
                let expectExpr = <@ defaultVal @>

                teste <@ defaultExpr.ToString () = expectExpr.ToString () @>
            }

            test "step_prefix sets prefix" {
                let step = Step.create "empty" { () }
                let pipeline = Pipeline.create "test" { run step }

                let pipelines =
                    Pipelines.create {
                        default_pipeline pipeline
                        step_prefix Prefix.Never
                    }

                teste <@ pipelines.StepPrefix = Prefix.Never @>
            }
        ]

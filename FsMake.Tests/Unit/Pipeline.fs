module FsMake.Tests.Unit.Pipeline

open Expecto
open FsMake
open FsMake.Tests

let tests =
    testList
        "Pipeline tests"
        [
            test "create should create pipeline with name" {
                let step = Step.create "emptyStep" { () }
                let pipeline = Pipeline.create "test" { run step }

                teste <@ pipeline.Name = "test" @>
            }

            test "create should create pipeline with steps" {
                let step1 = Step.create "emptyStep1" { () }
                let step2 = Step.create "emptyStep2" { () }

                let pipeline =
                    Pipeline.create "test" {
                        run step1
                        run step2
                    }

                let stages = pipeline.Stages
                let stagesExpr = <@ stages @>

                let stages = [ SequentialStage step1; SequentialStage step2 ]

                let expectExpr = <@ stages @>

                teste <@ stagesExpr.ToString () = expectExpr.ToString () @>
            }

            test "createFrom should adopt previous pipeline steps" {
                let step1 = Step.create "emptyStep1" { () }
                let step2 = Step.create "emptyStep2" { () }
                let step3 = Step.create "emptyStep2" { () }

                let pipeline1 =
                    Pipeline.create "test" {
                        run step1
                        run step2
                    }

                let pipeline2 = Pipeline.createFrom pipeline1 "test2" { run step3 }

                let stages = pipeline2.Stages
                let stagesExpr = <@ stages @>

                let stages = [ SequentialStage step1; SequentialStage step2; SequentialStage step3 ]

                let expectExpr = <@ stages @>

                teste <@ stagesExpr.ToString () = expectExpr.ToString () @>
            }
        ]

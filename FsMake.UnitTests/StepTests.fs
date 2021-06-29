module FsMake.UnitTests.StepTests

open Expecto
open Expecto.Flip
open FsMake

[<Tests>]
let tests =
    let consoleWriter =
        { new Console.IWriter with
            member _.Write(_) =
                ()
        }

    let procMon = ProcessMonitor.create consoleWriter

    let ctx : MakeContext =
        {
            PipelineName = "testPipeline"
            StepName = "testStep"
            IsParallel = false
            Console = consoleWriter
            Prefix = Console.Text ""
            PrefixOption = Prefix.WhenParallel
            ProcessMonitor = procMon
            ExtraArgs = []
        }

    testList
        "Step tests"
        [
            test "create should create step with name" {
                let step = Step.create "test" { () }

                teste <@ step.Name = "test" @>
            }

            test "context should get MakeContext" {
                let ctxResult = ctx |> Step.context

                teste <@ ctxResult = Ok ctx @>
            }

            test "fail should create StepError" {
                let failResult = ctx |> Step.fail "oh no!"

                teste <@ failResult = (MakeError [ Console.error "oh no!" ] |> Error) @>
            }

            test "failMessage should create StepError with messages" {
                let msgs = [ Console.error "msg1"; Console.error "msg2"; Console.error "msg3" ]

                let failResult = ctx |> Step.failMessages msgs

                teste <@ failResult = (MakeError msgs |> Error) @>
            }

            test "run executes step" {
                let mutable res = None

                let step = Step.create "test" { res <- Some true }

                step |> Step.Internal.run ctx |> ignore

                // compiler quotation bug workaround
                let res = res

                teste <@ res = Some true @>
            }

            test "run returns Ok result for non-failing step" {
                let step = Step.create "test" { () }

                let runResult = step |> Step.Internal.run ctx

                runResult |> Expect.isOk "Step run result should be Ok"
            }

            test "run returns Error result for failing step" {
                let step = Step.create "test" { do! Step.fail "oh no!" }

                let runResult = step |> Step.Internal.run ctx

                runResult |> Expect.isError "Step run result should be Error"
            }

            test "run returns Error result for Exception during step" {
                let step = Step.create "test" { failwith "oh no!" }

                let runResult = step |> Step.Internal.run ctx

                runResult |> Expect.isError "Step run result should be Error"
            }

            test "concatNames [3] creates correct result" {
                let step1 = Step.create "test1" { () }
                let step2 = Step.create "test2" { () }
                let step3 = Step.create "test3" { () }

                let concat = Step.Internal.concatNames [ step1; step2; step3 ]

                teste <@ concat = "test1, test2 and test3" @>
            }

            test "concatNames [2] creates correct result" {
                let step1 = Step.create "test1" { () }
                let step2 = Step.create "test2" { () }

                let concat = Step.Internal.concatNames [ step1; step2 ]

                teste <@ concat = "test1 and test2" @>
            }

            test "concatNames [1] creates correct result" {
                let step1 = Step.create "test1" { () }

                let concat = Step.Internal.concatNames [ step1 ]

                teste <@ concat = "test1" @>
            }
        ]

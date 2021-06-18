module FsMake.UnitTests.StepPartTests

open Expecto
open FsMake

[<Tests>]
let tests =
    let consoleWriter =
        { new Console.IWriter with
            member _.Write(_) =
                ()
        }

    let procMon = ProcessMonitor.create consoleWriter

    let ctx : StepContext =
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
        "StepPart tests"
        [
            test "zero creates empty part" {
                let part = StepPart.zero

                let result = part ctx

                teste <@ result = Ok () @>
            }

            test "return wraps value" {
                let part = StepPart.return' "hello"

                let result = part ctx

                teste <@ result = Ok "hello" @>
            }

            test "map maps result" {
                let part = StepPart.zero |> StepPart.map (fun _ -> "hello")

                let result = part ctx

                teste <@ result = Ok "hello" @>
            }

            test "bind continues to next on Ok" {
                let part : StepPart<string> =
                    StepPart.zero
                    |> StepPart.bind (fun _ -> fun _ -> Ok "hello")

                let result = part ctx

                teste <@ result = Ok "hello" @>
            }

            test "bind short circuits on Error" {
                let mutable run = false

                let part : StepPart<string> =
                    fun _ -> [ Console.error "oh no" ] |> StepError |> Error
                    |> StepPart.bind (fun _ ->
                        fun _ ->
                            run <- true
                            Ok "hello"
                    )

                let result = part ctx

                let run = run
                teste <@ result = ([ Console.error "oh no" ] |> StepError |> Error) @>
                teste <@ not run @>
            }

            test "zip tuples results" {
                let part1 : StepPart<string> = fun _ -> Ok "test1"
                let part2 : StepPart<string> = fun _ -> Ok "test2"

                let zipped = StepPart.zip part1 part2

                let result = zipped ctx

                teste <@ result = Ok ("test1", "test2") @>
            }

            test "context gets context" {
                let part = StepPart.context

                let result = part ctx

                teste <@ result = Ok ctx @>
            }

            test "zip returns Error result on part Error" {
                let part1 : StepPart<string> = fun _ -> [ Console.error "oh no" ] |> StepError |> Error
                let part2 : StepPart<string> = fun _ -> Ok "test2"

                let zipped1 = StepPart.zip part1 part2
                let zipped2 = StepPart.zip part2 part1

                let result1 = zipped1 ctx
                let result2 = zipped2 ctx

                let expected = [ Console.error "oh no" ] |> StepError |> Error
                teste <@ result1 = expected @>
                teste <@ result2 = expected @>
            }

            test "retry retries X attempts on exception" {
                let mutable attempt = 0

                let part : StepPart<unit> =
                    fun _ ->
                        attempt <- attempt + 1
                        failwith "oh no"
                    |> StepPart.retry 5

                part ctx |> ignore
                let attempt = attempt

                teste <@ attempt = 5 @>
            }

            test "retry retries X attempts on StepError" {
                let mutable attempt = 0

                let part : StepPart<unit> =
                    fun _ ->
                        attempt <- attempt + 1
                        [ Console.error "oh no" ] |> StepError |> Error
                    |> StepPart.retry 5

                part ctx |> ignore
                let attempt = attempt

                teste <@ attempt = 5 @>
            }

            test "retry does not retry on StepAbort" {
                let mutable attempt = 0

                let part : StepPart<unit> =
                    fun _ ->
                        attempt <- attempt + 1
                        [ Console.error "oh no" ] |> StepAbort |> Error
                    |> StepPart.retry 5

                part ctx |> ignore
                let attempt = attempt

                teste <@ attempt = 1 @>
            }

            test "memo executes only once" {
                let mutable run = 0

                let part : StepPart<string> =
                    fun _ ->
                        run <- run + 1
                        Ok "hello"
                    |> StepPart.memo

                part ctx |> ignore
                part ctx |> ignore
                let result = part ctx

                let run = run
                teste <@ run = 1 @>
                teste <@ result = Ok "hello" @>
            }

            test "memoRace executes only once" {
                let mutable run = 0

                let part : StepPart<string> =
                    fun _ ->
                        run <- run + 1
                        Ok "hello"
                    |> StepPart.memoRace

                part ctx |> ignore
                part ctx |> ignore
                let result = part ctx

                let run = run
                teste <@ run = 1 @>
                teste <@ result = Ok "hello" @>
            }

            test "memoRace can execute more than once" {
                let mutable run = 0

                let part : StepPart<string> =
                    fun _ ->
                        System.Threading.Thread.Sleep 100
                        System.Threading.Interlocked.Increment &run |> ignore
                        Ok "hello"
                    |> StepPart.memoRace

                [| part; part |]
                |> Array.Parallel.iter (fun x -> x ctx |> ignore)

                let result = part ctx

                let run = run
                teste <@ run = 2 @>
                teste <@ result = Ok "hello" @>
            }
        ]

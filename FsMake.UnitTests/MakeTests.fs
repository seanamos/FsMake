module FsMake.UnitTests.MakeTests

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

    let ctx: MakeContext =
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
        "Make tests"
        [
            test "zero creates empty make" {
                let make = Make.zero

                let result = make ctx

                teste <@ result = Ok () @>
            }

            test "return wraps value" {
                let make = Make.return' "hello"

                let result = make ctx

                teste <@ result = Ok "hello" @>
            }

            test "map maps result" {
                let make = Make.zero |> Make.map (fun _ -> "hello")

                let result = make ctx

                teste <@ result = Ok "hello" @>
            }

            test "bind continues to next on Ok" {
                let make: Make<string> = Make.zero |> Make.bind (fun _ _ -> Ok "hello")

                let result = make ctx

                teste <@ result = Ok "hello" @>
            }

            test "bind short circuits on Error" {
                let mutable run = false

                let make: Make<string> =
                    fun _ -> [ Console.error "oh no" ] |> MakeError |> Error
                    |> Make.bind (fun _ _ ->
                        run <- true
                        Ok "hello"
                    )

                let result = make ctx

                let run = run
                teste <@ result = ([ Console.error "oh no" ] |> MakeError |> Error) @>
                teste <@ not run @>
            }

            test "zip tuples results" {
                let make1: Make<string> = fun _ -> Ok "test1"
                let make2: Make<string> = fun _ -> Ok "test2"

                let zipped = Make.zip make1 make2

                let result = zipped ctx

                teste <@ result = Ok ("test1", "test2") @>
            }

            test "context gets context" {
                let make = Make.context

                let result = make ctx

                teste <@ result = Ok ctx @>
            }

            test "zip returns Error result on make Error" {
                let make1: Make<string> = fun _ -> [ Console.error "oh no" ] |> MakeError |> Error
                let make2: Make<string> = fun _ -> Ok "test2"

                let zipped1 = Make.zip make1 make2
                let zipped2 = Make.zip make2 make1

                let result1 = zipped1 ctx
                let result2 = zipped2 ctx

                let expected = [ Console.error "oh no" ] |> MakeError |> Error
                teste <@ result1 = expected @>
                teste <@ result2 = expected @>
            }

            test "retry retries X attempts on exception" {
                let mutable attempt = 0

                let make: Make<unit> =
                    fun _ ->
                        attempt <- attempt + 1
                        failwith "oh no"
                    |> Make.retry 5

                make ctx |> ignore
                let attempt = attempt

                teste <@ attempt = 5 @>
            }

            test "retry retries X attempts on MakeError" {
                let mutable attempt = 0

                let make: Make<unit> =
                    fun _ ->
                        attempt <- attempt + 1
                        [ Console.error "oh no" ] |> MakeError |> Error
                    |> Make.retry 5

                make ctx |> ignore
                let attempt = attempt

                teste <@ attempt = 5 @>
            }

            test "retry does not retry on StepAbort" {
                let mutable attempt = 0

                let make: Make<unit> =
                    fun _ ->
                        attempt <- attempt + 1
                        [ Console.error "oh no" ] |> MakeAbort |> Error
                    |> Make.retry 5

                make ctx |> ignore
                let attempt = attempt

                teste <@ attempt = 1 @>
            }

            test "memo executes only once" {
                let mutable run = 0

                let make: Make<string> =
                    fun _ ->
                        run <- run + 1
                        Ok "hello"
                    |> Make.memo

                make ctx |> ignore
                make ctx |> ignore
                let result = make ctx

                let run = run
                teste <@ run = 1 @>
                teste <@ result = Ok "hello" @>
            }

            test "memoRace executes only once" {
                let mutable run = 0

                let make: Make<string> =
                    fun _ ->
                        run <- run + 1
                        Ok "hello"
                    |> Make.memoRace

                make ctx |> ignore
                make ctx |> ignore
                let result = make ctx

                let run = run
                teste <@ run = 1 @>
                teste <@ result = Ok "hello" @>
            }

            test "memoRace can execute more than once" {
                let mutable run = 0

                let make: Make<string> =
                    fun _ ->
                        System.Threading.Thread.Sleep 100
                        System.Threading.Interlocked.Increment &run |> ignore
                        Ok "hello"
                    |> Make.memoRace

                [| make; make |] |> Array.Parallel.iter (fun x -> x ctx |> ignore)

                let result = make ctx

                let run = run
                teste <@ run = 2 @>
                teste <@ result = Ok "hello" @>
            }
        ]

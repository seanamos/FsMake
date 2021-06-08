namespace FsMake

open System
open System.Threading

type Pipelines =
    { Default: Pipeline option
      Pipelines: Pipeline list }

module Pipelines =
    module Builder =
        type DefaultPipeline = DefaultPipeline of Pipeline

    type Builder() =
        let mutable pipelines = { Default = None; Pipelines = [] }

        member _.Delay(f: unit -> 'T) : 'T =
            f ()

        member _.For(_: unit, x: unit -> unit) : unit =
            x ()

        member _.Combine(_: 'T, _: 'T) : unit =
            ()

        member _.Yield() : unit =
            ()

        member _.Yield(pipeline: Pipeline) : unit =
            pipelines <-
                { pipelines with
                      Pipelines = pipelines.Pipelines @ [ pipeline ] }

        member _.Yield(Builder.DefaultPipeline pipeline) : unit =
            pipelines <-
                { pipelines with
                      Default = Some pipeline }

        member _.Bind(pipeline: Pipeline, binder: Pipeline -> unit) : unit =
            pipelines <-
                { pipelines with
                      Pipelines = pipelines.Pipelines @ [ pipeline ] }

            binder pipeline

        member _.Return(_) =
            ()

        member _.Run(_: unit) : Pipelines =
            pipelines

    let create = Builder ()

    let defaultPipeline (pipeline: Pipeline) =
        Builder.DefaultPipeline pipeline

    [<AutoOpen>]
    module internal Internal =
        type FindPipelineResult =
            | DefaultPipeline of Pipeline
            | NoDefault
            | PipelineFound of pipeline: Pipeline
            | PipelineNotFound of arg: string

        let findPipelineFromArg (pipelines: Pipelines) (arg: string option) : FindPipelineResult =
            match arg with
            | None ->
                match pipelines.Default with
                | Some x -> DefaultPipeline x
                | None -> NoDefault
            | Some arg ->
                pipelines.Pipelines
                |> List.tryFind (fun x -> x.Name.Equals (arg, StringComparison.OrdinalIgnoreCase))
                |> function
                | Some pipeline -> PipelineFound pipeline
                | None -> PipelineNotFound arg

    let runWithArgs (args: string []) (pipelines: Pipelines) : unit =
        let parsedArgs = Cli.parseArgs args

        match parsedArgs with
        | Error x ->
            let writer = Console.defaultWriter

            Cli.printUsage writer x
        | Ok args ->
            let consoleOutput = args.ConsoleOutput |> Cli.ConsoleOutput.toConsoleOutputType
            let verbosity = args.Verbosity |> Cli.Verbosity.toConsoleVerbosity
            let writer = Console.createWriter consoleOutput verbosity

            match args.PrintHelp with
            | true -> Cli.printUsage writer []
            | false ->
                let findResult = args.Pipeline |> findPipelineFromArg pipelines

                match findResult with
                | DefaultPipeline x
                | PipelineFound x ->
                    use cts = new CancellationTokenSource ()

                    let cancelHandler =
                        ConsoleCancelEventHandler (fun _ x ->
                            x.Cancel <- true
                            cts.Cancel ()
                        )

                    Console.CancelKeyPress.AddHandler (cancelHandler)

                    let success = x |> Pipeline.run writer cts.Token

                    Console.CancelKeyPress.RemoveHandler (cancelHandler)

                    if success then exit 0 else exit 1
                | NoDefault ->
                    Console.error "No default pipeline was defined and no pipeline was specified"
                    |> writer.WriteLine

                    exit 1
                | PipelineNotFound arg ->
                    sprintf "Could not find a pipeline with the name "
                    |> Console.error
                    |> Console.appendToken arg
                    |> writer.WriteLine

                    exit 1

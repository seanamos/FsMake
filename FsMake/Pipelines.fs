namespace FsMake

open System
open System.Threading

type Pipelines =
    { Default: Pipeline option
      Pipelines: Pipeline list }

module Pipelines =
    type Builder() =
        let mutable pipelines = { Default = None; Pipelines = [] }

        member internal _.Pipelines
            with get () = pipelines
            and set (value) = pipelines <- value

        member _.Zero(vars: 'a) =
            ((), vars)

        member _.Delay(f: unit -> 'T) : 'T =
            f ()

        member _.Combine(_: 'T, _: 'T) : unit =
            ()

        member _.Yield(vars: 'a) : unit * 'a =
            ((), vars)

        member _.Bind(pipeline: Pipeline, binder: Pipeline -> unit * 'a) : unit * 'a =
            pipelines <-
                { pipelines with
                      Pipelines = pipelines.Pipelines @ [ pipeline ] }

            binder pipeline

        [<CustomOperation("add", MaintainsVariableSpace = true)>]
        member _.Add(((), vars: 'a), [<ProjectionParameter>] f: 'a -> Pipeline) : unit * 'a =
            let pipeline = f vars

            pipelines <-
                { pipelines with
                      Pipelines = pipelines.Pipelines @ [ pipeline ] }

            ((), vars)

        [<CustomOperation("default_pipeline", MaintainsVariableSpace = true)>]
        member _.DefaultPipeline(((), vars: 'a), [<ProjectionParameter>] f: 'a -> Pipeline) : unit * 'a =
            let pipeline = f vars

            pipelines <-
                { pipelines with
                      Default = Some pipeline }

            ((), vars)

        member _.Run(_: 'a) : Pipelines =
            pipelines

    let create = Builder ()

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

[<AutoOpen>]
module PipelinesBuilderExtensions =

    // pretty horrible hack, but it allows us to use do! syntax
    type Pipelines.Builder with
        member this.Bind(pipeline: Pipeline, binder: unit -> unit * 'a) : unit * 'a =
            this.Pipelines <-
                { this.Pipelines with
                      Pipelines = this.Pipelines.Pipelines @ [ pipeline ] }

            binder ()

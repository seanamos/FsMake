namespace FsMake

open System
open System.Threading

type Pipelines =
    { Default: Pipeline option
      Pipelines: Pipeline list
      StepPrefix: Prefix.PrefixOption }

module Pipelines =
    [<Sealed>]
    type Builder() =
        member _.Zero(vars: 'a) =
            ((), vars)

        member _.Delay(f: unit -> 'T) : 'T =
            f ()

        member _.Combine(_: 'T, _: 'T) : unit =
            ()

        member _.Yield(vars: 'a) : Pipelines * 'a =
            ({ Default = None
               Pipelines = []
               StepPrefix = Prefix.WhenParallel },
             vars)

        member _.For(_: _, binder: unit -> Pipelines * 'a) : Pipelines * 'a =
            binder ()

        member _.Bind(pipeline: Pipeline, binder: Pipeline -> Pipelines * 'a) : Pipelines * 'a =
            let (pipelines, vars) = binder pipeline

            let pipelines =
                { pipelines with
                      Pipelines = pipelines.Pipelines @ [ pipeline ] }

            (pipelines, vars)

        [<CustomOperation("add", MaintainsVariableSpace = true)>]
        member _.Add((pipelines: Pipelines, vars: 'a), [<ProjectionParameter>] f: 'a -> Pipeline) : Pipelines * 'a =
            let pipeline = f vars

            let pipelines =
                { pipelines with
                      Pipelines = pipelines.Pipelines @ [ pipeline ] }

            (pipelines, vars)

        [<CustomOperation("default_pipeline", MaintainsVariableSpace = true)>]
        member _.DefaultPipeline((pipelines: Pipelines, vars: 'a), [<ProjectionParameter>] f: 'a -> Pipeline) : Pipelines * 'a =
            let pipeline = f vars

            let pipelines =
                { pipelines with
                      Default = Some pipeline }

            (pipelines, vars)

        [<CustomOperation("step_prefix", MaintainsVariableSpace = true)>]
        member _.StepPrefix((pipelines: Pipelines, vars: 'a), [<ProjectionParameter>] f: 'a -> Prefix.PrefixOption) : Pipelines * 'a =
            let option = f vars

            let pipelines = { pipelines with StepPrefix = option }

            (pipelines, vars)

        member _.Run((pipelines: Pipelines, _: 'a)) : Pipelines =
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
        | Error (args, errors) ->
            let writer = Console.Internal.defaultWriter

            Cli.printUsage writer args errors
        | Ok args ->
            let consoleOutput = args.ConsoleOutput |> Cli.ConsoleOutput.toConsoleOutputType
            let verbosity = args.Verbosity |> Cli.Verbosity.toConsoleVerbosity
            let writer = Console.createWriter consoleOutput verbosity

            match args.PrintHelp with
            | true -> Cli.printUsage writer args []
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

                    let args : Pipeline.RunArgs =
                        { Writer = writer
                          ExtraArgs = args.ExtraArgs
                          PrefixOption = pipelines.StepPrefix
                          CancellationToken = cts.Token }

                    let success = x |> Pipeline.run args

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
        member _.Bind(pipeline: Pipeline, binder: unit -> Pipelines * 'a) : Pipelines * 'a =
            let (pipelines, vars) = binder ()

            let pipelines =
                { pipelines with
                      Pipelines = pipelines.Pipelines @ [ pipeline ] }

            (pipelines, vars)

    type Pipelines.Builder with
        member this.Bind(pipeline: Pipeline, binder: unit -> unit * unit) : Pipelines * unit =
            let (pipelines, vars) = this.Yield (())

            let pipelines =
                { pipelines with
                      Pipelines = pipelines.Pipelines @ [ pipeline ] }

            (pipelines, vars)

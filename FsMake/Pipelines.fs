namespace FsMake

open System
open System.Threading
open System.Runtime.InteropServices

/// <summary>
/// Represents a set of pipelines and settings.
/// </summary>
type Pipelines =
    {
        Default: Pipeline option
        Pipelines: Pipeline list
        StepPrefix: Prefix.PrefixOption
    }

/// <summary>
/// Module for creating and working with <see cref="T:Pipelines" />.
/// </summary>
module Pipelines =

    /// <summary>
    /// A <see cref="T:Pipelines" /> computation expression builder.
    /// <c>do!</c> and <c>let!</c> in the computation expression add a pipeline
    /// to the <see cref="T:Pipelines" /> being built.
    /// </summary>
    [<Sealed>]
    type Builder() =
        member _.Zero(vars: 'a) : unit * 'a =
            ((), vars)

        member _.Delay(f: unit -> 'T) : 'T =
            f ()

        member _.Combine(_: 'T, _: 'T) : unit =
            ()

        member _.Yield(vars: 'a) : Pipelines * 'a =
            let pipelines =
                {
                    Default = None
                    Pipelines = []
                    StepPrefix = Prefix.WhenParallel
                }

            (pipelines, vars)

        member _.For(_: _, binder: unit -> Pipelines * 'a) : Pipelines * 'a =
            binder ()

        member _.Bind(pipeline: Pipeline, binder: Pipeline -> Pipelines * 'a) : Pipelines * 'a =
            let (pipelines, vars) = binder pipeline

            let pipelines =
                { pipelines with
                    Pipelines = pipelines.Pipelines @ [ pipeline ]
                }

            (pipelines, vars)

        [<CustomOperation("add", MaintainsVariableSpace = true)>]
        member _.Add((pipelines: Pipelines, vars: 'a), [<ProjectionParameter>] f: 'a -> Pipeline) : Pipelines * 'a =
            let pipeline = f vars

            let pipelines =
                { pipelines with
                    Pipelines = pipelines.Pipelines @ [ pipeline ]
                }

            (pipelines, vars)

        [<CustomOperation("default_pipeline", MaintainsVariableSpace = true)>]
        member _.DefaultPipeline((pipelines: Pipelines, vars: 'a), [<ProjectionParameter>] f: 'a -> Pipeline) : Pipelines * 'a =
            let pipeline = f vars

            let pipelines =
                { pipelines with
                    Default = Some pipeline
                }

            (pipelines, vars)

        [<CustomOperation("step_prefix", MaintainsVariableSpace = true)>]
        member _.StepPrefix((pipelines: Pipelines, vars: 'a), [<ProjectionParameter>] f: 'a -> Prefix.PrefixOption) : Pipelines * 'a =
            let option = f vars

            let pipelines = { pipelines with StepPrefix = option }

            (pipelines, vars)

        member _.Run((pipelines: Pipelines, _: 'a)) : Pipelines =
            pipelines

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

        let runWithParsedArgs (args: Cli.Args) (pipelines: Pipelines) : int =
            let consoleOutput = args.ConsoleOutput |> Cli.ConsoleOutput.toConsoleOutputType

            let verbosity = args.Verbosity |> Cli.Verbosity.toConsoleVerbosity
            let writer = Console.createWriter consoleOutput verbosity

            if args.PrintHelp then
                Cli.printUsage writer args []
                0
            else
                let findResult = args.Pipeline |> findPipelineFromArg pipelines

                match findResult with
                | DefaultPipeline x
                | PipelineFound x ->
                    // 😥 temporary workaround for https://github.com/dotnet/fsharp/issues/11729
                    if not <| RuntimeInformation.IsOSPlatform (OSPlatform.Windows) then
                        let fixedPathEnv =
                            Environment
                                .GetEnvironmentVariable("PATH")
                                .Replace(';', ':')
                                .TrimEnd (':')

                        Environment.SetEnvironmentVariable ("PATH", fixedPathEnv)

                    use cts = new CancellationTokenSource ()

                    let cancelHandler =
                        ConsoleCancelEventHandler (fun _ x ->
                            x.Cancel <- true
                            cts.Cancel ()
                        )

                    Console.CancelKeyPress.AddHandler (cancelHandler)

                    let args : Pipeline.RunArgs =
                        {
                            Writer = writer
                            ExtraArgs = args.ExtraArgs
                            PrefixOption = pipelines.StepPrefix
                            CancellationToken = cts.Token
                        }

                    let success = x |> Pipeline.run args

                    Console.CancelKeyPress.RemoveHandler (cancelHandler)

                    if success then 0 else 1
                | NoDefault ->
                    Console.error "No default pipeline was defined and no pipeline was specified"
                    |> writer.WriteLine

                    1
                | PipelineNotFound arg ->
                    Console.error "Could not find a pipeline with the name "
                    |> Console.appendToken arg
                    |> writer.WriteLine

                    1

    /// <summary>
    /// Use a computation expression builder to define <see cref="T:Pipelines" />.
    /// </summary>
    /// <returns>A <see cref="T:Pipelines.Builder" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let clean = Step.create "clean" { () }
    /// let build = Step.create "build" { () }
    /// let testUnit = Step.create "test:unit" { () }
    /// let testInt = Step.create "test:integration" { () }
    ///
    /// let pipelines =
    ///     Pipelines.create {
    ///         // adds the build pipeline to the `Pipelines`
    ///         let! build = Pipeline.create "build" {
    ///             run clean
    ///             run build
    ///         }
    ///
    ///         // adds the test pipeline to the <see cref="Pipelines" />
    ///         do! Pipeline.createFrom build "test" {
    ///             run_parallel [ testUnit; testInt ]
    ///         }
    ///
    ///         default_pipeline build
    ///     }
    /// </code>
    /// </example>
    let create = Builder ()

    /// <summary>
    /// Runs a <see cref="T:Pipeline" /> from the given <c>pipelines</c> based on the command line arguments given in <c>args</c>.
    /// <para>
    /// Based on success or failure, returns a <c>0</c> or <c>1</c> exit code.
    /// </para>
    /// </summary>
    /// <param name="args">Command line arguments to be parsed.</param>
    /// <param name="pipelines"><see cref="T:Pipelines" /> definition.</param>
    let runWithArgs (args: string []) (pipelines: Pipelines) : int =
        let parsedArgs = Cli.parseArgs args

        match parsedArgs with
        | Error (args, errors) ->
            let writer = Console.Internal.defaultWriter

            Cli.printUsage writer args errors
            1
        | Ok args -> pipelines |> runWithParsedArgs args

    /// <summary>
    /// Runs a <see cref="T:Pipeline" /> from the given <c>pipelines</c> based on the command line arguments given in <c>args</c>.
    /// <para>
    /// Based on success or failure, exits the process with a <c>0</c> or <c>1</c> exit code.
    /// </para>
    /// </summary>
    /// <param name="args">Command line arguments to be parsed.</param>
    /// <param name="pipelines"><see cref="T:Pipelines" /> definition.</param>
    let runWithArgsAndExit (args: string []) (pipelines: Pipelines) : unit =
        pipelines |> runWithArgs args |> exit |> ignore


/// <summary>
/// Module containing extension methods for <see cref="T:Pipelines.Builder" />.
/// </summary>
/// <exclude />
[<AutoOpen>]
module PipelinesBuilderExtensions =

    // pretty horrible hack, but it allows us to use do! syntax
    type Pipelines.Builder with
        member _.Bind(pipeline: Pipeline, binder: unit -> Pipelines * 'a) : Pipelines * 'a =
            let (pipelines, vars) = binder ()

            let pipelines =
                { pipelines with
                    Pipelines = pipelines.Pipelines @ [ pipeline ]
                }

            (pipelines, vars)

    type Pipelines.Builder with
        member this.Bind(pipeline: Pipeline, binder: unit -> unit * unit) : Pipelines * unit =
            let (pipelines, vars) = this.Yield (())

            let pipelines =
                { pipelines with
                    Pipelines = pipelines.Pipelines @ [ pipeline ]
                }

            (pipelines, vars)

namespace FsMake

open System
open System.Threading
open System.Runtime.InteropServices

/// <summary>
/// Represents a set of pipelines and settings.
/// </summary>
type Pipelines =
    {
        /// <summary>
        /// Gets the <see cref="T:Pipeline" /> that will be executed by default.
        /// </summary>
        Default: Pipeline option

        /// <summary>
        /// Gets the <see cref="T:Pipeline" />s.
        /// </summary>
        Pipelines: Pipeline list

        /// <summary>
        /// Gets the <see cref="T:FsMake.Prefix.PrefixOption" /> for the <see cref="T:Step" />s that will run.
        /// </summary>
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

        /// <summary>
        /// Adds a <see cref="T:Pipeline" /> to the <see cref="T:Pipelines" />.
        /// </summary>
        /// <param name="state">The current state of the computation expression.</param>
        /// <param name="pipeline">The <see cref="T:Pipeline" /> to be added.</param>
        /// <typeparam name="'a">The types of the variables in the computation expression's state.</typeparam>
        /// <returns>Updated computation expression state.</returns>
        [<CustomOperation("add", MaintainsVariableSpace = true)>]
        member _.Add(state: (Pipelines * 'a), [<ProjectionParameter>] pipeline: 'a -> Pipeline) : Pipelines * 'a =
            let pipelines, vars = state
            let newPipeline = pipeline vars

            let pipelines =
                { pipelines with
                    Pipelines = pipelines.Pipelines @ [ newPipeline ]
                }

            (pipelines, vars)

        /// <summary>
        /// Sets the default <see cref="T:Pipeline" /> to be run when a pipeline is not specified in the arguments.
        /// </summary>
        /// <param name="state">The current state of the computation expression.</param>
        /// <param name="pipeline">The <see cref="T:Pipeline" /> to be used as the default.</param>
        /// <typeparam name="'a">The types of the variables in the computation expression's state.</typeparam>
        /// <returns>Updated computation expression state.</returns>
        [<CustomOperation("default_pipeline", MaintainsVariableSpace = true)>]
        member _.DefaultPipeline(state: (Pipelines * 'a), [<ProjectionParameter>] pipeline: 'a -> Pipeline) : Pipelines * 'a =
            let pipelines, vars = state
            let defaultPipeline = pipeline vars

            let pipelines =
                { pipelines with
                    Default = Some defaultPipeline
                }

            (pipelines, vars)

        /// <summary>
        /// Sets when to prefix a <see cref="T:Step" />'s console output.
        /// </summary>
        /// <param name="state">The current state of the computation expression.</param>
        /// <param name="prefixOption">The <see cref="T:Prefix.PrefixOption" />.</param>
        /// <typeparam name="'a">The types of the variables in the computation expression's state.</typeparam>
        /// <returns>Updated computation expression state.</returns>
        [<CustomOperation("step_prefix", MaintainsVariableSpace = true)>]
        member _.StepPrefix(state: (Pipelines * 'a), [<ProjectionParameter>] prefixOption: 'a -> Prefix.PrefixOption) : Pipelines * 'a =
            let pipelines, vars = state
            let option = prefixOption vars

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

        let createConsoleWriter (args: Cli.ParsedArgs) : Console.IWriter =
            let consoleOutput = args.ConsoleOutput |> Cli.ConsoleOutput.toConsoleOutputType
            let verbosity = args.Verbosity |> Cli.Verbosity.toConsoleVerbosity

            Console.createWriter consoleOutput verbosity

        let runWithParsedArgs (args: Cli.ParsedArgs) (pipelines: Pipelines) : int =
            let writer = createConsoleWriter args

            if args.PrintHelp then
                Cli.printUsage writer args [] pipelines.Pipelines
                0
            else
                if not args.NoLogo then Console.Info |> Cli.printLogo writer

                let findResult = args.Pipeline |> findPipelineFromArg pipelines

                match findResult with
                | DefaultPipeline x
                | PipelineFound x ->
                    // ???? temporary workaround for https://github.com/dotnet/fsharp/issues/11729
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

                    let args: Pipeline.RunArgs =
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

            Cli.printUsage writer args errors pipelines.Pipelines
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

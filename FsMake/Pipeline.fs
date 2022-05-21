namespace FsMake

open System
open System.Threading

/// <summary>
/// Represents a pipeline and its stages.
/// </summary>
type Pipeline =
    {
        /// <summary>
        /// Gets the name of the <see cref="T:Pipeline" />.
        /// </summary>
        Name: string

        /// <summary>
        /// Gets the description of the <see cref="T:Pipeline" />.
        /// </summary>
        Description: string option

        /// <summary>
        /// Gets the <see cref="T:Stage" />s.
        /// </summary>
        Stages: Stage list
    }

/// <summary>
/// Module for creating and working with a <see cref="T:Pipeline" />.
/// </summary>
module Pipeline =
    [<AutoOpen>]
    module internal Internal =
        type Context =
            {
                Pipeline: Pipeline
                Console: Console.IWriter
                PrefixOption: Prefix.PrefixOption
                ProcessMonitor: ProcessMonitor.Agent
                LongestStepNameLength: int
                ExtraArgs: string list
            }

        type StepResult =
            | Success of step: Step * stat: Step.Internal.RunStat
            | Failed of step: Step * stat: Step.Internal.RunStat * err: MakeError
            | Skipped of step: Step

        module StepResult =
            let createSkippedList (steps: Step list) : StepResult list =
                steps |> List.map Skipped

            let anyFailed (pipelineResults: StepResult list) : bool =
                pipelineResults
                |> List.tryFind (
                    function
                    | Failed _ -> true
                    | _ -> false
                )
                |> Option.isSome

            let totalTime (pipelineResults: StepResult list) : TimeSpan =
                pipelineResults
                |> List.fold
                    (fun state x ->
                        match x with
                        | Success (_, stat) -> stat.ExecutionTime + state
                        | Failed (_, stat, _) -> stat.ExecutionTime + state
                        | Skipped _ -> state
                    )
                    TimeSpan.Zero

            let printResults (writer: Console.IWriter) (pipelineResults: StepResult list) : unit =
                let formatTime (time: TimeSpan) =
                    time.ToString (@"mm\:ss\:fff")

                let totalTime = pipelineResults |> totalTime

                Console.Info |> writer.WriteLine
                Console.Info |> Console.message ("╔══════════════════════════════════════╦═══════════╗") |> writer.WriteLine

                pipelineResults
                |> List.iter (fun x ->
                    match x with
                    | Success (_, stat) ->
                        Console.Info
                        |> Console.message "║ "
                        |> Console.appendColor Console.successColor (sprintf "☑ %-35s" stat.StepName)
                        |> Console.append (sprintf "║ %s ║" (stat.ExecutionTime |> formatTime))
                        |> writer.WriteLine
                    | Failed (_, stat, _) ->
                        Console.Info
                        |> Console.message "║ "
                        |> Console.appendColor Console.errorColor (sprintf "☒ %-25s" stat.StepName)
                        |> Console.append (sprintf " (failed) ║ %s ║" (stat.ExecutionTime |> formatTime))
                        |> writer.WriteLine
                    | Skipped step ->
                        Console.Info
                        |> Console.message "║ "
                        |> Console.appendColor Console.warnColor (sprintf "☐ %-24s" step.Name)
                        |> Console.append " (skipped) ║ 00:00:000 ║"
                        |> writer.WriteLine
                )

                Console.Info |> Console.message ("╠══════════════════════════════════════╬═══════════╣") |> writer.WriteLine

                Console.Info
                |> Console.message (sprintf "║ %-37s║ %s ║" "Total" (totalTime |> formatTime))
                |> Console.append Environment.NewLine
                |> Console.append "╚══════════════════════════════════════╩═══════════╝"
                |> writer.WriteLine

        let createMakeContext (ctx: Context) (isParallel: bool) (step: Step) : MakeContext =
            let prefix = Prefix.Internal.createPrefix ctx.LongestStepNameLength step.Name

            {
                PipelineName = ctx.Pipeline.Name
                StepName = step.Name
                IsParallel = isParallel
                Console = ctx.Console
                Prefix = prefix
                PrefixOption = ctx.PrefixOption
                ProcessMonitor = ctx.ProcessMonitor
                ExtraArgs = ctx.ExtraArgs
            }

        type RunStepArgs =
            {
                RunNextStage: StepResult list -> Stage list -> StepResult list
                Context: Context
                AccResults: StepResult list
                RemainingStages: Stage list
            }

        let runStep (args: RunStepArgs) (step: Step) : StepResult list =
            let ctx = createMakeContext args.Context false step
            let stepResult = step |> Step.Internal.run ctx

            match stepResult with
            | Ok stat ->
                args.RemainingStages
                |> args.RunNextStage (args.AccResults @ [ Success (step, stat) ])
            | Error (stat, err) ->
                err
                |> MakeError.toConsoleMessage
                |> Prefix.Internal.addOptionalPrefixes
                    {
                        IsParallel = false
                        PrefixOption = ctx.PrefixOption
                        Prefix = ctx.Prefix
                    }
                |> args.Context.Console.WriteLines

                args.AccResults @ [ Failed (step, stat, err) ]

        let runParallelSteps (args: RunStepArgs) (steps: Step list) : StepResult list =
            let results =
                steps
                |> Array.ofList
                |> Array.Parallel.map (fun step ->
                    let ctx = createMakeContext args.Context true step
                    let stepResult = step |> Step.Internal.run ctx

                    (step, ctx, stepResult)
                )
                |> Array.map (
                    function
                    | (step, _, Ok stat) -> Success (step, stat)
                    | (step, ctx, Error (stat, err)) ->
                        err
                        |> MakeError.toConsoleMessage
                        |> Prefix.Internal.addOptionalPrefixes
                            {
                                IsParallel = true
                                PrefixOption = ctx.PrefixOption
                                Prefix = ctx.Prefix
                            }
                        |> args.Context.Console.WriteLines

                        Failed (step, stat, err)
                )
                |> List.ofArray

            if results |> StepResult.anyFailed then
                args.AccResults @ results
            else
                args.RemainingStages |> args.RunNextStage (args.AccResults @ results)

        let runStages (ctx: Context) (stages: Stage list) : StepResult list =
            let rec nextStage accResults remStages =
                match remStages with
                | [] -> accResults
                | x :: xs ->
                    let stepArgs =
                        {
                            RunNextStage = nextStage
                            Context = ctx
                            AccResults = accResults
                            RemainingStages = xs
                        }

                    match x with
                    | SequentialStage step ->
                        x  |> Stage.printHeader ctx.Console
                        step |> runStep stepArgs
                    | ParallelStage steps ->
                        x |> Stage.printHeader ctx.Console
                        steps |> runParallelSteps stepArgs
                    | SequentialMaybeStage (step, cond) ->
                        if cond then
                            x |> Stage.printHeader ctx.Console

                            step |> runStep stepArgs
                        else
                            Console.Info |> ctx.Console.WriteLine
                            Console.warn "Skipping step "
                            |> Console.appendToken step.Name
                            |> Console.append ", condition not met"
                            |> ctx.Console.WriteLine

                            xs |> nextStage (accResults @ [ Skipped step ])
                    | ParallelMaybeStage (steps, cond) ->
                        let stepNames = steps |> Step.Internal.concatNames

                        if cond then
                            x |> Stage.printHeader ctx.Console

                            steps |> runParallelSteps stepArgs
                        else
                            Console.Info |> ctx.Console.WriteLine
                            Console.warn "Skipping step(s) "
                            |> Console.appendToken stepNames
                            |> Console.append ", condition not met"
                            |> ctx.Console.WriteLine

                            let skipResults = steps |> StepResult.createSkippedList

                            xs |> nextStage (accResults @ skipResults)
                    | ParallelMaybesStage (pmaybes) ->
                        let (toRun, toSkip) = pmaybes |> ParallelMaybe.Internal.partionedSteps

                        if toSkip.Length > 0 then
                            Console.Info |> ctx.Console.WriteLine
                            Console.warn "Skipping step(s) "
                            |> Console.appendToken (toSkip |> Step.Internal.concatNames)
                            |> Console.append ", condition not met"
                            |> ctx.Console.WriteLine

                        let skipResults = toSkip |> StepResult.createSkippedList

                        [ ParallelStage toRun ] @ xs |> nextStage (accResults @ skipResults)

            stages |> nextStage []

    /// <summary>
    /// A <see cref="T:Pipeline" /> computation expression builder.
    /// </summary>
    /// <param name="name">Name of the pipeline.</param>
    /// <param name="from">A <see cref="T:Pipeline" /> to use as a base. A new pipeline will be created from this one and have stages appended.</param>
    [<Sealed>]
    type Builder(name: string, ?from: Pipeline) =
        let mutable pipeline =
            match from with
            | Some x -> { x with Name = name }
            | None ->
                {
                    Name = name
                    Description = None
                    Stages = []
                }

        member _.Zero() : unit =
            ()

        member _.Delay(f: unit -> 'T) : 'T =
            f ()

        member _.For(_: unit, x: unit -> unit) : unit =
            x ()

        member _.Yield(_: unit) : unit =
            ()

        member _.Yield(pmaybes: ParallelMaybe list) : unit =
            pipeline <-
                { pipeline with
                    Stages = pipeline.Stages @ [ ParallelMaybesStage pmaybes ]
                }

        member _.Run(_: unit) =
            pipeline

        member _.Combine(_: 'T, _: 'T) : unit =
            ()

        /// <summary>
        ///
        /// </summary>
        /// <param name="state"></param>
        /// <param name="desc"></param>
        /// <returns>Unit.</returns>
        [<CustomOperation("desc")>]
        member _.Desc(state: unit, desc: string) : unit =
            pipeline <-
                { pipeline with
                    Description = Some desc
                }

        /// <summary>
        /// Adds a <see cref="T:Step" /> that will always be run.
        /// </summary>
        /// <param name="state">Unused.</param>
        /// <param name="step">The <see cref="T:Step" /> to add.</param>
        /// <returns>Unit.</returns>
        [<CustomOperation("run")>]
        member _.RunStep(state: unit, step: Step) : unit =
            pipeline <-
                { pipeline with
                    Stages = pipeline.Stages @ [ SequentialStage step ]
                }

        /// <summary>
        /// Adds a <see cref="T:Step" /> that will be run conditionally.
        /// <para>The <see cref="T:Step" /> will only run if <paramref name="cond" /> is true.</para>
        /// </summary>
        /// <param name="state">Unused.</param>
        /// <param name="step">The <see cref="T:Step" /> to conditionally run.</param>
        /// <param name="cond">The condition that must be true for the step to run.</param>
        /// <returns>Unit.</returns>
        [<CustomOperation("run_maybe")>]
        member _.RunMaybe(state: unit, step: Step, cond: bool) : unit =
            pipeline <-
                { pipeline with
                    Stages = pipeline.Stages @ [ SequentialMaybeStage (step, cond) ]
                }

        /// <summary>
        ///  Adds a list of <see cref="T:Step" />s to be run in parralel.
        /// </summary>
        /// <param name="state">Unused.</param>
        /// <param name="steps">The list of <see cref="T:Step" />s that will be run in parallel.</param>
        /// <returns></returns>
        [<CustomOperation("run_parallel")>]
        member _.RunParallel(state: unit, steps: Step list) : unit =
            pipeline <-
                { pipeline with
                    Stages = pipeline.Stages @ [ ParallelStage steps ]
                }

        /// <summary>
        /// Adds steps that will be run in parallel.
        /// <para>The steps will only run if <paramref name="cond" /> is true.</para>
        /// </summary>
        /// <param name="state">Unused.</param>
        /// <param name="steps">The steps to conditionally run in parallel.</param>
        /// <param name="cond">The condition that must be true for the steps to run.</param>
        /// <returns>Unit.</returns>
        [<CustomOperation("run_parallel_maybe")>]
        member _.RunParallelMaybe(state: unit, steps: Step list, cond: bool) : unit =
            pipeline <-
                { pipeline with
                    Stages = pipeline.Stages @ [ ParallelMaybeStage (steps, cond) ]
                }


    /// <summary>
    /// Creates a pipeline using a <see cref="T:FsMake.PipelineModule.Builder" /> computation expression.
    /// </summary>
    /// <param name="name">The name of the pipeline.</param>
    /// <returns>A <see cref="T:FsMake.PipelineModule.Builder" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let emptyStep = Step.create "emptyStep" { () }
    /// let pipeline =
    ///     Pipeline.create "pipeline" {
    ///         run emptyStep
    ///     }
    /// </code>
    /// </example>
    let create (name: string) : Builder =
        Builder (name)

    /// <summary>
    /// Creates a pipeline using a <see cref="T:FsMake.PipelineModule.Builder" /> computation expression.
    /// The <c>pipeline</c> parameter is an exsting pipeline to use as a base.
    /// This does not modify the existing <see cref="T:Pipeline" />, it returns a new <see cref="T:Pipeline" /> with additional stages.
    /// </summary>
    /// <param name="pipeline">A <see cref="T:Pipeline" /> to use as a base to build upon. A new <see cref="T:Pipeline" /> will be created from this one and have stages appended.</param>
    /// <param name="name">The name of the new <see cref="T:Pipeline" />.</param>
    /// <returns>A <see cref="T:FsMake.PipelineModule.Builder" />.</returns>
    let createFrom (pipeline: Pipeline) (name: string) : Builder =
        Builder (name, pipeline)

    /// <summary>
    /// Required arguments for running a <see cref="T:Pipeline" />.
    /// </summary>
    type RunArgs =
        {
            Writer: Console.IWriter
            ExtraArgs: string list
            PrefixOption: Prefix.PrefixOption
            CancellationToken: CancellationToken
        }

    /// <summary>
    /// Runs a pipeline.
    /// </summary>
    /// <param name="args">Required arguments.</param>
    /// <param name="pipeline">The <see cref="T:Pipeline" /> to run.</param>
    /// <returns><c>true</c> if the pipeline succeeded, <c>false</c> if there was any failure.</returns>
    let run (args: RunArgs) (pipeline: Pipeline) : bool =
        let longestNameLength = pipeline.Stages |> Stage.longestStepNameLength
        use procMonitor = ProcessMonitor.create (args.Writer)

        use __ = args.CancellationToken.Register (fun () -> procMonitor |> ProcessMonitor.killAll)

        let context: Context =
            {
                Pipeline = pipeline
                Console = args.Writer
                PrefixOption = args.PrefixOption
                ProcessMonitor = procMonitor
                LongestStepNameLength = longestNameLength
                ExtraArgs = args.ExtraArgs
            }

        Console.info "Running pipeline "
        |> Console.appendToken pipeline.Name
        |> Console.append " 🚀"
        |> args.Writer.WriteLine

        let results = pipeline.Stages |> runStages context

        results |> StepResult.printResults args.Writer

        Console.Info |> args.Writer.WriteLine

        let failed = results |> StepResult.anyFailed

        if failed then
            Console.Info
            |> Console.statusMessage Console.errorColor ""
            |> Console.appendToken pipeline.Name
            |> Console.append " pipeline failed 💥"
            |> args.Writer.WriteLine
        else
            Console.Info
            |> Console.statusMessage Console.successColor ""
            |> Console.appendToken pipeline.Name
            |> Console.append " pipeline complete 🍻"
            |> args.Writer.WriteLine

        not failed

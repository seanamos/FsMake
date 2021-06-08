namespace FsMake

open System
open System.Threading

type Pipeline = { Name: string; Stages: Stage list }

module Pipeline =
    [<AutoOpen>]
    module internal Internal =
        type StepResult =
            | Success of step: Step * stat: Step.RunStat
            | Failed of step: Step * stat: Step.RunStat * err: StepError
            | Skipped of step: Step

        module StepResult =
            let createSkippedList (steps: Step list) : StepResult list =
                steps |> List.map Skipped

            let anyFailed (pipelineResults: StepResult list) : bool =
                pipelineResults
                |> List.tryFind
                    (function
                    | Failed _ -> true
                    | _ -> false)
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
                let line = "----------------------------------------------"

                Console.Info |> Console.message line |> writer.WriteLine

                pipelineResults
                |> List.iter (fun x ->
                    match x with
                    | Success (_, stat) ->
                        Console.Info
                        |> Console.messageColor Console.successColor (sprintf "%-35s" stat.StepName)
                        |> Console.append (sprintf ": %s" (stat.ExecutionTime |> formatTime))
                        |> writer.WriteLine
                    | Failed (_, stat, _) ->
                        Console.Info
                        |> Console.messageColor Console.errorColor (sprintf "%-25s" stat.StepName)
                        |> Console.append (sprintf " (failed) : %s" (stat.ExecutionTime |> formatTime))
                        |> writer.WriteLine
                    | Skipped step ->
                        Console.Info
                        |> Console.messageColor Console.warnColor (sprintf "%-24s" step.Name)
                        |> Console.append " (skipped) : 00:00:000"
                        |> writer.WriteLine
                )

                Console.Info |> Console.message line |> writer.WriteLine

                Console.Info
                |> Console.message (sprintf "%-35s: %s" "Total" (totalTime |> formatTime))
                |> writer.WriteLine

        type RunStepArgs =
            { RunNextStage: StepResult list -> Stage list -> StepResult list
              Context: StepContext
              AccResults: StepResult list
              RemainingStages: Stage list }

        let runStep (args: RunStepArgs) (step: Step) : StepResult list =
            let stepContext =
                { args.Context with
                      StepName = step.Name }

            let stepResult = step |> Step.run stepContext

            match stepResult with
            | Ok stat ->
                args.RemainingStages
                |> args.RunNextStage (args.AccResults @ [ Success (step, stat) ])
            | Error (stat, err) ->
                err |> StepError.toConsoleMessage |> args.Context.Console.WriteLines

                args.AccResults @ [ Failed (step, stat, err) ]

        let runParallelSteps (args: RunStepArgs) (steps: Step list) : StepResult list =
            let results =
                steps
                |> Array.ofList
                |> Array.Parallel.map (fun step ->
                    let stepContext =
                        { args.Context with
                              StepName = step.Name
                              IsParallel = true }

                    let stepResult = step |> Step.run stepContext

                    (step, stepResult)
                )
                |> Array.map
                    (function
                    | (step, Ok stat) -> Success (step, stat)
                    | (step, Error (stat, err)) ->
                        let prefix = sprintf "%-*s | " args.Context.LongestStepNameLength step.Name

                        err
                        |> StepError.toConsoleMessage
                        |> List.map (Console.prefix (Console.Colorized (prefix, Console.errorColor)))
                        |> args.Context.Console.WriteLines

                        Failed (step, stat, err))
                |> List.ofArray

            if results |> StepResult.anyFailed then
                args.AccResults @ results
            else
                args.RemainingStages |> args.RunNextStage (args.AccResults @ results)

        let runStages (writer: Console.IWriter) (context: StepContext) (stages: Stage list) : StepResult list =
            let rec nextStage accResults remStages =
                match remStages with
                | [] -> accResults
                | x :: xs ->
                    let stepArgs =
                        { RunNextStage = nextStage
                          Context = context
                          AccResults = accResults
                          RemainingStages = xs }

                    match x with
                    | SequentialStage step ->
                        Console.info "Running " |> Console.appendToken step.Name |> writer.WriteLine

                        step |> runStep stepArgs
                    | ParallelStage steps ->
                        Console.info "Running "
                        |> Console.appendToken (steps |> Step.Internal.concatNames)
                        |> Console.append " in parallel"
                        |> writer.WriteLine

                        steps |> runParallelSteps stepArgs
                    | SequentialMaybeStage (step, cond) ->
                        if cond then
                            Console.info "Running "
                            |> Console.appendToken step.Name
                            |> Console.append ", condition passed"
                            |> writer.WriteLine

                            step |> runStep stepArgs
                        else
                            Console.warn "Skipping step "
                            |> Console.appendToken step.Name
                            |> Console.append ", condition not met"
                            |> writer.WriteLine

                            xs |> nextStage (accResults @ [ Skipped step ])
                    | ParallelMaybeStage (steps, cond) ->
                        let stepNames = steps |> Step.Internal.concatNames

                        if cond then
                            Console.info "Running "
                            |> Console.appendToken (stepNames)
                            |> Console.append " in parallel"
                            |> writer.WriteLine

                            steps |> runParallelSteps stepArgs
                        else
                            Console.warn "Skipping step(s) "
                            |> Console.appendToken stepNames
                            |> Console.append ", condition not met"
                            |> writer.WriteLine

                            let skipResults = steps |> StepResult.createSkippedList

                            xs |> nextStage (accResults @ skipResults)
                    | ParallelMaybesStage (pmaybes) ->
                        let (toRun, toSkip) = pmaybes |> ParallelMaybe.Internal.partionedSteps

                        if toSkip.Length > 0 then
                            Console.warn "Skipping step(s) "
                            |> Console.appendToken (toSkip |> Step.Internal.concatNames)
                            |> Console.append ", condition not met"
                            |> writer.WriteLine

                        let skipResults = toSkip |> StepResult.createSkippedList

                        [ ParallelStage toRun ] @ xs |> nextStage (accResults @ skipResults)

            stages |> nextStage []

    type Builder(name: string, ?from: Pipeline) =
        let mutable pipeline =
            match from with
            | Some x -> { x with Name = name }
            | None -> { Name = name; Stages = [] }

        member _.Delay(f: unit -> 'T) : 'T =
            f ()

        member _.For(_: unit, x: unit -> unit) : unit =
            x ()

        member _.Yield(_: unit) : unit =
            ()

        member _.Yield(pmaybes: ParallelMaybe list) : unit =
            pipeline <-
                { pipeline with
                      Stages = pipeline.Stages @ [ ParallelMaybesStage pmaybes ] }

        member _.Run(_: unit) =
            pipeline

        member _.Combine(_: 'T, _: 'T) : unit =
            ()

        [<CustomOperation("run")>]
        member _.RunStep(_: unit, step: Step) : unit =
            pipeline <-
                { pipeline with
                      Stages = pipeline.Stages @ [ SequentialStage step ] }

        [<CustomOperation("maybe_run")>]
        member _.MaybeRun(_: unit, step: Step, cond: bool) : unit =
            pipeline <-
                { pipeline with
                      Stages = pipeline.Stages @ [ SequentialMaybeStage (step, cond) ] }

        [<CustomOperation("run_parallel")>]
        member _.RunParallel(_: unit, steps: Step list) : unit =
            pipeline <-
                { pipeline with
                      Stages = pipeline.Stages @ [ ParallelStage steps ] }

        [<CustomOperation("maybe_run_parallel")>]
        member _.MaybeRunParallel(_: unit, steps: Step list, cond: bool) : unit =
            pipeline <-
                { pipeline with
                      Stages = pipeline.Stages @ [ ParallelMaybeStage (steps, cond) ] }


    let create (name: string) : Builder =
        Builder (name)

    let createFrom (pipeline: Pipeline) (name: string) : Builder =
        Builder (name, pipeline)

    let run (writer: Console.IWriter) (extraArgs: string list) (ct: CancellationToken) (pipeline: Pipeline) : bool =
        let longestNameLength = pipeline.Stages |> Stage.longestStepNameLength
        use procMonitor = ProcessMonitor.create (writer)

        use __ = ct.Register (fun () -> procMonitor |> ProcessMonitor.killAll)

        let context : StepContext =
            { PipelineName = pipeline.Name
              StepName = ""
              IsParallel = false
              Console = writer
              ProcessMonitor = procMonitor
              LongestStepNameLength = longestNameLength
              ExtraArgs = extraArgs }

        Console.info "Running pipeline "
        |> Console.appendToken pipeline.Name
        |> writer.WriteLine

        let results = pipeline.Stages |> runStages writer context

        writer.WriteLine (Console.Info)
        results |> StepResult.printResults writer

        writer.WriteLine (Console.Info)

        let failed = results |> StepResult.anyFailed

        if failed then
            Console.Info
            |> Console.statusMessage Console.errorColor ""
            |> Console.appendToken pipeline.Name
            |> Console.append " pipeline failed"
            |> writer.WriteLine
        else
            Console.Info
            |> Console.statusMessage Console.successColor ""
            |> Console.appendToken pipeline.Name
            |> Console.append " pipeline complete"
            |> writer.WriteLine

        not failed

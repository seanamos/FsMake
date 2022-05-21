namespace FsMake

open System

/// <summary>
/// Stages used in a pipeline.
/// </summary>
type Stage =
    /// <summary>
    /// A sequential stage that runs a single step.
    /// </summary>
    /// <param name="step">The step to run.</param>
    | SequentialStage of step: Step
    /// <summary>
    /// A stage that runs all steps in parallel.
    /// </summary>
    /// <param name="steps">The steps to run.</param>
    | ParallelStage of steps: Step list
    /// <summary>
    /// A sequential stage that runs a single step if the condition is <c>true</c>.
    /// </summary>
    /// <param name="step">The steps to run.</param>
    /// <param name="condition">The condition that must be <c>true</c> for the step to run.</param>
    | SequentialMaybeStage of step: Step * condition: bool
    /// <summary>
    /// A parallel stage that runs all steps if the condition is <c>true</c>.
    /// </summary>
    /// <param name="steps">The steps to run.</param>
    /// <param name="condition">The condition that must be <c>true</c> for the step to run.</param>
    | ParallelMaybeStage of steps: Step list * condition: bool
    /// <summary>
    /// A parallel stage that runs steps in parallel if their individual conditions are <c>true</c>.
    /// </summary>
    /// <param name="steps">The steps to run.</param>
    | ParallelMaybesStage of steps: ParallelMaybe list

module internal Stage =
    let longestStepNameLength (stages: Stage list) : int =
        let rec nextStage longest rem =
            match rem with
            | [] -> longest
            | x :: xs ->
                match x with
                | SequentialStage step -> xs |> nextStage (Math.Max (step.Name.Length, longest))
                | ParallelStage steps ->
                    let longestStep = steps |> Seq.map (fun x -> x.Name.Length) |> Seq.max

                    xs |> nextStage (Math.Max (longestStep, longest))
                | SequentialMaybeStage (step, _) -> xs |> nextStage (Math.Max (step.Name.Length, longest))
                | ParallelMaybeStage (steps, _) ->
                    let longestStep = steps |> Seq.map (fun x -> x.Name.Length) |> Seq.max

                    xs |> nextStage (Math.Max (longestStep, longest))
                | ParallelMaybesStage steps ->
                    let steps = steps |> ParallelMaybe.Internal.toSteps
                    let longestStep = steps |> Seq.map (fun x -> x.Name.Length) |> Seq.max

                    xs |> nextStage (Math.Max (longestStep, longest))

        stages |> nextStage 0

    let private printRunning (steps: string) (trailer: string option) (console: Console.IWriter) : unit =
        let lineVariableLength =
            match trailer with
            | Some x -> steps.Length + x.Length
            | None -> steps.Length

        let line = "═════════" + String ('═', lineVariableLength) + "═"

        Console.Info
        |> Console.message Environment.NewLine
        |> Console.append ("╔" + line + "╗" + Environment.NewLine)
        |> Console.append "║ Running "
        |> Console.appendToken steps
        |> fun m ->
            match trailer with
            | Some x -> m |> Console.append x
            | _ -> m
        |> Console.append " ║"
        |> Console.append (Environment.NewLine + "╚" + line + "╝" + Environment.NewLine)
        |> console.WriteLine

    let printHeader (console: Console.IWriter) (stage: Stage) : unit =
        match stage with
        | SequentialStage step -> console |> printRunning step.Name None
        | ParallelStage steps ->
            let stepsText = steps |> Step.Internal.concatNames

            console |> printRunning stepsText (Some " in parallel")
        | SequentialMaybeStage (step, _) ->
            console |> printRunning step.Name (Some ", condition passed")
        | ParallelMaybeStage (steps, _) ->
            let stepsText = steps |> Step.Internal.concatNames

            console |> printRunning stepsText (Some " in parallel, condition passed")
        | ParallelMaybesStage _ -> ()

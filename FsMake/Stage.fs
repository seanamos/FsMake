namespace FsMake

open System

/// <summary>
/// Stages used in a pipeline.
/// </summary>
type Stage =
    /// <summary>
    /// A sequential stage the run a single step.
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
    /// A parallel stage that runs steps if their individual conditions are <c>true</c>.
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

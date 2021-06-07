namespace FsMake

open System

type Stage =
    | SequentialStage of step: Step
    | ParallelStage of steps: Step list
    | SequentialMaybeStage of step: Step * condition: bool
    | ParallelMaybeStage of steps: Step list * condition: bool
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

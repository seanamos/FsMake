namespace FsMake

open System

type Stage =
    | Sequential of step: Step
    | Parallel of steps: Step list
    | SequentialMaybe of step: Step * condition: bool
    | ParallelMaybe of steps: Step list * condition: bool
    | ParallelMaybes of steps: ParallelMaybe list

module internal Stage =
    let longestStepNameLength (stages: Stage list) : int =
        let rec nextStage longest rem =
            match rem with
            | [] -> longest
            | x :: xs ->
                match x with
                | Sequential step -> xs |> nextStage (Math.Max (step.Name.Length, longest))
                | Parallel steps ->
                    let longestStep = steps |> Seq.map (fun x -> x.Name.Length) |> Seq.max
                    xs |> nextStage (Math.Max (longestStep, longest))
                | SequentialMaybe (step, _) -> xs |> nextStage (Math.Max (step.Name.Length, longest))
                | ParallelMaybe (steps, _) ->
                    let longestStep = steps |> Seq.map (fun x -> x.Name.Length) |> Seq.max
                    xs |> nextStage (Math.Max (longestStep, longest))
                | ParallelMaybes steps ->
                    let steps = steps |> ParallelMaybe.toSteps

                    let longestStep = steps |> Seq.map (fun x -> x.Name.Length) |> Seq.max
                    xs |> nextStage (Math.Max (longestStep, longest))

        stages |> nextStage 0

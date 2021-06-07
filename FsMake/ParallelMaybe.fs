namespace FsMake

type ParallelMaybe =
    | PStep of step: Step
    | PMaybe of step: Step * condition: bool

module ParallelMaybe =
    [<AutoOpen>]
    module internal Internal =
        let partition (pmaybes: ParallelMaybe list) : (ParallelMaybe list * ParallelMaybe list) =
            pmaybes
            |> List.partition
                (function
                | PMaybe (_, cond) when cond -> true
                | PMaybe _ -> false
                | PStep _ -> true)

        let toSteps (pmaybes: ParallelMaybe list) : Step list =
            pmaybes
            |> List.map
                (function
                | PMaybe (x, _) -> x
                | PStep x -> x)

        let partionedSteps (pmaybes: ParallelMaybe list) : (Step list * Step list) =
            let (run, skip) = pmaybes |> partition

            (run |> toSteps, skip |> toSteps)

    type Builder() =
        member _.Yield(_) : ParallelMaybe list =
            []

        [<CustomOperation("run")>]
        member _.RunStep(maybes: ParallelMaybe list, step: Step) : ParallelMaybe list =
            maybes @ [ PStep step ]

        [<CustomOperation("maybe_run")>]
        member _.Maybe(maybes: ParallelMaybe list, step: Step, cond: bool) : ParallelMaybe list =
            maybes @ [ PMaybe (step, cond) ]

[<AutoOpen>]
module ParallelMaybeBuilder =
    // fsharplint:disable-next-line
    let run_parallel_maybes = ParallelMaybe.Builder ()

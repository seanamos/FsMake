namespace FsMake

/// <summary>
/// Represents the conditions of a parallel maybe pipeline stage.
/// </summary>
type ParallelMaybe =
    /// <summary>
    /// A parallel <see cref="T:Step" /> with no condition that will always run.
    /// </summary>
    /// <param name="step">The <see cref="T:Step" /> to run.</param>
    | PStep of step: Step
    /// <summary>
    /// A parallel <see cref="T:Step" /> with a condition that will only run when <c>condition</c> is <c>true</c>.
    /// </summary>
    /// <param name="step">The <see cref="T:Step" /> to run.</param>
    /// <param name="condition">The condition that must be <c>true</c> for the step to run.</param>
    | PMaybe of step: Step * condition: bool

module ParallelMaybe =
    [<AutoOpen>]
    module internal Internal =
        let partition (pmaybes: ParallelMaybe list) : (ParallelMaybe list * ParallelMaybe list) =
            pmaybes
            |> List.partition
                (function
                | PMaybe (_, true) -> true
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

    /// <summary>
    /// A <see cref="T:ParallelMaybe" /> <c>list</c> computation expression builder.
    /// </summary>
    [<Sealed>]
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
module ParallelMaybeBuilders =
    // fsharplint:disable
    /// <summary>
    /// Creates a <see cref="T:ParallelMaybe" /> <c>list</c> using a <see cref="T:ParallelMaybe.Builder" /> computation expression.
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// let emptyStep1 = Step.create "empty1" { () }
    /// let emptyStep2 = Step.create "empty2" { () }
    /// let emptyStep3 = Step.create "empty3" { () }
    ///
    /// let condition = true
    ///
    /// let pipeline =
    ///     Pipeline.create "example" {
    ///         run_parallel_maybes {
    ///             run emptyStep1
    ///             maybe_run emptyStep2 condition
    ///             run emptyStep3
    ///         }
    ///     }
    /// </code>
    /// </example>
    let run_parallel_maybes = ParallelMaybe.Builder ()
// fsharplint:enable

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

/// <summary>
/// Module for <see cref="T:ParallelMaybe" /> functions.
/// </summary>
module ParallelMaybe =
    [<AutoOpen>]
    module internal Internal =
        let partition (pmaybes: ParallelMaybe list) : (ParallelMaybe list * ParallelMaybe list) =
            pmaybes
            |> List.partition (
                function
                | PMaybe (_, true) -> true
                | PMaybe _ -> false
                | PStep _ -> true
            )

        let toSteps (pmaybes: ParallelMaybe list) : Step list =
            pmaybes
            |> List.map (
                function
                | PMaybe (x, _) -> x
                | PStep x -> x
            )

        let partionedSteps (pmaybes: ParallelMaybe list) : (Step list * Step list) =
            let (run, skip) = pmaybes |> partition

            (run |> toSteps, skip |> toSteps)

    /// <summary>
    /// A <see cref="T:ParallelMaybe" /> <c>list</c> computation expression builder.
    /// <para>
    /// Used with <see cref="P:FsMake.ParallelMaybeBuilders.run_parallel_maybes" />
    /// </para>
    /// </summary>
    [<Sealed>]
    type Builder() =
        member _.Yield(_) : ParallelMaybe list =
            []

        /// <summary>
        /// Adds a <see cref="T:Step" /> that will always be run.
        /// </summary>
        /// <param name="state">Current state of the computation expression.</param>
        /// <param name="step">The <see cref="T:Step" /> to add.</param>
        /// <returns>Updated state of the computation expression.</returns>
        [<CustomOperation("run")>]
        member _.RunStep(state: ParallelMaybe list, step: Step) : ParallelMaybe list =
            state @ [ PStep step ]

        /// <summary>
        /// Adds a <see cref="T:Step" /> that will be run when <paramref name="cond" /> is true.
        /// </summary>
        /// <param name="state">Current state of the computation expression.</param>
        /// <param name="step">The <see cref="T:Step" /> to add.</param>
        /// <param name="cond">The condition that must be true for the <paramref name="step" /> to run.</param>
        /// <returns>Updated state of the computation expression.</returns>
        [<CustomOperation("run_maybe")>]
        member _.RunMaybe(state: ParallelMaybe list, step: Step, cond: bool) : ParallelMaybe list =
            state @ [ PMaybe (step, cond) ]

/// <summary>
/// Auto-opened module containing functions for using <see cref="T:ParallelMaybe" /> computation expressions.
/// </summary>
[<AutoOpen>]
module ParallelMaybeBuilders =
    // fsharplint:disable
    /// <summary>
    /// Creates a <see cref="T:ParallelMaybe" /> <c>list</c> using a <see cref="T:FsMake.ParallelMaybeModule.Builder" /> computation expression.
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
    ///             run_maybe emptyStep2 condition
    ///             run emptyStep3
    ///         }
    ///     }
    /// </code>
    /// </example>
    let run_parallel_maybes = ParallelMaybe.Builder ()
// fsharplint:enable

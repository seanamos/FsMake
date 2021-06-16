namespace FsMake

open System
open System.Diagnostics

/// <summary>
/// Represents a pipeline step.
/// </summary>
type Step =
    { Name: string
      StepPart: StepPart<unit> }

/// <summary>
/// Module for creating and working with steps.
/// </summary>
module Step =

    /// <summary>
    /// A <see cref="T:Step" /> computation expression builder.
    /// </summary>
    /// <param name="name">Name of the step to build.</param>
    [<Sealed>]
    type Builder(name: string) =
        inherit StepPart.BaseBuilder()

        member _.Run(generator: unit -> StepPart<unit>) : Step =
            let part =
                fun ctx ->
                    let part = generator ()
                    part ctx

            { Name = name; StepPart = part }

    [<AutoOpen>]
    module internal Internal =
        type RunStat =
            { StepName: string
              ExecutionTime: TimeSpan }

        type RunResult = Result<RunStat, RunStat * StepError>

        let concatNames (steps: Step list) : string =
            steps
            |> List.fold (fun state x -> if state.Length = 0 then x.Name else $"{state}, {x.Name}") ""

        let run (context: StepContext) (step: Step) : RunResult =
            let stopwatch = Stopwatch ()

            try
                stopwatch.Start ()
                let result = step.StepPart context
                stopwatch.Stop ()

                let runStat =
                    { StepName = step.Name
                      ExecutionTime = stopwatch.Elapsed }

                match result with
                | Ok _ -> Ok runStat
                | Error x -> Error (runStat, x)
            with ex ->
                stopwatch.Stop ()

                ({ StepName = step.Name
                   ExecutionTime = stopwatch.Elapsed },
                 StepUnhandledEx ex)
                |> Error

    /// <summary>
    /// Creates a step using a <see cref="T:Step.Builder" /> computation expression.
    /// </summary>
    /// <param name="name">The name of the step.</param>
    /// <returns>A <see cref="T:Step.Builder" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let emptyStep =
    ///     Step.create "emptyStep" {
    ///         ()
    ///     }
    /// </code>
    /// </example>
    let create (name: string) : Builder =
        Builder (name)


    /// <summary>
    /// Gets the current <see cref="T:StepContext" />.
    /// </summary>
    /// <returns>The <see cref="T:StepContext" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let myStep =
    ///     Step.create "myStep" {
    ///         let! ctx = Step.context
    ///         printfn "Current pipeline: %s" ctx.PipelineName
    ///     }
    /// </code>
    /// </example>
    let context = StepPart.context

    /// <summary>
    /// Fails the current step with a message.
    /// </summary>
    /// <param name="message">The message to be printed as the failure reason.</param>
    /// <returns>An empty <see cref="T:StepPart" /></returns>
    /// <example>
    /// <code lang="fsharp">
    /// let myStep =
    ///     Step.create "myStep" {
    ///         do! Step.fail "Oh no!"
    ///     }
    /// </code>
    /// </example>
    let fail (message: string) : StepPart<unit> =
        fun _ ->
            let msg = Console.error message
            StepError [ msg ] |> Error

    /// <summary>
    /// Fails the current step with a list of <see cref="T:Console.Message" /> to be printed.
    /// This can be used to create detailed multi-line failures.
    /// </summary>
    /// <param name="messages">The messages to be printed.</param>
    /// <returns>An empty <see cref="T:StepPart" /></returns>
    /// <example>
    /// <code lang="fsharp">
    /// let myStep =
    ///     Step.create "myStep" {
    ///         do!
    ///             [ Console.error "Oh " |> Console.appendToken "no!" ]
    ///             |> Step.failMessages
    ///     }
    /// </code>
    /// </example>
    let failMessages (messages: Console.Message list) : StepPart<unit> =
        fun _ -> StepError messages |> Error

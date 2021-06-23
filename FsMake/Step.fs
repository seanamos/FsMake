namespace FsMake

open System
open System.Diagnostics

/// <summary>
/// Represents a pipeline step.
/// </summary>
type Step = { Name: string; Make: Make<unit> }

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
        inherit Make.BaseBuilder()

        member _.Run(generator: unit -> Make<unit>) : Step =
            let make =
                fun ctx ->
                    let innerMake = generator ()
                    innerMake ctx

            { Name = name; Make = make }

    [<AutoOpen>]
    module internal Internal =
        type RunStat =
            {
                StepName: string
                ExecutionTime: TimeSpan
            }

        type RunResult = Result<RunStat, RunStat * MakeError>

        let concatNames (steps: Step list) : string =
            steps
            |> List.foldi
                (fun idx state x ->
                    if idx = 0 then x.Name
                    else if idx + 1 = steps.Length then $"{state} and {x.Name}"
                    else $"{state}, {x.Name}"
                )
                ""

        let run (context: MakeContext) (step: Step) : RunResult =
            let stopwatch = Stopwatch ()

            try
                stopwatch.Start ()
                let result = step.Make context
                stopwatch.Stop ()

                let runStat =
                    {
                        StepName = step.Name
                        ExecutionTime = stopwatch.Elapsed
                    }

                match result with
                | Ok _ -> Ok runStat
                | Error x -> Error (runStat, x)
            with
            | ex ->
                stopwatch.Stop ()

                let runStat =
                    {
                        StepName = step.Name
                        ExecutionTime = stopwatch.Elapsed
                    }

                (runStat, MakeUnhandledEx ex) |> Error

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
    /// Gets the current <see cref="T:Make" />.
    /// </summary>
    /// <returns>The <see cref="T:Make" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let myStep =
    ///     Step.create "myStep" {
    ///         let! ctx = Step.context
    ///         printfn "Current pipeline: %s" ctx.PipelineName
    ///     }
    /// </code>
    /// </example>
    let context = Make.context

    /// <summary>
    /// Fails the current step with a message.
    /// </summary>
    /// <param name="message">The message to be printed as the failure reason.</param>
    /// <returns>An <c>Error</c> <see cref="T:Make" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let myStep =
    ///     Step.create "myStep" {
    ///         do! Step.fail "Oh no!"
    ///     }
    /// </code>
    /// </example>
    let fail (message: string) : Make<unit> =
        Make.fail message

    /// <summary>
    /// Fails the current step with a list of <see cref="T:Console.Message" /> to be printed.
    /// This can be used to create detailed multi-line failures.
    /// </summary>
    /// <param name="messages">The messages to be printed.</param>
    /// <returns>An <c>Error</c> <see cref="T:Make" />.</returns>
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
    let failMessages (messages: Console.Message list) : Make<unit> =
        Make.failMessages messages

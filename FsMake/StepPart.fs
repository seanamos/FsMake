namespace FsMake

open System

/// <summary>
/// The context passed to each running step.
/// </summary>
type StepContext =
    {
        PipelineName: string
        StepName: string
        IsParallel: bool
        Console: Console.IWriter
        Prefix: Console.TextPart
        PrefixOption: Prefix.PrefixOption
        ProcessMonitor: ProcessMonitor.Agent
        ExtraArgs: string list
    }

/// <summary>
/// The types of errors that can occur while running a step.
/// </summary>
type StepError =
    /// <summary>
    /// The step was aborted, most likely by the <see cref="T:System.Threading.CancellationToken" />
    /// in the <see cref="T:StepContext" /> being cancelled.
    /// </summary>
    /// <param name="message">A list of message that should be printed to the console.</param>
    | StepAbort of message: Console.Message list
    /// <summary>
    /// Any error that is not an exception.
    /// </summary>
    /// <param name="message">A list of message that should be printed to the console.</param>
    | StepError of message: Console.Message list
    /// <summary>
    /// An unhandled exception occurred while running the step.
    /// </summary>
    | StepUnhandledEx of ex: exn


module internal StepError =
    let toConsoleMessage (error: StepError) : Console.Message list =
        match error with
        | StepAbort x -> x
        | StepError x -> x
        | StepUnhandledEx ex -> ex |> Exception.toConsoleMessage

/// <summary>
/// A <see cref="T:StepPart" /> represents a function that takes a <see cref="T:StepContext" /> and returns a <see cref="T:FSharp.Core.Result" />.
/// <para>
/// It allows various "parts" that make up the execution of a step to be "glued" together and thread a <see cref="T:StepContext" /> through.
/// </para>
/// <para>
/// This could possibly be renamed. ie. StepElement, StepSegment etc.
/// </para>
/// </summary>
type StepPart<'T> = StepContext -> Result<'T, StepError>

/// <summary>
/// Module for working with a <see cref="T:StepPart" />
/// </summary>
module StepPart =

    /// <summary>
    /// Takes a mapping function to create a new <see cref="T:StepPart" /> from the result of an existing <see cref="T:StepPart" />.
    /// <para>
    /// This is typically used in computation expressions automatically, but can be used manually.
    /// </para>
    /// </summary>
    /// <param name="mapping">The mapping function.</param>
    /// <param name="part">The existing <see cref="T:StepPart" />.</param>
    /// <param name="ctx">The current <see cref="T:StepContext" />.</param>
    /// <typeparam name="'T">The type returned from the existing <see cref="T:StepPart" />.</typeparam>
    /// <typeparam name="'U">The type returned from the mapping function.</typeparam>
    /// <returns>A new <see cref="T:StepPart" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let boolPart : StepPart&lt;bool&gt; =
    ///     let stringPart : StepPart&lt;string&gt; =
    ///         fun (ctx: StepContext) -> Ok ctx.StepName
    ///
    ///     stringPart |> StepPart.map (fun stepName -> stepName = "build")
    /// </code>
    /// </example>
    let map (mapping: 'T -> 'U) (part: StepPart<'T>) : StepPart<'U> =
        fun ctx ->
            let result = part ctx

            match result with
            | Ok x -> mapping x |> Ok
            | Error x -> Error x

    /// <summary>
    /// Takes a binder function to create a new <see cref="T:StepPart" /> from the result of an existing <see cref="T:StepPart" />.
    /// <para>
    /// This is typically used in computation expressions automatically, but can be used manually.
    /// </para>
    /// </summary>
    /// <param name="binder">The binder function.</param>
    /// <param name="part">The existing <see cref="T:StepPart" />.</param>
    /// <param name="ctx">The current <see cref="T:StepContext" />.</param>
    /// <typeparam name="'T">The type returned from the existing <see cref="T:StepPart" />.</typeparam>
    /// <typeparam name="'U">The type returned from the binder function.</typeparam>
    /// <returns>A new <see cref="T:StepPart" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let newPart : StepPart&lt;unit&gt; =
    ///     let examplePart : StepPart&lt;string list&gt; =
    ///         fun (ctx: StepContext) -> Ok ctx.ExtraArgs
    ///
    ///     examplePart |> StepPart.bind (fun args -> Cmd.createWithArgs "dotnet" args |> Cmd.run)
    /// </code>
    /// </example>
    let bind (binder: 'T -> StepPart<'U>) (part: StepPart<'T>) : StepPart<'U> =
        fun ctx ->
            let result = part ctx

            match result with
            | Ok x ->
                let bound = binder x
                bound ctx
            | Error x -> Error x


    /// <summary>
    /// Takes two <see cref="T:StepPart" /> and creates a tuple <see cref="T:StepPart" /> of their results.
    /// <para>
    /// This is typically used in computation expressions automatically, but can be used manually.
    /// </para>
    /// </summary>
    /// <param name="part1">The first part.</param>
    /// <param name="part2">The second part.</param>
    /// <param name="ctx">The current <see cref="T:StepContext" />.</param>
    /// <typeparam name="'T">The type returned from the first <see cref="T:StepPart" />.</typeparam>
    /// <typeparam name="'T1">The type returned from the second <see cref="T:StepPart" />.</typeparam>
    /// <returns>The tupled result.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let part1 = StepPart.zero
    /// let part2 = StepPart.zero
    ///
    /// let examplePart : StepPart&lt;unit * unit&gt; = StepPart.zip part1 part2
    /// </code>
    /// </example>
    let zip (part1: StepPart<'T>) (part2: StepPart<'T1>) : StepPart<'T * 'T1> =
        fun ctx ->
            match (part1 ctx, part2 ctx) with
            | Ok x1res, Ok x2res -> Ok (x1res, x2res)
            | Error e, _ -> Error e
            | _, Error e -> Error e

    /// <summary>
    /// Creates an empty <see cref="T:StepPart" />.
    /// <para>
    /// This is typically used in computation expressions automatically, but can be used manually.
    /// </para>
    /// </summary>
    /// <returns>An empty <see cref="T:StepPart" />.</returns>
    /// <example>
    /// <code>
    /// let emptyPart : StepPart&lt;unit&gt; = StepPart.zero
    /// </code>
    /// </example>
    let zero : StepPart<unit> = fun _ -> Ok ()

    /// <summary>
    /// Wraps a value in a <see cref="T:StepPart" />.
    /// <para>
    /// This is typically used in computation expressions automatically, but can be used manually.
    /// </para>
    /// </summary>
    /// <param name="value">The value to be wrapped.</param>
    /// <typeparam name="'T">The type of the value to be wrapped.</typeparam>
    /// <returns>The wrapped value.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let wrappedValue : StepPart&lt;int&gt; = StepPart.return' 5
    /// </code>
    /// </example>
    let return' (value: 'T) : StepPart<'T> =
        fun _ -> Ok value

    /// <summary>
    /// Gets a <see cref="T:StepPart" /> that can be used to retrieve the current <see cref="T:StepContext" />.
    /// <para>
    /// This is typically used in computation expressions.
    /// </para>
    /// </summary>
    /// <returns>The part.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let part =
    ///     stepPart {
    ///         let! ctx = StepPart.context
    ///         printfn "%s" ctx.PipelineName
    ///     }
    /// </code>
    /// </example>
    let context : StepPart<StepContext> = Ok

    /// <summary>
    /// Base class for <see cref="T:StepPart" /> computation expression builders.
    /// </summary>
    [<AbstractClass>]
    type BaseBuilder() =
        member _.Return(value: 'T) : StepPart<'T> =
            return' value

        member inline _.ReturnFrom(part: StepPart<'T>) : StepPart<'T> =
            part

        member _.Zero() : StepPart<unit> =
            zero

        member inline _.Bind(part: StepPart<'T>, binder: 'T -> StepPart<'U>) : StepPart<'U> =
            bind binder part

        member _.Delay(generator: unit -> StepPart<'T>) : unit -> StepPart<'T> =
            generator

        member this.Combine(part: StepPart<unit>, binder: unit -> StepPart<'T>) : StepPart<'T> =
            this.Bind (part, binder)

        member _.TryWith(generator: unit -> StepPart<'T>, handler: exn -> StepPart<'T>) : StepPart<'T> =
            try
                generator ()
            with
            | ex -> handler ex

        member _.TryFinally(generator: unit -> StepPart<'T>, compensation: unit -> unit) : StepPart<'T> =
            try
                generator ()
            finally
                compensation ()

        member this.Using(resource: 'T :> IDisposable, binder: 'T -> StepPart<'U>) : StepPart<'U> =
            this.TryFinally ((fun () -> binder resource), (fun () -> if not <| isNull resource then resource.Dispose ()))

        member this.While(guard: unit -> bool, generator: unit -> StepPart<unit>) : StepPart<unit> =
            if not <| guard () then
                this.Zero ()
            else
                this.Bind (generator (), (fun () -> this.While (guard, generator)))

        member this.For(sequence: #seq<'T>, binder: 'T -> StepPart<unit>) : StepPart<unit> =
            this.Using (sequence.GetEnumerator (), (fun enum -> this.While (enum.MoveNext, this.Delay (fun () -> binder enum.Current))))

        member _.BindReturn(part: StepPart<'T>, f: 'T -> 'U) : StepPart<'U> =
            map f part

        member _.MergeSources(source1: StepPart<'T>, source2: StepPart<'T1>) : StepPart<('T * 'T1)> =
            zip source1 source2

    /// <summary>
    /// A <see cref="T:StepPart" /> computation expression builder.
    /// </summary>
    [<Sealed>]
    type Builder() =
        inherit BaseBuilder()

        member inline _.Run(generator: unit -> StepPart<'T>) : StepPart<'T> =
            fun ctx ->
                let part = generator ()
                part ctx

    /// <summary>
    /// Creates a new <see cref="T:StepPart" /> from an existing one, that adds retry behaviour.
    /// This will "catch" all errors except for <see cref="T:StepError.StepAbort" />.
    /// </summary>
    /// <param name="attempts">The amount of attempts in total to try.</param>
    /// <param name="part">The part to add retries to.</param>
    /// <typeparam name="'T">The return type.</typeparam>
    /// <returns>The new <see cref="T:StepPart" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let cmdWithRetry =
    ///     Cmd.createWithArgs "dotnet" [ "test" ]
    ///     |> Cmd.run
    ///     |> StepPart.retry 2
    /// </code>
    /// </example>
    let retry (attempts: int) (part: StepPart<'T>) : StepPart<'T> =
        let rec nextRetry attempted ctx =
            let prefixArgs : Prefix.Internal.OptionalPrefixArgs =
                {
                    IsParallel = ctx.IsParallel
                    PrefixOption = ctx.PrefixOption
                    Prefix = ctx.Prefix
                }

            let errorMessage (messages: Console.Message list) =
                messages
                |> Prefix.Internal.addOptionalPrefixes prefixArgs
                |> ctx.Console.WriteLines

            let retryMessage () =
                Console.warn "Retrying, attempt "
                |> Console.appendToken ((attempted + 1).ToString ())
                |> Prefix.Internal.addOptionalPrefix prefixArgs
                |> ctx.Console.WriteLine

            try
                let result = part ctx

                match result with
                | Ok _ as x -> x
                | Error (StepAbort _) as x -> x
                | Error x ->
                    if attempted < attempts then
                        x |> StepError.toConsoleMessage |> errorMessage
                        retryMessage ()

                        ctx |> nextRetry (attempted + 1)
                    else
                        Error x
            with
            | ex ->
                if attempted < attempts then
                    ex |> Exception.toConsoleMessage |> errorMessage
                    retryMessage ()

                    ctx |> nextRetry (attempted + 1)
                else
                    StepUnhandledEx ex |> Error

        fun ctx -> ctx |> nextRetry 1

    /// <summary>
    /// A computation expression builder for creating a retryable <see cref="T:StepPart" />.
    /// </summary>
    [<Sealed>]
    type RetryBuilder(attempts: int) =
        inherit BaseBuilder()

        member _.Run(generator: unit -> StepPart<'T>) : StepPart<'T> =
            let part =
                fun ctx ->
                    let innerPart = generator ()
                    innerPart ctx

            part |> retry attempts


    /// <summary>
    /// Memoizes a <see cref="T:StepPart" /> so it is only executed once. Subsequent executions return the result immediately.
    /// <para>
    /// This only allows single access to the part, so if it is run in parallel, only one will run and the rest will block until a result is available.
    /// </para>
    /// </summary>
    /// <param name="part">The part to memoize.</param>
    /// <typeparam name="'T">The return type of the part.</typeparam>
    /// <returns>A new memoized part.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let versionPart =
    ///     Cmd.createWithArgs "dotnet" [ "gitversion" ]
    ///     |> Cmd.run
    ///     |> StepPart.memo
    /// </code>
    /// </example>
    let memo (part: StepPart<'T>) : StepPart<'T> =
        let locker = obj ()
        let mutable memoized = None

        fun ctx ->
            lock locker
            <| fun () ->
                match memoized with
                | Some x -> x
                | None ->
                    let result = part ctx
                    memoized <- result |> Some
                    result

    /// <summary>
    /// A computation expression builder for creating a memoized <see cref="T:StepPart" />.
    /// </summary>
    [<Sealed>]
    type MemoBuilder() =
        inherit BaseBuilder()

        member inline _.Run(generator: unit -> StepPart<'T>) : StepPart<'T> =
            let part = fun ctx -> ctx |> generator ()
            memo part

    /// <summary>
    /// Memoizes a <see cref="T:StepPart" /> so it is only executed once. Subsequent executions return the result immediately.
    /// Unlike <see cref="M:StepPart.memo" />, this allows parallel executions to occur.
    /// <para>
    /// If it is run in parallel, it will be run multiple times until a result has been stored. Once a result has been stored, subsequent
    /// runs will immediately return the result.
    /// </para>
    /// </summary>
    /// <param name="part">The part to memoize.</param>
    /// <typeparam name="'T">The return type of the part.</typeparam>
    /// <returns>A new memoized part.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let versionPart =
    ///     Cmd.createWithArgs "dotnet" [ "gitversion" ]
    ///     |> Cmd.result
    ///     |> StepPart.memoRace
    /// </code>
    /// </example>
    let memoRace (part: StepPart<'T>) : StepPart<'T> =
        let mutable memoized = None

        fun ctx ->
            match memoized with
            | Some x -> x
            | None ->
                let result = part ctx
                memoized <- result |> Some
                result

    /// <summary>
    /// A computation expression builder for creating a memoized <see cref="T:StepPart" /> with parallel access.
    /// </summary>
    [<Sealed>]
    type MemoRaceBuilder() =
        inherit BaseBuilder()

        member inline _.Run(generator: unit -> StepPart<'T>) : StepPart<'T> =
            let part = fun ctx -> ctx |> generator ()
            memoRace part

[<AutoOpen>]
module StepPartBuilders =
    /// <summary>
    /// Creates a <see cref="T:StepPart" /> using a <see cref="T:StepPart.Builder" /> computation expression.
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// let newPart = stepPart {
    ///     let ctx = StepPart.context
    ///     printfn "%s" ctx.StepName
    /// }
    /// </code>
    /// </example>
    let stepPart = StepPart.Builder ()

    /// <summary>
    /// Creates a retry <see cref="T:StepPart" /> using a <see cref="T:StepPart.Builder" /> computation expression.
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// let test =
    ///     Step.create "test" {
    ///         do! retry 2 {
    ///             do! Cmd.createWithArgs "dotnet" [ "test" ] |> Cmd.run
    ///         }
    ///     }
    /// </code>
    /// </example>
    let retry (attempts: int) : StepPart.RetryBuilder =
        StepPart.RetryBuilder (attempts)

    /// <summary>
    /// Creates a single access memo <see cref="T:StepPart" /> using a <see cref="T:StepPart.Builder" /> computation expression.
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// let versionPart =
    ///     memo {
    ///         return! Cmd.createWithArgs "dotnet" [ "gitversion" ] |> Cmd.result
    ///     }
    /// </code>
    /// </example>
    let memo = StepPart.MemoBuilder ()

    /// <summary>
    /// Creates a parallel access memo <see cref="T:StepPart" /> using a <see cref="T:StepPart.Builder" /> computation expression.
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// let versionPart =
    ///     memoRace {
    ///         return! Cmd.createWithArgs "dotnet" [ "gitversion" ] |> Cmd.result
    ///     }
    /// </code>
    /// </example>
    let memoRace = StepPart.MemoRaceBuilder ()

namespace FsMake

open System

/// <summary>
/// The context passed to each <see cref="T:Make`1" />.
/// </summary>
type MakeContext =
    {

        /// <summary>
        /// Gets the name of the currently executing <see cref="T:Pipeline" />.
        /// </summary>
        PipelineName: string

        /// <summary>
        /// Gets the name of the currently execting <see cref="T:Step" />.
        /// </summary>
        StepName: string

        /// <summary>
        /// Gets a <c>bool</c> indicating if the <see cref="T:Step" /> is running in parallel.
        /// </summary>
        IsParallel: bool

        /// <summary>
        /// Gets the current <see cref="T:Console.IWriter" /> that can be used to write to the console.
        /// </summary>
        Console: Console.IWriter

        /// <summary>
        /// Gets the console prefix for the current <see cref="T:Step" />.
        /// </summary>
        Prefix: Console.TextPart

        /// <summary>
        /// Gets the globally configured <see cref="T:Prefix.PrefixOption" />.
        /// </summary>
        PrefixOption: Prefix.PrefixOption

        /// <summary>
        /// Gets the <see cref="T:ProcessMonitor.Agent" /> for the currently running <see cref="T:Pipeline" />.
        /// </summary>
        ProcessMonitor: ProcessMonitor.Agent

        /// <summary>
        /// Gets the extra arguments passed to the runner.
        /// <para>These are arguments that were passed after "--". eg. <c>dotnet fsi build.fsx build -- --clean</c></para>
        /// </summary>
        ExtraArgs: string list
    }

/// <summary>
/// The types of errors that can occur while running a step.
/// </summary>
type MakeError =
    /// <summary>
    /// The make was aborted, most likely by the <see cref="T:System.Threading.CancellationToken" />
    /// in the <see cref="T:MakeContext" /> being cancelled.
    /// </summary>
    /// <param name="message">A list of message that should be printed to the console.</param>
    | MakeAbort of message: Console.Message list
    /// <summary>
    /// Any error that is not an exception.
    /// </summary>
    /// <param name="message">A list of message that should be printed to the console.</param>
    | MakeError of message: Console.Message list
    /// <summary>
    /// An unhandled exception occurred while running the <see cref="T:Make`1" />.
    /// </summary>
    | MakeUnhandledEx of ex: exn


module internal MakeError =
    let toConsoleMessage (error: MakeError) : Console.Message list =
        match error with
        | MakeAbort x -> x
        | MakeError x -> x
        | MakeUnhandledEx ex -> ex |> Exception.toConsoleMessage

/// <summary>
/// A <see cref="T:Make`1" /> represents a function that takes a <see cref="T:MakeContext" /> and returns a <see cref="T:FSharp.Core.FSharpResult`2" />.
/// <para>
/// It allows various "pieces" that make up the execution of a step to be "glued" together and thread a <see cref="T:MakeContext" /> through.
/// </para>
/// </summary>
type Make<'T> = MakeContext -> Result<'T, MakeError>


/// <summary>
/// Module for working with a <see cref="T:Make`1" />
/// </summary>
///
/// <namespacedoc>
///   <summary>Contains core functionality.</summary>
/// </namespacedoc>
module Make =

    /// <summary>
    /// Takes a mapping function to create a new <see cref="T:Make`1" /> from the result of an existing <see cref="T:Make`1" />.
    /// <para>
    /// This is typically used in computation expressions automatically, but can be used manually.
    /// </para>
    /// </summary>
    /// <param name="mapping">The mapping function.</param>
    /// <param name="make">The existing <see cref="T:Make`1" />.</param>
    /// <param name="ctx">The current <see cref="T:MakeContext" />.</param>
    /// <typeparam name="'T">The type returned from the existing <see cref="T:Make`1" />.</typeparam>
    /// <typeparam name="'U">The type returned from the mapping function.</typeparam>
    /// <returns>A new <see cref="T:Make`1" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let boolMake : Make&lt;bool&gt; =
    ///     let stringMake : Make&lt;string&gt; =
    ///         fun (ctx: MakeContext) -> Ok ctx.StepName
    ///
    ///     stringMake |> Make.map (fun stepName -> stepName = "build")
    /// </code>
    /// </example>
    let map (mapping: 'T -> 'U) (make: Make<'T>) : Make<'U> =
        fun ctx ->
            let result = make ctx

            match result with
            | Ok x -> mapping x |> Ok
            | Error x -> Error x

    /// <summary>
    /// Takes a binder function to create a new <see cref="T:Make`1" /> from the result of an existing <see cref="T:Make`1" />.
    /// <para>
    /// This is typically used in computation expressions automatically, but can be used manually.
    /// </para>
    /// </summary>
    /// <param name="binder">The binder function.</param>
    /// <param name="make">The existing <see cref="T:Make`1" />.</param>
    /// <param name="ctx">The current <see cref="T:MakeContext" />.</param>
    /// <typeparam name="'T">The type returned from the existing <see cref="T:Make`1" />.</typeparam>
    /// <typeparam name="'U">The type returned from the binder function.</typeparam>
    /// <returns>A new <see cref="T:Make`1" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let newMake : Make&lt;unit&gt; =
    ///     let exampleMake : Make&lt;string list&gt; =
    ///         fun (ctx: MakeContext) -> Ok ctx.ExtraArgs
    ///
    ///     exampleMake |> Make.bind (fun args -> Cmd.createWithArgs "dotnet" args |> Cmd.run)
    /// </code>
    /// </example>
    let bind (binder: 'T -> Make<'U>) (make: Make<'T>) : Make<'U> =
        fun ctx ->
            let result = make ctx

            match result with
            | Ok x ->
                let bound = binder x
                bound ctx
            | Error x -> Error x


    /// <summary>
    /// Takes two <see cref="T:Make`1" /> and creates a tuple <see cref="T:Make`1" /> of their results.
    /// <para>
    /// This is typically used in computation expressions automatically, but can be used manually.
    /// </para>
    /// </summary>
    /// <param name="make1">The first make.</param>
    /// <param name="make2">The second make.</param>
    /// <param name="ctx">The current <see cref="T:MakeContext" />.</param>
    /// <typeparam name="'T">The type returned from the first <see cref="T:Make`1" />.</typeparam>
    /// <typeparam name="'T1">The type returned from the second <see cref="T:Make`1" />.</typeparam>
    /// <returns>The tupled result.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let make1 = Make.zero
    /// let make2 = Make.zero
    ///
    /// let exampleMake : Make&lt;unit * unit&gt; = Make.zip make1 make2
    /// </code>
    /// </example>
    let zip (make1: Make<'T>) (make2: Make<'T1>) : Make<'T * 'T1> =
        fun ctx ->
            match (make1 ctx, make2 ctx) with
            | Ok x1res, Ok x2res -> Ok (x1res, x2res)
            | Error e, _ -> Error e
            | _, Error e -> Error e

    /// <summary>
    /// Creates an empty <see cref="T:Make`1" />.
    /// <para>
    /// This is typically used in computation expressions automatically, but can be used manually.
    /// </para>
    /// </summary>
    /// <returns>An empty <see cref="T:Make`1" />.</returns>
    /// <example>
    /// <code>
    /// let emptyMake : Make&lt;unit&gt; = Make.zero
    /// </code>
    /// </example>
    let zero: Make<unit> = fun _ -> Ok ()

    /// <summary>
    /// Wraps a value in a <see cref="T:Make`1" />.
    /// <para>
    /// This is typically used in computation expressions automatically, but can be used manually.
    /// </para>
    /// </summary>
    /// <param name="value">The value to be wrapped.</param>
    /// <typeparam name="'T">The type of the value to be wrapped.</typeparam>
    /// <returns>The wrapped value.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let wrappedValue : Make&lt;int&gt; = Make.return' 5
    /// </code>
    /// </example>
    let return' (value: 'T) : Make<'T> =
        fun _ -> Ok value

    /// <summary>
    /// Gets a <see cref="T:Make`1" /> that can be used to retrieve the current <see cref="T:MakeContext" />.
    /// <para>
    /// This is typically used in computation expressions.
    /// </para>
    /// </summary>
    /// <returns>The <see cref="T:Make`1" /> contains a <see cref="T:MakeContext" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let ctxMake =
    ///     make {
    ///         let! ctx = Make.context
    ///         printfn "%s" ctx.PipelineName
    ///     }
    /// </code>
    /// </example>
    let context: Make<MakeContext> = Ok

    /// <summary>
    /// Fails the current <see cref="T:Make`1" /> with a message.
    /// </summary>
    /// <param name="message">The message to be printed as the failure reason.</param>
    /// <typeparam name="'T">The <see cref="T:Make`1" /> type.</typeparam>
    /// <returns>An <c>Error</c> <see cref="T:Make`1" /></returns>
    /// <example>
    /// <code lang="fsharp">
    /// let errMake =
    ///     make {
    ///         do! Make.fail "Oh no!"
    ///     }
    /// </code>
    /// </example>
    let fail (message: string) : Make<'T> =
        fun _ ->
            let msg = Console.error message
            MakeError [ msg ] |> Error

    /// <summary>
    /// Fails the current <see cref="T:Make`1" /> with a list of <see cref="T:Console.Message" /> to be printed.
    /// This can be used to create detailed multi-line failures.
    /// </summary>
    /// <param name="messages">The messages to be printed.</param>
    /// <typeparam name="'T">The <see cref="T:Make`1" /> type.</typeparam>
    /// <returns>An <c>Error</c> <see cref="T:Make`1" /></returns>
    /// <example>
    /// <code lang="fsharp">
    /// let failMake =
    ///     make {
    ///         do!
    ///             [ Console.error "Oh " |> Console.appendToken "no!" ]
    ///             |> Make.failMessages
    ///     }
    /// </code>
    /// </example>
    let failMessages (messages: Console.Message list) : Make<'T> =
        fun _ -> MakeError messages |> Error

    /// <summary>
    /// Base class for <see cref="T:Make`1" /> computation expression builders.
    /// </summary>
    [<AbstractClass>]
    type BaseBuilder() =
        member _.Return(value: 'T) : Make<'T> =
            return' value

        member inline _.ReturnFrom(make: Make<'T>) : Make<'T> =
            make

        member _.Zero() : Make<unit> =
            zero

        member inline _.Bind(make: Make<'T>, binder: 'T -> Make<'U>) : Make<'U> =
            bind binder make

        member _.Delay(generator: unit -> Make<'T>) : unit -> Make<'T> =
            generator

        member this.Combine(make: Make<unit>, binder: unit -> Make<'T>) : Make<'T> =
            this.Bind (make, binder)

        member _.TryWith(generator: unit -> Make<'T>, handler: exn -> Make<'T>) : Make<'T> =
            try
                generator ()
            with
            | ex -> handler ex

        member _.TryFinally(generator: unit -> Make<'T>, compensation: unit -> unit) : Make<'T> =
            try
                generator ()
            finally
                compensation ()

        member this.Using(resource: 'T :> IDisposable, binder: 'T -> Make<'U>) : Make<'U> =
            this.TryFinally ((fun () -> binder resource), (fun () -> if not <| isNull resource then resource.Dispose ()))

        member this.While(guard: unit -> bool, generator: unit -> Make<unit>) : Make<unit> =
            if not <| guard () then
                this.Zero ()
            else
                this.Bind (generator (), (fun () -> this.While (guard, generator)))

        member this.For(sequence: #seq<'T>, binder: 'T -> Make<unit>) : Make<unit> =
            this.Using (sequence.GetEnumerator (), (fun enum -> this.While (enum.MoveNext, this.Delay (fun () -> binder enum.Current))))

        member _.BindReturn(make: Make<'T>, f: 'T -> 'U) : Make<'U> =
            map f make

        member _.MergeSources(source1: Make<'T>, source2: Make<'T1>) : Make<('T * 'T1)> =
            zip source1 source2

    /// <summary>
    /// A <see cref="T:Make`1" /> computation expression builder.
    /// <para>
    /// This is used with <see cref="P:FsMake.MakeBuilders.make" />.
    /// </para>
    /// </summary>
    [<Sealed>]
    type Builder() =
        inherit BaseBuilder()

        member inline _.Run(generator: unit -> Make<'T>) : Make<'T> =
            fun ctx ->
                let make = generator ()
                make ctx

    /// <summary>
    /// Creates a new <see cref="T:Make`1" /> from an existing one, that adds retry behaviour.
    /// This will "catch" all errors except for <see cref="T:MakeError.MakeAbort" />.
    /// </summary>
    /// <param name="attempts">The amount of attempts in total to try.</param>
    /// <param name="make">The <see cref="T:Make`1" /> to add retries to.</param>
    /// <typeparam name="'T">The return type.</typeparam>
    /// <returns>The new <see cref="T:Make`1" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let cmdWithRetry =
    ///     Cmd.createWithArgs "dotnet" [ "test" ]
    ///     |> Cmd.run
    ///     |> Make.retry 2
    /// </code>
    /// </example>
    let retry (attempts: int) (make: Make<'T>) : Make<'T> =
        let rec nextRetry attempted ctx =
            let prefixArgs: Prefix.Internal.OptionalPrefixArgs =
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
                let result = make ctx

                match result with
                | Ok _ as x -> x
                | Error (MakeAbort _) as x -> x
                | Error x ->
                    if attempted < attempts then
                        x |> MakeError.toConsoleMessage |> errorMessage
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
                    MakeUnhandledEx ex |> Error

        fun ctx -> ctx |> nextRetry 1

    /// <summary>
    /// A computation expression builder for creating a retryable <see cref="T:Make`1" />.
    /// <para>
    /// This is used with <see cref="P:FsMake.MakeBuilders.retry" />.
    /// </para>
    /// </summary>
    [<Sealed>]
    type RetryBuilder(attempts: int) =
        inherit BaseBuilder()

        member _.Run(generator: unit -> Make<'T>) : Make<'T> =
            let make =
                fun ctx ->
                    let innerMake = generator ()
                    innerMake ctx

            make |> retry attempts


    /// <summary>
    /// Memoizes a <see cref="T:Make`1" /> so it is only executed once. Subsequent executions return the result immediately.
    /// <para>
    /// This only allows single access to the <see cref="T:Make`1" />, so if it is run in parallel, only one will run and the rest will block until a result is available.
    /// </para>
    /// </summary>
    /// <param name="make">The <see cref="T:Make`1" /> to memoize.</param>
    /// <typeparam name="'T">The return type of the <see cref="T:Make`1" />.</typeparam>
    /// <returns>A new memoized <see cref="T:Make`1" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let versionMake =
    ///     Cmd.createWithArgs "dotnet" [ "gitversion" ]
    ///     |> Cmd.run
    ///     |> Make.memo
    /// </code>
    /// </example>
    let memo (make: Make<'T>) : Make<'T> =
        let locker = obj ()
        let mutable memoized = None

        fun ctx ->
            lock locker
            <| fun () ->
                match memoized with
                | Some x -> x
                | None ->
                    let result = make ctx
                    memoized <- result |> Some
                    result

    /// <summary>
    /// A computation expression builder for creating a memoized <see cref="T:Make`1" />.
    /// <para>
    /// This is used with <see cref="P:FsMake.MakeBuilders.memo" />.
    /// </para>
    /// </summary>
    [<Sealed>]
    type MemoBuilder() =
        inherit BaseBuilder()

        member inline _.Run(generator: unit -> Make<'T>) : Make<'T> =
            let make = fun ctx -> ctx |> generator ()
            memo make

    /// <summary>
    /// Memoizes a <see cref="T:Make`1" /> so it is only executed once. Subsequent executions return the result immediately.
    /// Unlike <see cref="M:Make.memo" />, this allows parallel executions to occur.
    /// <para>
    /// If it is run in parallel, it will be run multiple times until a result has been stored. Once a result has been stored, subsequent
    /// runs will immediately return the result.
    /// </para>
    /// </summary>
    /// <param name="make">The <see cref="T:Make`1" /> to memoize.</param>
    /// <typeparam name="'T">The return type of the <see cref="T:Make`1" />.</typeparam>
    /// <returns>A new memoized <see cref="T:Make`1" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let versionMake =
    ///     Cmd.createWithArgs "dotnet" [ "gitversion" ]
    ///     |> Cmd.result
    ///     |> Make.memoRace
    /// </code>
    /// </example>
    let memoRace (make: Make<'T>) : Make<'T> =
        let mutable memoized = None

        fun ctx ->
            match memoized with
            | Some x -> x
            | None ->
                let result = make ctx
                memoized <- result |> Some
                result

    /// <summary>
    /// A computation expression builder for creating a memoized <see cref="T:Make`1" /> with parallel access.
    /// <para>
    /// This is used with <see cref="P:FsMake.MakeBuilders.memoRace" />.
    /// </para>
    /// </summary>
    [<Sealed>]
    type MemoRaceBuilder() =
        inherit BaseBuilder()

        member inline _.Run(generator: unit -> Make<'T>) : Make<'T> =
            let make = fun ctx -> ctx |> generator ()
            memoRace make

/// <summary>
/// Auto-opened module containing functions for using <see cref="T:Make`1" /> computation expressions.
/// </summary>
[<AutoOpen>]
module MakeBuilders =
    /// <summary>
    /// Creates a <see cref="T:Make`1" /> using a <see cref="T:Make.Builder" /> computation expression.
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// let newMake =
    ///     make {
    ///         let ctx = Make.context
    ///         printfn "%s" ctx.StepName
    ///     }
    /// </code>
    /// </example>
    let make = Make.Builder ()

    /// <summary>
    /// Creates a retry <see cref="T:Make`1" /> using a <see cref="T:Make.RetryBuilder" /> computation expression.
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
    let retry (attempts: int) : Make.RetryBuilder =
        Make.RetryBuilder (attempts)

    /// <summary>
    /// Creates a single access memo <see cref="T:Make`1" /> using a <see cref="T:Make.MemoBuilder" /> computation expression.
    /// <para>
    /// Once a memo has run, it will return the same value without running again.
    /// </para>
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// let versionMake =
    ///     memo {
    ///         return! Cmd.createWithArgs "dotnet" [ "gitversion" ] |> Cmd.result
    ///     }
    /// </code>
    /// </example>
    let memo = Make.MemoBuilder ()

    /// <summary>
    /// Creates a parallel access memo <see cref="T:Make`1" /> using a <see cref="T:Make.MemoRaceBuilder" /> computation expression.
    /// <para>
    /// Once a memoRace has run, it will return the same value without running again.
    /// </para>
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// let versionMake =
    ///     memoRace {
    ///         return! Cmd.createWithArgs "dotnet" [ "gitversion" ] |> Cmd.result
    ///     }
    /// </code>
    /// </example>
    let memoRace = Make.MemoRaceBuilder ()

namespace FsMake

open System

type StepContext =
    { PipelineName: string
      StepName: string
      IsParallel: bool
      Console: Console.IWriter
      Prefix: Console.TextPart
      PrefixOption: Prefix.PrefixOption
      ProcessMonitor: ProcessMonitor.Agent
      ExtraArgs: string list }

type StepError =
    | StepAbort of message: Console.Message list
    | StepError of message: Console.Message list
    | StepUnhandledEx of ex: exn

module StepError =
    let toConsoleMessage (error: StepError) : Console.Message list =
        match error with
        | StepAbort x -> x
        | StepError x -> x
        | StepUnhandledEx ex -> ex |> Exception.toConsoleMessage

type StepPart<'T> = StepContext -> Result<'T, StepError>

module StepPart =
    let map (mapping: 'T -> 'U) (part: StepPart<'T>) : StepPart<'U> =
        fun ctx ->
            let result = part ctx

            match result with
            | Ok x -> mapping x |> Ok
            | Error x -> Error x

    let bind (binder: 'T -> StepPart<'U>) (part: StepPart<'T>) : StepPart<'U> =
        fun ctx ->
            let result = part ctx

            match result with
            | Ok x ->
                let bound = binder x
                bound ctx
            | Error x -> Error x

    let zip (part1: StepPart<'T>) (part2: StepPart<'T1>) : StepContext -> Result<('T * 'T1), StepError> =
        fun ctx ->
            match (part1 ctx, part2 ctx) with
            | Ok x1res, Ok x2res -> Ok (x1res, x2res)
            | Error e, _ -> Error e
            | _, Error e -> Error e

    let zero : StepPart<unit> = fun _ -> Ok ()

    let return' (value: 'T) : StepPart<'T> =
        fun _ -> Ok value

    let context : StepPart<StepContext> = Ok

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
            with ex -> handler ex

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

    [<Sealed>]
    type Builder() =
        inherit BaseBuilder()

        member inline _.Run(generator: unit -> StepPart<'T>) : StepPart<'T> =
            fun ctx ->
                let part = generator ()
                part ctx

    let retry (attempts: int) (part: StepPart<'T>) : StepPart<'T> =
        let rec nextRetry attempted ctx =
            let prefixArgs : Prefix.Internal.OptionalPrefixArgs =
                { IsParallel = ctx.IsParallel
                  PrefixOption = ctx.PrefixOption
                  Prefix = ctx.Prefix }

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
            with ex ->
                if attempted < attempts then
                    ex |> Exception.toConsoleMessage |> errorMessage
                    retryMessage ()

                    ctx |> nextRetry (attempted + 1)
                else
                    StepUnhandledEx ex |> Error

        fun ctx -> ctx |> nextRetry 1

    [<Sealed>]
    type RetryBuilder(attempts: int) =
        inherit BaseBuilder()

        member _.Run(generator: unit -> StepPart<'T>) : StepPart<'T> =
            let part =
                fun ctx ->
                    let innerPart = generator ()
                    innerPart ctx

            part |> retry attempts

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

    [<Sealed>]
    type MemoBuilder() =
        inherit BaseBuilder()

        member inline _.Run(generator: unit -> StepPart<'T>) : StepPart<'T> =
            let part = fun ctx -> ctx |> generator ()
            memo part

    let memoRace (part: StepPart<'T>) : StepPart<'T> =
        let mutable memoized = None

        fun ctx ->
            match memoized with
            | Some x -> x
            | None ->
                let result = part ctx
                memoized <- result |> Some
                result

    [<Sealed>]
    type MemoRaceBuilder() =
        inherit BaseBuilder()

        member inline _.Run(generator: unit -> StepPart<'T>) : StepPart<'T> =
            let part = fun ctx -> ctx |> generator ()
            memoRace part

[<AutoOpen>]
module StepPartBuilders =
    let stepPart = StepPart.Builder ()

    let retry (attempts: int) : StepPart.RetryBuilder =
        StepPart.RetryBuilder (attempts)

    let memo = StepPart.MemoBuilder ()
    let memoRace = StepPart.MemoRaceBuilder ()

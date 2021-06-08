namespace FsMake

open System

type StepContext =
    { PipelineName: string
      StepName: string
      IsParallel: bool
      Console: Console.IWriter
      ProcessMonitor: ProcessMonitor.Agent
      LongestStepNameLength: int }

module internal Exception =
    let toConsoleMessage (ex: Exception) : Console.Message list =
        let split = ex.ToString().Split (Environment.NewLine)

        Console.error $"Exception:"
        :: [ for exLine in split -> Console.Error |> Console.messageColor Console.errorColor exLine ]

type StepError =
    | StepUserAbort of message: Console.Message list
    | StepError of message: Console.Message list
    | StepUnhandledEx of ex: exn

module StepError =
    let toConsoleMessage (error: StepError) : Console.Message list =
        match error with
        | StepUserAbort x -> x
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

    let return' (value: 'T) : StepPart<'T> = fun _ -> Ok value

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

[<AutoOpen>]
module StepPartBuilder =
    let stepPart = StepPart.Builder ()

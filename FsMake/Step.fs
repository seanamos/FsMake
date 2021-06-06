namespace FsMake

open System
open System.Diagnostics

type StepContext =
    { PipelineName: string
      StepName: string
      IsParallel: bool
      Console: Console.IWriter
      ProcessMonitor: ProcessMonitor.Agent
      LongestStepNameLength: int }

type StepError =
    | StepUserAbort of message: Console.Message list
    | StepError of message: Console.Message list
    | StepUnhandledEx of ex: exn

module StepError =
    let toConsoleMessage (error: StepError) : Console.Message list =
        match error with
        | StepUserAbort x -> x
        | StepError x -> x
        | StepUnhandledEx ex ->
            let split = ex.ToString().Split (Environment.NewLine)

            Console.error $"Exception:{Environment.NewLine}"
            :: [ for i in 0 .. split.Length - 1 ->
                     let text =
                         if i + 1 <> split.Length then
                             $"{split.[i]}{Environment.NewLine}"
                         else
                             split.[i]

                     Console.Error |> Console.messageColor Console.errorColor text ]

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
                let binded = binder x
                binded ctx
            | Error x -> Error x

    let zip (part1: StepPart<'T>) (part2: StepPart<'T1>) : StepContext -> Result<('T * 'T1), StepError> =
        fun ctx ->
            match (part1 ctx, part2 ctx) with
            | Ok x1res, Ok x2res -> Ok (x1res, x2res)
            | Error e, _ -> Error e
            | _, Error e -> Error e

    let context : StepPart<StepContext> = Ok

    type Builder() =
        member _.Return(value: 'T) : StepPart<'T> =
            fun _ -> Ok value

        member inline _.ReturnFrom(part: StepPart<'T>) : StepPart<'T> =
            part

        member this.Zero() : StepPart<unit> =
            this.Return ()

        member inline _.Bind(part: StepPart<'T>, binder: 'T -> StepPart<'U>) : StepPart<'U> =
            bind binder part

        member _.Delay(generator: unit -> StepPart<'T>) : unit -> StepPart<'T> =
            generator

        member inline _.Run(generator: unit -> StepPart<'T>) : StepPart<'T> =
            fun ctx ->
                let part = generator ()
                part ctx

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

        member _.BindReturn(part: StepPart<'T>, f: 'T -> 'U) =
            map f part

        member _.MergeSources(source1: StepPart<'T>, source2: StepPart<'T1>) : StepContext -> Result<('T * 'T1), StepError> =
            zip source1 source2

        member inline _.Source(part: StepPart<_>) : StepPart<_> =
            part

[<AutoOpen>]
module StepPartBuilder =
    let stepPart = StepPart.Builder ()

type Step =
    { Name: string
      StepPart: StepPart<unit> }

module Step =
    type RunStat =
        { StepName: string
          ExecutionTime: TimeSpan }

    type RunResult = Result<RunStat, RunStat * StepError>

    // I don't know of another way to do this other than replicating the whole builder
    type Builder(name: string) =
        member _.Return(value: 'T) : StepPart<'T> =
            fun _ -> Ok value

        member inline _.ReturnFrom(part: StepPart<'T>) : StepPart<'T> =
            part

        member this.Zero() : StepPart<unit> =
            this.Return ()

        member inline _.Bind(part: StepPart<'T>, binder: 'T -> StepPart<'U>) : StepPart<'U> =
            StepPart.bind binder part

        member _.Delay(generator: unit -> StepPart<'T>) : unit -> StepPart<'T> =
            generator

        member _.Run(generator: unit -> StepPart<unit>) : Step =
            let part =
                fun ctx ->
                    let part = generator ()
                    part ctx

            { Name = name; StepPart = part }

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

        member _.BindReturn(part: StepPart<'T>, f: 'T -> 'U) =
            StepPart.map f part

        member _.MergeSources(source1: StepPart<'T>, source2: StepPart<'T1>) =
            StepPart.zip source1 source2

        member inline _.Source(part: StepPart<_>) : StepPart<_> =
            part

    [<AutoOpen>]
    module internal Internal =
        let concatNames (steps: Step list) : string =
            steps
            |> List.fold (fun state x -> if state.Length = 0 then x.Name else $"{state}, {x.Name}") ""

    let create (name: string) : Builder =
        Builder (name)

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

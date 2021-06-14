namespace FsMake

open System
open System.Diagnostics

type Step =
    { Name: string
      StepPart: StepPart<unit> }

module Step =
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

    let create (name: string) : Builder =
        Builder (name)

    let context = StepPart.context

    let fail (message: string) : StepPart<unit> =
        fun _ ->
            let msg = Console.error message
            StepError [ msg ] |> Error

    let failMessage (messages: Console.Message list) : StepPart<unit> =
        fun _ -> StepError messages |> Error

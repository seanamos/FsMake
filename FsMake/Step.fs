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

type Step =
    { Name: string
      Action: StepContext -> unit }

module Step =
    type RunStat =
        { StepName: string
          ExecutionTime: TimeSpan }


    [<AutoOpen>]
    module internal Internal =
        let concatNames (steps: Step list) : string =
            steps
            |> List.fold (fun state x -> if state.Length = 0 then x.Name else $"{state}, {x.Name}") ""

    let create (name: string) (action: StepContext -> unit) : Step =
        { Name = name; Action = action }

    let run (context: StepContext) (step: Step) : Result<RunStat, RunStat * exn> =
        let stopwatch = Stopwatch ()

        try
            stopwatch.Start ()
            step.Action context
            stopwatch.Stop ()

            { StepName = step.Name
              ExecutionTime = stopwatch.Elapsed }
            |> Ok
        with ex ->
            stopwatch.Stop ()

            ({ StepName = step.Name
               ExecutionTime = stopwatch.Elapsed },
             ex)
            |> Error

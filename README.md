# FsMake

A task runner library for F# scripting.

## Usage

```fsharp
// build.fsx
#r "nuget: FsMake"

open FsMake
open System.IO

let args = fsi.CommandLineArgs |> Array.skip 1 // first arg is the name of script
let useAnsi = EnvVar.getOption "ANSI" |> Option.isSome
let buildConfig = EnvVar.getOption "BUILD_CONFIG"

let clean =
    Step.create "clean" {
        let! ctx = StepPart.context

        if ctx.ExtraArgs |> List.exists (fun x -> x = "--clean") then
            Glob.create "nupkgs/*.nupkg" |> Glob.toPaths |> Seq.iter (File.Delete)

            do! Cmd.createWithArgs "dotnet" [ "clean"; "-v"; "m" ] |> Cmd.run
        else
            Console.warn "Skipping clean" |> ctx.Console.WriteLine
    }

let restore = Step.create "restore" { do! Cmd.createWithArgs "dotnet" [ "restore" ] |> Cmd.run }

let build =
    Step.create "build" {
        let buildConfigArg = buildConfig |> Option.map (fun x -> [ "-c"; x ])

        do!
            Cmd.createWithArgs "dotnet" [ "build"; "--no-restore" ]
            |> Cmd.argMaybe useAnsi "/consoleloggerparameters:ForceConsoleColor"
            |> Cmd.argsOption buildConfigArg
            |> Cmd.run
    }

let testUnit =
    Step.create "test:unit" {
        do!
            Cmd.create "dotnet"
            |> Cmd.args [ "test"
                          "--no-build"
                          "--no-restore"
                          "MyProject.UnitTests" ]
            |> Cmd.run
    }

let testInt =
    Step.create "test:integration" {
        do!
            retry 2 {
                do!
                    Cmd.create "dotnet"
                    |> Cmd.args [ "test"
                                  "--no-build"
                                  "--no-restore"
                                  "MyProject.IntegrationTests" ]
                    |> Cmd.run
            }
    }

Pipelines.create {
    let! build =
        Pipeline.create "build" {
            run clean
            run restore
            run build
        }

    do! Pipeline.createFrom build "test" { run_parallel [ testUnit; testInt ] }

    default_pipeline build
}
|> Pipelines.runWithArgs args
```

To run:
```sh
dotnet fsi build.fsx
```

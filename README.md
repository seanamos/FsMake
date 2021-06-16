# FsMake

F# pipeline step/task runner.

![Nuget](https://img.shields.io/nuget/v/FsMake?style=flat-square)

## Development 🚧

> ⚠ While this library is pre 1.0.0, there is the possibility of breaking changes in the API.

## Usage

```fsharp
// build.fsx
#r "nuget: FsMake"
// or a specific version
// #r "nuget: FsMake, 0.1.0"

open FsMake
open System.IO

let args = fsi.CommandLineArgs
let useAnsi = EnvVar.getOption "ANSI" |> Option.isSome
let buildConfig = EnvVar.getOption "BUILD_CONFIG"
let buildConfigArg = buildConfig |> Option.map (fun x -> [ "-c"; x ])

let clean =
    Step.create "clean" {
        Glob.create "nupkgs/*.nupkg" |> Glob.toPaths |> Seq.iter (File.Delete)

        do! Cmd.createWithArgs "dotnet" [ "clean"; "-v"; "m" ] |> Cmd.run
    }

let restore = Step.create "restore" { do! Cmd.createWithArgs "dotnet" [ "restore" ] |> Cmd.run }

let build =
    Step.create "build" {
        do!
            Cmd.createWithArgs "dotnet" [ "build"; "--no-restore" ]
            |> Cmd.argMaybe useAnsi "/consoleloggerparameters:ForceConsoleColor"
            |> Cmd.argsOption buildConfigArg
            |> Cmd.run
    }

let testUnit =
    Step.create "test:unit" {
        do!
            Cmd.createWithArgs "dotnet" [ "test" ]
            |> Cmd.args [ "--no-build"; "--no-restore"; "MyProject.UnitTests" ]
            |> Cmd.run
    }

let testInt =
    Step.create "test:integration" {
        do!
            retry 2 {
                do!
                    Cmd.createWithArgs "dotnet" [ "test" ]
                    |> Cmd.args [ "--no-build"; "--no-restore"; "MyProject.IntegrationTests" ]
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

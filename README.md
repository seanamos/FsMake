# FsMake

F# pipeline step/task runner.

[![Nuget](https://img.shields.io/nuget/v/FsMake?style=flat-square)](https://www.nuget.org/packages/FsMake)
[![develop branch status](https://img.shields.io/github/workflow/status/seanamos/FsMake/Test%20incoming%20commits/develop?style=flat-square)](https://github.com/seanamos/FsMake/actions/workflows/test.yml?query=branch%3Adevelop+)

## Development 🚧

> ⚠ While this library is pre 1.0.0, there is the possibility of breaking changes in the public API.

## Documentation

Documentation with examples and API reference can be found [here](https://seanamos.github.io/FsMake/).

## Demo

[![FsMake Demo](https://user-images.githubusercontent.com/10598927/122659251-67a2ca00-d176-11eb-9ebd-42c8243a9ff8.png)](https://asciinema.org/a/3qDaGJBnqyyEhHU1IRJi1g8Ta)

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
|> Pipelines.runWithArgsAndExit args
```

To run:
```sh
dotnet fsi build.fsx
```

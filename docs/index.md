---
title: FsMake
category: Index
categoryindex: 0
index: 0
---

# Design Goals

FsMake is a batteries *excluded*, extendable, task runner.

The core is designed to be dependency free and easy to use to run other processes.<br />
This makes it easier to maintain and reduces churn when other tools have breaking changes.

It uses a simple `pipeline` approach to define execution order that is easy to reason about.

## Requirements

 - `>=` .NET 5.0 SDK to use as a package reference in a `.fsx` script.
 - `>=` .NET Core 3.1 SDK to use as a project or `.fsx` with paket generated load scripts.

## Getting Started

In your project root create a `build.cmd` file for Windows users:

    [lang=sh]
    dotnet fsi build.fsx -- %*

Create a `build.sh` for non Windows users:

    [lang=sh]
    #!/bin/sh

    dotnet fsi build.fsx -- "$@"

**Don't forget to `chmod +x build.sh`!**

Create a `build.fsx` file:

    [hide]
    #I "../FsMake/bin/Release/netcoreapp3.1"
    #r "FsMake.dll"

<!-- Sep -->

    [lang=fsharp]
    // Uses the latest stable version of FsMake
    #r "nuget: FsMake"
    // It is recommended to specify a version
    // #r "nuget: FsMake, x.x.x"

    open FsMake

    let args = fsi.CommandLineArgs

    // Creates a restore step.
    let restore =
        Step.create "restore" {
            // Commands that return an exit code other than 0 fail the step by default.
            // This can be controlled with [Cmd.exitCodeCheck].
            do! Cmd.createWithArgs "dotnet" [ "restore" ] |> Cmd.run
        }

    let build =
        Step.create "build" {
            do! Cmd.createWithArgs "dotnet" [ "build" ] |> Cmd.run
        }

    // Define your pipelines, for now we just want a simple "build" pipeline.
    // You can define pipelines with parallel/conditional steps.
    // You can also define pipelines from other pipelines.
    Pipelines.create {
        let! build =
            Pipeline.create "build" {
                run restore
                run build
            }

        default_pipeline build
    }
    |> Pipelines.runWithArgsAndExit args

To run this pipeline:

    [lang=sh]
    ./build.sh

### Further Example

FsMake uses FsMake to run its own pipelines!

Check the [FsMake build.fsx](https://github.com/seanamos/FsMake/blob/master/build.fsx) for a more extensive example.

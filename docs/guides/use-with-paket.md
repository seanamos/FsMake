---
title: Use with paket load scripts
category: Guides
categoryindex: 0
index: 1
---

# Use with paket load scripts

In your `paket.dependencies` file, add the following:

    [lang=text]
    group build
        storage: none
        framework: netcoreapp3.1
        source https://api.nuget.org/v3/index.json

        nuget FsMake

Add the following to your `build.cmd` & `build.sh`:

    [lang=text]
    dotnet paket generate-load-scripts --group build

Create a `build.fsx` file at the root of your solution:

    [hide]
    #I "../../FsMake/bin/Release/netcoreapp3.1"
    #r "FsMake.dll"

<!-- Sep -->

    #load ".paket/load/netcoreapp3.1/build/build.group.fsx"

    open FsMake

    let restore =
        Step.create "restore" {
            do! Cmd.createWithArgs "dotnet" [ "restore" ] |> Cmd.run
        }

    let build =
        Step.create "build" {
            do! Cmd.createWithArgs "dotnet" [ "build" ] |> Cmd.run
        }

    Pipelines.create {
        let! build =
            Pipeline.create "build" {
                run restore
                run build
            }

        default_pipeline build
    }
    |> Pipelines.runWithArgsAndExit argv

To run your build:

    [lang=sh]
    dotnet fsi build.fsx --

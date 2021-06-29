---
title: Use as a project
category: Guides
categoryindex: 0
index: 0
---

# Use as a project

In the example below, replace `MyProject` with your root namespace(s).

Create a folder and a new console project in your solution directory.

    [lang=sh]
    mkdir MyProject.Build

    dotnet new console -lang F# -o MyProject.Build

    dotnet sln add MyProject.Build

Add a nuget reference to FsMake

    [lang=sh]
    // using paket
    dotnet paket add --project MyProject.Build FsMake

    // using nuget
    dotnet add MyProject.Build package FsMake

Open `MyProject.Build/Program.fs` and replace its contents with:

    [hide]
    #I "../../FsMake/bin/Release/netcoreapp3.1"
    #r "FsMake.dll"

<!-- Sep -->

    open FsMake

    [<EntryPoint>]
    let main argv =
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
        |> Pipelines.runWithArgs argv

To run your build:

    [lang=sh]
    dotnet run --project MyProject.Build

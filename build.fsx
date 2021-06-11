#r "nuget: FsMake, 0.1.0-alpha.28"

open FsMake
open System.IO

let args = fsi.CommandLineArgs
let useAnsi = EnvVar.getOption "ANSI" |> Option.isSome
let buildConfig = EnvVar.getOption "BUILD_CONFIG"
let buildConfigArg = buildConfig |> Option.map (fun x -> [ "-c"; x ])

let semVer =
    Cmd.create "dotnet"
    |> Cmd.args [ "gitversion"
                  "/showvariable"
                  "semver" ]
    |> Cmd.redirectOutput Cmd.RedirectToBoth
    |> Cmd.runAndGetResult

let clean =
    Step.create "clean" {
        let! ctx = StepPart.context

        if ctx.ExtraArgs |> List.contains "--clean" then
            Glob.create "nupkgs/*" |> Glob.toPaths |> Seq.iter (File.Delete)

            do! Cmd.createWithArgs "dotnet" [ "clean"; "-v"; "m" ] |> Cmd.run
    }

let build =
    Step.create "build" {
        do!
            Cmd.create "dotnet"
            |> Cmd.argsOption buildConfigArg
            |> Cmd.argMaybe useAnsi "/consoleloggerparameters:ForceConsoleColor"
            |> Cmd.run
    }

Pipelines.create {
    do! Pipeline.create "build" {
        run clean
        run build
    }
}
|> Pipelines.runWithArgs args

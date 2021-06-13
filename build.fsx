#r "nuget: FsMake, 0.1.0-alpha.33"

open FsMake
open System.IO

let args = fsi.CommandLineArgs
let useAnsi = EnvVar.getOption "ANSI" |> Option.isSome
let buildConfig = EnvVar.getOption "BUILD_CONFIG"
let buildConfigArg = buildConfig |> Option.map (fun x -> [ "-c"; x ])
let nugetApiKey = EnvVar.getOption "NUGET_API_KEY"

let semVerPart =
    Cmd.createWithArgs "dotnet" [ "gitversion" ]
    |> Cmd.args [ "/showvariable"; "semver" ]
    |> Cmd.redirectOutput Cmd.RedirectToBoth
    |> Cmd.runAndGetResult
    |> StepPart.map (fun x -> x.Output.Std)
    |> StepPart.memo

let clean =
    Step.create "clean" {
        let! ctx = Step.context

        // only clean if --clean is specified
        if ctx.ExtraArgs |> List.contains "--clean" then
            Glob.create "nupkgs/*" |> Glob.toPaths |> Seq.iter (File.Delete)

            do! Cmd.createWithArgs "dotnet" [ "clean"; "-v"; "m" ] |> Cmd.run
        else
            Console.warn "Skipping clean, --clean not specified" |> ctx.Console.WriteLine
    }

let restore = Step.create "restore" { do! Cmd.createWithArgs "dotnet" [ "restore" ] |> Cmd.run }

let build =
    Step.create "build" {
        do!
            Cmd.createWithArgs "dotnet" [ "build" ]
            |> Cmd.argsOption buildConfigArg
            |> Cmd.argMaybe useAnsi "/consoleloggerparameters:ForceConsoleColor"
            |> Cmd.run
    }

let testFormat = Step.create "test:format" { do! Cmd.createWithArgs "dotnet" [ "fantomas"; "-r"; "."; "--check" ] |> Cmd.run }
let testLint = Step.create "test:lint" { do! Cmd.createWithArgs "dotnet" [ "fsharplint"; "lint"; "FsMake.sln" ] |> Cmd.run }

let nupkgCreate =
    Step.create "nupkg:create" {
        let! semVer = semVerPart

        do!
            Cmd.createWithArgs "dotnet" [ "paket"; "pack" ]
            |> Cmd.args [ "--version"; semVer; "nupkgs" ]
            |> Cmd.run
    }

let nupkgPush =
    Step.create "nupkg:push" {
        let! ctx = Step.context
        let! semver = semVerPart
        let listPkg = ctx.ExtraArgs |> List.contains "--list-nupkg"
        let pkg = $"nupkgs/FsMake.{semver}"

        match nugetApiKey with
        | None -> do! Step.fail "NUGET_API_KEY env var not specified!"
        | Some key ->
            do!
                Cmd.createWithArgs "dotnet" [ "nuget"; "push"; pkg ]
                |> Cmd.args [ "--source"; "https://api.nuget.org/v3/index.json"; "--api-key" ]
                |> Cmd.argSecret key
                |> Cmd.run

            // unlist the package
            if not listPkg then
                Console.info $"Unlisting FsMake {semver}" |> ctx.Console.WriteLine

                do!
                    Cmd.createWithArgs "dotnet" [ "nuget"; "delete"; "FsMake"; semver ]
                    |> Cmd.args [ "--source"; "https://api.nuget.org/v3/index.json"; "--api-key" ]
                    |> Cmd.argSecret key
                    |> Cmd.run

    }

Pipelines.create {
    let! build =
        Pipeline.create "build" {
            run clean
            run restore
            run build
        }

    do! Pipeline.createFrom build "test" { run_parallel [ testFormat; testLint ] }

    let! nupkgCreate = Pipeline.createFrom build "nupkg:create" { run nupkgCreate }

    do! Pipeline.createFrom nupkgCreate "publish" { run nupkgPush }

    default_pipeline build
}
|> Pipelines.runWithArgs args

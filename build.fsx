#r "nuget: FsMake, 0.1.0"

open FsMake
open System.IO

let args = fsi.CommandLineArgs
let useAnsi = EnvVar.getOptionAs<bool> "ANSI" |> Option.contains true
let buildConfig = EnvVar.getOption "BUILD_CONFIG"
let buildConfigArg = buildConfig |> Option.map (fun x -> [ "-c"; x ])
let nugetApiKey = EnvVar.getOption "NUGET_API_KEY"

let nugetListPkg =
    EnvVar.getOptionAs<bool> "NUGET_LIST_PKG"
    |> Option.contains true

let semVerPart =
    Cmd.createWithArgs "dotnet" [ "gitversion" ]
    |> Cmd.args [ "/showvariable"; "semver" ]
    |> Cmd.redirectOutput Cmd.RedirectToBoth
    |> Cmd.result
    |> StepPart.map (fun x -> x.Output.Std)
    |> StepPart.memo

let clean =
    Step.create "clean" {
        let! ctx = Step.context

        // only clean if --clean is specified
        if ctx.ExtraArgs |> List.contains "--clean" then
            Glob.create "nupkgs/*"
            |> Glob.toPaths
            |> Seq.iter (File.Delete)

            Glob.create "**/bin"
            |> Glob.add "**/obj"
            |> Glob.toPaths
            |> Seq.iter (fun x -> Directory.Delete (x, true))

            do!
                Cmd.createWithArgs "dotnet" [ "clean"; "-v"; "m" ]
                |> Cmd.run
        else
            Console.warn "Skipping clean, "
            |> Console.appendParts [ Console.Token "--clean"; Console.Text " not specified" ]
            |> ctx.Console.WriteLine
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

let testFormat =
    Step.create "test:format" {
        do!
            Cmd.createWithArgs "dotnet" [ "fantomas"; "-r"; "."; "--check" ]
            |> Cmd.run
    }

let testLint =
    Step.create "test:lint" {
        do!
            Cmd.createWithArgs "dotnet" [ "fsharplint"; "lint"; "FsMake.sln" ]
            |> Cmd.run
    }

let testUnit =
    Step.create "test:unit" {
        do!
            Cmd.createWithArgs "dotnet" [ "run"; "--no-restore"; "--no-build" ]
            |> Cmd.workingDir "FsMake.UnitTests"
            |> Cmd.run
    }

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
        let pkg = $"nupkgs/FsMake.{semver}.nupkg"

        match nugetApiKey with
        | None -> do! Step.fail "NUGET_API_KEY env var not specified!"
        | Some key ->
            do!
                Cmd.createWithArgs "dotnet" [ "nuget"; "push"; pkg ]
                |> Cmd.args [ "--source"; "https://api.nuget.org/v3/index.json"; "--api-key" ]
                |> Cmd.argSecret key
                |> Cmd.run

            // unlist the package
            if not nugetListPkg then
                Console.info "Unlisting "
                |> Console.appendToken $"FsMake {semver}"
                |> ctx.Console.WriteLine

                do!
                    Cmd.createWithArgs "dotnet" [ "nuget"; "delete"; "FsMake"; semver ]
                    |> Cmd.args [ "--non-interactive"
                                  "--source"
                                  "https://api.nuget.org/v3/index.json"
                                  "--api-key" ]
                    |> Cmd.argSecret key
                    |> Cmd.run

    }

let tag =
    Step.create "tag" {
        let! semver = semVerPart

        do!
            Cmd.createWithArgs "git" [ "tag" ]
            |> Cmd.args [ "-a"; semver; "-m"; $"{semver} release" ]
            |> Cmd.run

        do!
            Cmd.createWithArgs "git" [ "push"; "origin"; semver ]
            |> Cmd.run
    }

Pipelines.create {
    let! build =
        Pipeline.create "build" {
            run clean
            run restore
            run build
        }

    do! Pipeline.createFrom build "test" { run_parallel [ testFormat; testLint; testUnit ] }

    let! nupkgCreate = Pipeline.createFrom build "nupkg:create" { run nupkgCreate }

    do! Pipeline.createFrom nupkgCreate "publish:nupkg" { run nupkgPush }

    do! Pipeline.create "publish:tag" { run tag }

    default_pipeline build
}
|> Pipelines.runWithArgs args

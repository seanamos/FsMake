#r "nuget: FsMake, 0.2.1"

open FsMake
open System
open System.IO
open System.Text.Json

let args = fsi.CommandLineArgs
let isRelease = EnvVar.getOptionAs<bool> "RELEASING" |> Option.contains true
let useAnsi = EnvVar.getOptionAs<bool> "ANSI" |> Option.contains true
let buildConfig = EnvVar.getOption "BUILD_CONFIG"
let buildConfigArg = buildConfig |> Option.map (fun x -> [ "-c"; x ])

let nugetListPkg = EnvVar.getOptionAs<bool> "NUGET_LIST_PKG" |> Option.contains true

let getNugetApiKey = EnvVar.getOrFail "NUGET_API_KEY"
let getGithubToken = EnvVar.getOrFail "GITHUB_TOKEN"

let getGitversion =
    Cmd.createWithArgs "dotnet" [ "gitversion" ]
    |> Cmd.redirectOutput Cmd.RedirectToBoth
    |> Cmd.result
    |> StepPart.map (fun x ->
        x.Output.Std
        |> JsonSerializer.Deserialize<{| MajorMinorPatch: string
                                         SemVer: string
                                         BranchName: string
                                         PreReleaseNumber: Nullable<int> |}>
    )
    |> StepPart.memo

let clean =
    Step.create "clean" {
        let! ctx = Step.context

        // only clean if --clean is specified
        if ctx.ExtraArgs |> List.contains "--clean" then
            Glob.create "nupkgs/*"
            |> Glob.add ".fsdocs/*"
            |> Glob.toPaths
            |> Seq.iter (File.Delete)

            Glob.create "**/bin"
            |> Glob.add "**/obj"
            |> Glob.toPaths
            |> Seq.iter (fun x -> Directory.Delete (x, true))

            do! Cmd.createWithArgs "dotnet" [ "clean"; "-v"; "m" ] |> Cmd.run
        else
            Console.warn "Skipping clean, "
            |> Console.appendParts [ Console.Token "--clean"; Console.Text " not specified" ]
            |> ctx.Console.WriteLine
    }

let assemblyinfo = Step.create "assemblyinfo" { do! Cmd.createWithArgs "dotnet" [ "gitversion"; "/updateassemblyinfo" ] |> Cmd.run }

let restore = Step.create "restore" { do! Cmd.createWithArgs "dotnet" [ "restore" ] |> Cmd.run }

let build =
    Step.create "build" {
        do!
            Cmd.createWithArgs "dotnet" [ "build"; "--warnaserror" ]
            |> Cmd.argsOption buildConfigArg
            |> Cmd.argMaybe useAnsi "/consoleloggerparameters:ForceConsoleColor"
            |> Cmd.run
    }

let ``test:format`` = Step.create "test:format" { do! Cmd.createWithArgs "dotnet" [ "fantomas"; "-r"; "."; "--check" ] |> Cmd.run }
let ``test:lint`` = Step.create "test:lint" { do! Cmd.createWithArgs "dotnet" [ "fsharplint"; "lint"; "FsMake.sln" ] |> Cmd.run }

let ``test:unit`` =
    Step.create "test:unit" {
        do!
            Cmd.createWithArgs "dotnet" [ "run"; "--no-build" ]
            |> Cmd.argsOption buildConfigArg
            |> Cmd.workingDir "FsMake.UnitTests"
            |> Cmd.run
    }

let ``nupkg:create`` =
    Step.create "nupkg:create" {
        let! gitversion = getGitversion
        let semver = gitversion.SemVer

        do!
            Cmd.createWithArgs "dotnet" [ "pack"; "--no-build" ]
            |> Cmd.argsOption buildConfigArg
            |> Cmd.args [ $"/p:Version=%s{semver}"; "-o"; "nupkgs"; "FsMake" ]
            |> Cmd.run
    }

let ``nupkg:push`` =
    Step.create "nupkg:push" {
        let! nugetApiKey = getNugetApiKey
        let! ctx = Step.context
        let! gitversion = getGitversion
        let semver = gitversion.SemVer
        let pkg = $"nupkgs/FsMake.%s{semver}.nupkg"

        do!
            Cmd.createWithArgs "dotnet" [ "nuget"; "push"; pkg ]
            |> Cmd.args [ "--source"; "https://api.nuget.org/v3/index.json"; "--api-key" ]
            |> Cmd.argSecret nugetApiKey
            |> Cmd.run

        // unlist the package
        if not nugetListPkg then
            Console.info "Unlisting "
            |> Console.appendToken $"FsMake %s{semver}"
            |> ctx.Console.WriteLine

            do!
                Cmd.createWithArgs "dotnet" [ "nuget"; "delete"; "FsMake"; semver ]
                |> Cmd.args [ "--non-interactive"
                              "--source"
                              "https://api.nuget.org/v3/index.json"
                              "--api-key" ]
                |> Cmd.argSecret nugetApiKey
                |> Cmd.run

    }

let docs =
    Step.create "docs" {
        do!
            Cmd.createWithArgs "dotnet" [ "fsdocs"; "build" ]
            |> Cmd.args [ "--output"; "docs-output" ]
            |> Cmd.argsOption (buildConfig |> Option.map (fun x -> [ "--properties"; $"Configuration={x}" ]))
            |> Cmd.run
    }

let ``github:pages`` =
    Step.create "github:pages" {
        let! gitversion = getGitversion

        do! Cmd.createWithArgs "git" [ "fetch" ] |> Cmd.run
        do! Cmd.createWithArgs "git" [ "checkout"; "gh-pages" ] |> Cmd.run
        do! Cmd.createWithArgs "git" [ "rm"; "--ignore-unmatch"; "*" ] |> Cmd.run

        do!
            Cmd.createWithArgs "git" [ "checkout" ]
            |> Cmd.args [ "HEAD"; "--"; ".gitignore" ]
            |> Cmd.run

        let currDir = Environment.CurrentDirectory
        let rootDir = [| currDir; "docs-output" |] |> Path.Combine

        Glob.create "docs-output/**"
        |> Glob.toPathTypes
        |> Seq.iter
            (function
            | Glob.File x ->
                let newPath = Path.GetRelativePath (rootDir, x)
                let newDir = Path.GetDirectoryName (newPath)

                if not <| String.IsNullOrEmpty (newDir) && not <| Directory.Exists (newDir) then
                    Directory.CreateDirectory (newDir) |> ignore

                File.Move (x, newPath)
            | _ -> ())

        do! Cmd.createWithArgs "git" [ "add"; "." ] |> Cmd.run

        let! gitStatus =
            Cmd.createWithArgs "git" [ "status"; "-s" ]
            |> Cmd.redirectOutput Cmd.RedirectToBoth
            |> Cmd.result
            |> StepPart.map (fun x -> x.Output.Std)

        if not <| String.IsNullOrEmpty (gitStatus) then
            do!
                Cmd.createWithArgs "git" [ "commit"; "-m"; $"Docs update for {gitversion.SemVer}" ]
                |> Cmd.run

            do! Cmd.createWithArgs "git" [ "push" ] |> Cmd.run
        else
            let! ctx = Step.context

            Console.warn "Docs were not updated, nothing to commit" |> ctx.Console.WriteLine

        do! Cmd.createWithArgs "git" [ "checkout"; gitversion.BranchName ] |> Cmd.run
    }

let ``github:release`` =
    Step.create "github:release" {
        let! githubToken = getGithubToken
        let! gitversion = getGitversion
        let semver = gitversion.SemVer
        let isPre = gitversion.PreReleaseNumber.HasValue
        let milestone = gitversion.MajorMinorPatch
        let branch = gitversion.BranchName

        do!
            Cmd.createWithArgs "dotnet" [ "gitreleasemanager"; "create"; "--token" ]
            |> Cmd.argSecret githubToken
            |> Cmd.args [ "-o"; "seanamos"; "-r"; "FsMake" ]
            |> Cmd.args [ "-m"; milestone; "-n"; semver; "-c"; branch ]
            |> Cmd.argMaybe isPre "--pre"
            |> Cmd.run
    }

Pipelines.create {
    let! build =
        Pipeline.create "build" {
            run clean
            run restore
            maybe_run assemblyinfo isRelease
            run build
        }

    do! Pipeline.createFrom build "test" { run_parallel [ ``test:format``; ``test:lint``; ``test:unit`` ] }

    let! nupkgCreate = Pipeline.createFrom build "nupkg:create" { run ``nupkg:create`` }

    do! Pipeline.createFrom nupkgCreate "publish:nupkg" { run ``nupkg:push`` }

    do!
        Pipeline.createFrom build "github:pages" {
            run docs
            run ``github:pages``
        }

    do! Pipeline.create "publish:github" { run ``github:release`` }

    default_pipeline build
}
|> Pipelines.runWithArgs args

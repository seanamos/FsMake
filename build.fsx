#r "nuget: FsMake, 0.5.0"

open FsMake
open System
open System.IO
open System.Text.Json

let args = fsi.CommandLineArgs
let shouldClean = EnvVar.getOptionAs<int> "CLEAN" |> Option.contains 1
let isRelease = EnvVar.getOptionAs<int> "RELEASING" |> Option.contains 1
let useAnsi = EnvVar.getOptionAs<int> "ANSI" |> Option.contains 1
let buildConfig = EnvVar.getOption "BUILD_CONFIG" |> Option.defaultValue "Debug"

let nugetListPkg = EnvVar.getOptionAs<bool> "NUGET_LIST_PKG" |> Option.contains true

let getNugetApiKey = EnvVar.getOrFail "NUGET_API_KEY"
let getGithubToken = EnvVar.getOrFail "GITHUB_TOKEN"

let makeGitversion =
    Cmd.createWithArgs "dotnet" [ "gitversion" ]
    |> Cmd.redirectOutput Cmd.RedirectToBoth
    |> Cmd.result
    |> Make.map (fun x ->
        x.Output.Std
        |> JsonSerializer.Deserialize<{| MajorMinorPatch: string
                                         SemVer: string
                                         BranchName: string
                                         PreReleaseNumber: Nullable<int> |}>
    )
    |> Make.memo

let clean =
    Step.create "clean" {
        let! ctx = Step.context

        // only clean if CLEAN=1 is specified
        if shouldClean then
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
            |> Console.appendParts [ Console.Token "CLEAN=1"; Console.Text " env var not set" ]
            |> ctx.Console.WriteLine
    }

let restore = Step.create "restore" { do! Cmd.createWithArgs "dotnet" [ "restore" ] |> Cmd.run }

let assemblyinfo = Step.create "assemblyinfo" { do! Cmd.createWithArgs "dotnet" [ "gitversion"; "/updateassemblyinfo" ] |> Cmd.run }

let build =
    Step.create "build" {
        do!
            Cmd.createWithArgs "dotnet" [ "build"; "--warnaserror" ]
            |> Cmd.args [ "-c"; buildConfig ]
            |> Cmd.argMaybe useAnsi "/consoleloggerparameters:ForceConsoleColor"
            |> Cmd.run
    }

let ``test:format`` = Step.create "test:format" { do! Cmd.createWithArgs "dotnet" [ "fantomas"; "-r"; "."; "--check" ] |> Cmd.run }
let ``test:lint`` = Step.create "test:lint" { do! Cmd.createWithArgs "dotnet" [ "fsharplint"; "lint"; "FsMake.sln" ] |> Cmd.run }

let ``test:tests`` =
    Step.create "test:tests" {
        do!
            Cmd.createWithArgs "dotnet" [ "run"; "--no-build" ]
            |> Cmd.args [ "-c"; buildConfig ]
            |> Cmd.workingDir "FsMake.Tests"
            |> Cmd.run
    }

let ``nupkg:create`` =
    Step.create "nupkg:create" {
        let! gitversion = makeGitversion
        let semver = gitversion.SemVer

        do!
            Cmd.createWithArgs "dotnet" [ "pack"; "--no-build" ]
            |> Cmd.args [ "-c"; buildConfig ]
            |> Cmd.args [ $"/p:Version=%s{semver}"; "-o"; "nupkgs"; "FsMake" ]
            |> Cmd.run
    }

let ``nupkg:push`` =
    Step.create "nupkg:push" {
        let! nugetApiKey = getNugetApiKey
        let! ctx = Step.context
        let! gitversion = makeGitversion
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
            |> Cmd.args [ "--properties"; $"Configuration={buildConfig}" ]
            |> Cmd.run
    }

let ``github:pages`` =
    Step.create "github:pages" {
        let! gitversion = makeGitversion

        do! Cmd.createWithArgs "git" [ "stash"; "-u" ] |> Cmd.run
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
        |> Seq.iter (
            function
            | Glob.File x ->
                let newPath = Path.GetRelativePath (rootDir, x)
                let newDir = Path.GetDirectoryName (newPath)

                if not <| String.IsNullOrEmpty (newDir) && not <| Directory.Exists (newDir) then
                    Directory.CreateDirectory (newDir) |> ignore

                File.Move (x, newPath)
            | _ -> ()
        )

        do! Cmd.createWithArgs "git" [ "add"; "." ] |> Cmd.run

        let! gitStatus =
            Cmd.createWithArgs "git" [ "status"; "-s" ]
            |> Cmd.redirectOutput Cmd.RedirectToBoth
            |> Cmd.result
            |> Make.map (fun x -> x.Output.Std)

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
        let! gitversion = makeGitversion
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
            run_maybe assemblyinfo isRelease
            run build
        }

    do!
        Pipeline.createFrom build "test" {
            run_parallel [ ``test:format``; ``test:lint`` ]
            run ``test:tests``
        }

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
|> Pipelines.runWithArgsAndExit args

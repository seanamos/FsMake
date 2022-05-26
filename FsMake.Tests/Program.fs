module FsMake.Tests.Program

open Expecto
open FsMake.Tests

[<Tests>]
let integrationTests =
    testList
        "Integration"
        [
            Integration.Cmd.tests
            Integration.EnvVar.tests
            Integration.Pipelines.tests
            Integration.TestProjFsx.tests
            Integration.TestProjRun.tests
        ]

[<Tests>]
let unitTests =
    testList
        "Unit"
        [
            Unit.Cli.tests
            Unit.Console.tests
            Unit.Glob.globTests
            Unit.Glob.parseTests
            Unit.Glob.regexTests
            Unit.Make.tests
            Unit.Pipeline.tests
            Unit.Pipelines.tests
            Unit.Step.tests
        ]

[<EntryPoint>]
let main argv =
    runTestsInAssemblyWithCLIArgs [] argv

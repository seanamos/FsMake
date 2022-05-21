module FsMake.Tests.Integration.TestProj

open Expecto
open Expecto.Flip
open System
open System.IO
open System.Diagnostics

let testProjDir =
    let currDir = Directory.GetCurrentDirectory ()

    let rec walkDirs from =
        let testDir = from + "/testproj"

        if Directory.Exists (testDir) then
            testDir
        else
            let nextDir = Directory.GetParent from
            walkDirs nextDir.FullName

    walkDirs currDir

let createRunnerProcStartInfo args =
#if RELEASE
    let procStartInfo = ProcessStartInfo ("dotnet", "fsi --define:RELEASE build.fsx")
#else
    let procStartInfo = ProcessStartInfo ("dotnet", "fsi build.fsx")
#endif

    if args |> String.IsNullOrEmpty |> not then
        procStartInfo.Arguments <- procStartInfo.Arguments + $" {args}"

    procStartInfo.Arguments <- procStartInfo.Arguments + " -o ansi"

    procStartInfo.WorkingDirectory <- testProjDir

    let process' = new Process ()
    process'.StartInfo <- procStartInfo
    process'


let tests =
    testSequenced
    <| testList
        "TestProject tests"
        [
            test "kitchen sink" {
                use process' = createRunnerProcStartInfo "kitchen-sink"

                process'.Start () |> ignore
                process'.WaitForExit ()

                process'.ExitCode |> Expect.equal "Expected exit code to be 0" 0
            }

            test "kitchen sink fails" {
                use process' = createRunnerProcStartInfo "kitchen-sink"
                process'.StartInfo.EnvironmentVariables.Add ("FAIL", "1")

                process'.Start () |> ignore
                process'.WaitForExit ()

                (process'.ExitCode, 0)
                |> Expect.isGreaterThan "Expected exit code to be greater than 0"
            }
        ]

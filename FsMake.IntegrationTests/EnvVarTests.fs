module FsMake.IntegrationTests.EnvVarTests

open Expecto
open Expecto.Flip
open FsMake
open System

[<Tests>]
let tests =
    let consoleWriter =
        { new Console.IWriter with
            member _.Write(_) =
                ()
        }

    let procMon = ProcessMonitor.create consoleWriter

    let ctx: MakeContext =
        {
            PipelineName = "testPipeline"
            StepName = "testStep"
            IsParallel = false
            Console = consoleWriter
            Prefix = Console.Text ""
            PrefixOption = Prefix.WhenParallel
            ProcessMonitor = procMon
            ExtraArgs = []
        }

    testList
        "EnvVar Integration Tests"
        [
            test "get returns env var" {
                let envVarName = Guid.NewGuid().ToString()

                Environment.SetEnvironmentVariable(envVarName, "1")

                let value = EnvVar.get envVarName

                value |> Expect.equal "Expected env var to be set" "1"
            }

            test "get throws when no env var set" {
                let envVarName = Guid.NewGuid().ToString()

                Expect.throwsT<EnvVar.NotFoundException>
                    "Expect exception when env var not set"
                    (fun () -> EnvVar.get envVarName |> ignore)
            }

            test "getAs returns typed env var" {
                let envVarName = Guid.NewGuid().ToString()

                Environment.SetEnvironmentVariable(envVarName, "1")

                let value = EnvVar.getAs<int> envVarName

                value |> Expect.equal "Expected env var to be set" 1
            }

            test "getAs throws when env cannot be converted" {
                let envVarName = Guid.NewGuid().ToString()

                Environment.SetEnvironmentVariable(envVarName, "asdsa")

                Expect.throws
                    "Expect exception when env var cannot be converted"
                    (fun () -> EnvVar.getAs<int> envVarName |> ignore)
            }

            test "getOption returns Some" {
                let envVarName = Guid.NewGuid().ToString()

                Environment.SetEnvironmentVariable(envVarName, "1")

                let value = EnvVar.getOption envVarName

                value |> Expect.equal "Expected env var to be set" (Some "1")
            }

            test "getOption returns None when env var not set" {
                let envVarName = Guid.NewGuid().ToString()

                let value = EnvVar.getOption envVarName

                value |> Expect.equal "Expected env var to be set" (None)
            }

            test "getOptionAs returns Some typed" {
                let envVarName = Guid.NewGuid().ToString()

                Environment.SetEnvironmentVariable(envVarName, "1")

                let value = EnvVar.getOptionAs<int> envVarName

                value |> Expect.equal "Expected Some when env var set" (Some 1)
            }

            test "getOptionAs returns None when env var cannot be converted" {
                let envVarName = Guid.NewGuid().ToString()

                let value = EnvVar.getOptionAs<int> envVarName

                value |> Expect.equal "Expected None when env var not set" (None)
            }

            test "getOrFail returns Ok" {
                let envVarName = Guid.NewGuid().ToString()

                Environment.SetEnvironmentVariable(envVarName, "1")

                let makeEnv = EnvVar.getOrFail envVarName

                let result = ctx |> makeEnv

                result |> Expect.equal "Expected Ok when env var set" (Ok "1")
            }

            test "getOrFail returns Error when env var not set" {
                let envVarName = Guid.NewGuid().ToString()

                let makeEnv = EnvVar.getOrFail envVarName

                let result = ctx |> makeEnv

                result |> Expect.isError "Expected Error when env var not set"
            }

            test "getAsOrFail returns Ok typed" {
                let envVarName = Guid.NewGuid().ToString()

                Environment.SetEnvironmentVariable(envVarName, "1")

                let makeEnv = EnvVar.getAsOrFail<int> envVarName

                let result = ctx |> makeEnv

                result |> Expect.equal "Expected Ok when env var set" (Ok 1)
            }

            test "getAsOrFail returns Error when env var cannot be converted" {
                let envVarName = Guid.NewGuid().ToString()

                Environment.SetEnvironmentVariable(envVarName, "asd")

                let makeEnv = EnvVar.getAsOrFail<int> envVarName

                let result = ctx |> makeEnv

                result |> Expect.isError "Expected Error when env var cannot be converted"
            }
        ]

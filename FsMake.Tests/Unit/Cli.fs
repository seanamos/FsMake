module FsMake.Tests.Unit.Cli

open Expecto
open Expecto.Flip
open FsCheck
open FsMake
open FsMake.Tests

module Gen =
    type HelpArgs = HelpArgs of string array

    let cliHelpArgArb =
        gen {
            let! strList =
                Arb.generate<NonNull<string>>
                |> Gen.filter (fun x -> x <> NonNull "--")
                |> Gen.listOf
                |> Gen.map (List.map (fun (NonNull x) -> x))

            let! args = "--help" :: strList |> Gen.shuffle

            return HelpArgs args
        }
        |> Arb.fromGen

    let config =
        { FsCheckConfig.defaultConfig with
            arbitrary = typeof<HelpArgs>.DeclaringType :: FsCheckConfig.defaultConfig.arbitrary
        }

let tests =
    testList
        "Cli tests"
        [
            test "parseArgs should parse .fsx file" {
                let parsed = [| "test.fsx" |] |> Cli.parseArgs

                let args = parsed |> Expect.wantOk "parsed should be Ok"
                teste <@ args.Executable = Some (Cli.ExecutableType.Fsx "test.fsx") @>
            }

            test "parseArgs should parse .dll file" {
                let parsed = [| "test.dll" |] |> Cli.parseArgs

                let args = parsed |> Expect.wantOk "parsed should be Ok"
                teste <@ args.Executable = Some (Cli.ExecutableType.Dll "test.dll") @>
            }

            test "parseArgs should not match non script file arg" {
                let parsed = [| "test" |] |> Cli.parseArgs

                let args = parsed |> Expect.wantOk "parsed should be Ok"
                teste <@ args.Executable = None @>
            }

            test "parseArgs should parse first arg as pipeline when no script file arg" {
                let parsed = [| "pipeline" |] |> Cli.parseArgs

                let args = parsed |> Expect.wantOk "parsed should be Ok"
                teste <@ args.Pipeline = Some "pipeline" @>
            }

            test "parseArgs should parse second arg as pipeline when script file arg" {
                let parsed = [| "test.fsx"; "pipeline" |] |> Cli.parseArgs

                let args = parsed |> Expect.wantOk "parsed should be Ok"
                teste <@ args.Pipeline = Some "pipeline" @>
            }

            test "parseArgs should parse third arg as pipeline when leading -- when script file arg" {
                let parsed = [| "test.fsx"; "--"; "pipeline" |] |> Cli.parseArgs

                let args = parsed |> Expect.wantOk "parsed should be Ok"
                teste <@ args.Pipeline = Some "pipeline" @>
            }

            testPropertyWithConfig Gen.config "parseArgs should parse --help in any position"
            <| fun (Gen.HelpArgs rawArgs) ->
                let parsed = rawArgs |> Cli.parseArgs

                let args = parsed |> Expect.wantOk "parsed should be Ok"

                teste <@ args.PrintHelp @>

            test "parseArgs does not parse --help when ExtraArg" {
                let parsed = [| "--"; "--help" |] |> Cli.parseArgs

                let args = parsed |> Expect.wantOk "parsed should be Ok"
                teste <@ not args.PrintHelp @>
                teste <@ args.ExtraArgs = [ "--help" ] @>
            }

            test "parseArgs should parse --help after leading -- when using .fsx" {
                let parsed = [| "test.fsx"; "--"; "--help" |] |> Cli.parseArgs

                let args = parsed |> Expect.wantOk "parsed should be Ok"
                teste <@ args.PrintHelp @>
            }

            test "parseArgs parses -v/--verbosity" {
                let combos =
                    [
                        ("disabled", Cli.Disabled)
                        ("quiet", Cli.Quiet)
                        ("normal", Cli.Normal)
                        ("all", Cli.All)
                    ]

                let check arg =
                    combos
                    |> List.iter (fun (opt, exp) ->
                        let parsed = [| arg; opt |] |> Cli.parseArgs
                        let args = parsed |> Expect.wantOk "parsed should be Ok"

                        teste <@ args.Verbosity = exp @>
                    )

                check "-v"
                check "--verbosity"
            }

            test "parseArgs errors on invalid verbosity" {
                let parsed = [| "-v"; "dsadsadsa" |] |> Cli.parseArgs

                parsed |> Expect.isError "parsed should be Error"
            }

            test "parseArgs errors on missing verbosity" {
                let parsed = [| "-v" |] |> Cli.parseArgs

                parsed |> Expect.isError "parsed should be Error"
            }

            test "parseArgs parses -o/--console-output" {
                let combos = [ ("ansi", Cli.Ansi); ("standard", Cli.Standard) ]

                let check arg =
                    combos
                    |> List.iter (fun (opt, exp) ->
                        let parsed = [| arg; opt |] |> Cli.parseArgs
                        let args = parsed |> Expect.wantOk "parsed should be Ok"

                        teste <@ args.ConsoleOutput = exp @>
                    )

                check "-o"
                check "--console-output"
            }

            test "parseArgs errors on invalid console-output" {
                let parsed = [| "-o"; "dsadsadsa" |] |> Cli.parseArgs

                parsed |> Expect.isError "parsed should be Error"
            }

            test "parseArgs errors on missing console-output" {
                let parsed = [| "-o" |] |> Cli.parseArgs

                parsed |> Expect.isError "parsed should be Error"
            }

            test "parseArgs parses ExtraArgs" {
                let parsed = [| "--"; "--extra1"; "-x2"; "ex3" |] |> Cli.parseArgs

                let args = parsed |> Expect.wantOk "parsed should be Ok"
                teste <@ args.ExtraArgs = [ "--extra1"; "-x2"; "ex3" ] @>
            }
        ]

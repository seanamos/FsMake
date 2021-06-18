namespace FsMake

open System.Reflection

module internal Cli =
    type Verbosity =
        | Disabled
        | Quiet
        | Normal
        | All

    module Verbosity =
        let toConsoleVerbosity (verbosity: Verbosity) : Console.Verbosity =
            match verbosity with
            | Disabled -> Console.Disabled
            | Quiet -> Console.Quiet
            | Normal -> Console.Normal
            | All -> Console.All

    type ConsoleOutput =
        | Ansi
        | Standard

    module ConsoleOutput =
        let toConsoleOutputType (output: ConsoleOutput) : Console.OutputType =
            match output with
            | Ansi -> Console.Ansi
            | Standard -> Console.Standard

    type ParseError =
        | InvalidOptionParam of option: string * param: string
        | OptionParamMissing of option: string
        | InvalidArgument of arg: string

    module ParseError =
        let toConsoleMessage (error: ParseError) : Console.Message =
            match error with
            | InvalidOptionParam (option, param) ->
                Console.error "Invalid parameter \""
                |> Console.appendToken param
                |> Console.append "\" for option "
                |> Console.appendToken option
            | OptionParamMissing option ->
                Console.error "Missing required parameter for option "
                |> Console.appendToken option
            | InvalidArgument arg -> Console.error "Invalid argument " |> Console.appendToken arg

    type Args =
        {
            PrintHelp: bool
            ScriptFile: string option
            Pipeline: string option
            ConsoleOutput: ConsoleOutput
            Verbosity: Verbosity
            ExtraArgs: string list
        }

    let printUsage (writer: Console.IWriter) (args: Args) (errors: ParseError list) : unit =
        let assembly = Assembly.GetExecutingAssembly ()
        let assemblyAttr = assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute> ()
        let version = assemblyAttr.InformationalVersion.ToString ()

        let scriptFile = args.ScriptFile |> Option.defaultValue "<script>.fsx"

        let usage =
            @$"
Usage: dotnet fsi {scriptFile} [pipeline] [options] [-- extra args]
Options:
  --help                                       Shows help and usage information
  -v, --verbosity <disabled|quiet|normal|all>  The verbosity level of FsMake's output [default: normal]
  -o, --console-output <standard|ansi>         The type of console output produced by FsMake"

        Console.Important
        |> Console.message "FsMake "
        |> Console.appendToken version
        |> writer.WriteLine

        if not errors.IsEmpty then
            Console.Error |> writer.WriteLine

            errors
            |> List.iter (ParseError.toConsoleMessage >> writer.WriteLine)

        Console.Important
        |> Console.message usage
        |> writer.WriteLine

    type ParserState =
        | NormalArgs
        | ExtraArgs

    let parseArgs (args: string array) : Result<Args, Args * ParseError list> =
        let args = args |> List.ofArray

        let rec parseNextArg remArgs errors idx state options =
            match (state, remArgs) with
            | (_, []) -> (options, errors)
            | (NormalArgs, "--help" :: _) -> ({ options with PrintHelp = true }, [])
            | (NormalArgs, "-v" :: xs)
            | (NormalArgs, "--verbosity" :: xs) ->
                match xs with
                | "disabled" :: xss ->
                    { options with Verbosity = Disabled }
                    |> parseNextArg xss errors (idx + 1) state
                | "quiet" :: xss ->
                    { options with Verbosity = Quiet }
                    |> parseNextArg xss errors (idx + 1) state
                | "normal" :: xss ->
                    { options with Verbosity = Normal }
                    |> parseNextArg xss errors (idx + 1) state
                | "all" :: xss ->
                    { options with Verbosity = All }
                    |> parseNextArg xss errors (idx + 1) state
                | x :: xss ->
                    options
                    |> parseNextArg xss (InvalidOptionParam ("-v, --verbosity", x) :: errors) (idx + 1) state
                | xss ->
                    options
                    |> parseNextArg xss (OptionParamMissing "-v, --verbosity" :: errors) (idx + 1) state
            | (NormalArgs, "-o" :: xs)
            | (NormalArgs, "--console-output" :: xs) ->
                match xs with
                | "ansi" :: xss ->
                    { options with ConsoleOutput = Ansi }
                    |> parseNextArg xss errors (idx + 1) state
                | "standard" :: xss ->
                    { options with
                        ConsoleOutput = Standard
                    }
                    |> parseNextArg xss errors (idx + 1) state
                | x :: xss ->
                    options
                    |> parseNextArg xss (InvalidOptionParam ("-o, --console-output", x) :: errors) (idx + 1) state
                | xss ->
                    options
                    |> parseNextArg xss (OptionParamMissing "-o, --console-output" :: errors) (idx + 1) state
            | (NormalArgs, "--" :: xs) -> options |> parseNextArg xs errors (idx + 1) ExtraArgs
            | (NormalArgs, x :: xs) ->
                match (idx, options.ScriptFile) with
                | (0, _) when x.EndsWith (".fsx") ->
                    { options with ScriptFile = Some x }
                    |> parseNextArg xs errors (idx + 1) state
                | (0, _)
                | (1, Some _) ->
                    { options with Pipeline = Some x }
                    |> parseNextArg xs errors (idx + 1) state
                | _ ->
                    options
                    |> parseNextArg xs (InvalidArgument x :: errors) (idx + 1) state
            | (ExtraArgs, x :: xs) ->
                { options with
                    ExtraArgs = options.ExtraArgs @ [ x ]
                }
                |> parseNextArg xs errors (idx + 1) state

        let (args, errors) =
            {
                PrintHelp = false
                ScriptFile = None
                Pipeline = None
                ConsoleOutput = Standard
                Verbosity = Normal
                ExtraArgs = []
            }
            |> parseNextArg args [] 0 NormalArgs

        if errors.IsEmpty then Ok args else Error (args, errors)

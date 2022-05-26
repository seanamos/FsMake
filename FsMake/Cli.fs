namespace FsMake

open System
open System.IO
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

    [<RequireQualifiedAccess>]
    type ExecutableType =
        | Fsx of string
        | Dll of string

    type ParsedArgs =
        {
            PrintHelp: bool
            Executable: ExecutableType option
            Pipeline: string option
            ConsoleOutput: ConsoleOutput
            Verbosity: Verbosity
            NoLogo: bool
            ExtraArgs: string list
        }

    let printLogo (writer: Console.IWriter) (level: Console.Level) =
        let assembly = Assembly.GetExecutingAssembly ()
        let assemblyAttr = assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute> ()
        let version = assemblyAttr.InformationalVersion.ToString ()

        let logo =
            "
█▀▀ █▀ █▀▄▀█ ▄▀█ █▄▀ █▀▀
█▀░ ▄█ █░▀░█ █▀█ █░█ ██▄ "

        level
        |> Console.message logo
        |> Console.appendToken version
        |> Console.append Environment.NewLine
        |> writer.WriteLine


    let printUsage (writer: Console.IWriter) (args: ParsedArgs) (errors: ParseError list) (pipelines: Pipeline list) : unit =
        let usageText =
            match args.Executable with
            | Some x ->
                match x with
                | ExecutableType.Fsx file -> $"dotnet fsi {file} --"
                | ExecutableType.Dll file ->
                    let file = file |> Path.GetFileName |> Path.GetFileNameWithoutExtension
                    $"dotnet run {file}"
            | _ -> "dotnet fsi <script>.fsx --"

        let pipelineText =
            if pipelines.Length > 0 then
                pipelines
                |> List.rev
                |> List.map (fun x ->
                    match x.Description with
                    | Some desc -> $"  {x.Name}{String (' ', 45 - x.Name.Length)}{desc}"
                    | None -> $"  {x.Name}"
                )
                |> String.concat Environment.NewLine
            else
                "  No pipelines"

        let helpText =
            $"Usage: {usageText} [pipeline] [options] [-- extra args]

Options:
  --help                                       Shows help and usage information
  -v, --verbosity <disabled|quiet|normal|all>  The verbosity level of FsMake's output [default: normal]
  -o, --console-output <standard|ansi>         The type of console output produced by FsMake
  -n, --no-logo                                Prevents the logo from being printed

Pipelines:
{pipelineText}"

        Console.Important |> printLogo writer

        if not errors.IsEmpty then
            Console.Error |> writer.WriteLine

            errors |> List.iter (ParseError.toConsoleMessage >> writer.WriteLine)

        Console.Important |> Console.message helpText |> writer.WriteLine

    type ParserState =
        | NormalArgs of leadingDashes: bool
        | ExtraArgs

    let parseArgs (args: string array) : Result<ParsedArgs, ParsedArgs * ParseError list> =
        let args = args |> List.ofArray

        let rec parseNextArg remArgs errors idx state options =
            match (state, remArgs) with
            | (_, []) -> (options, errors)
            | (NormalArgs _, "--help" :: _) -> ({ options with PrintHelp = true }, [])
            | (NormalArgs _, "-v" :: xs)
            | (NormalArgs _, "--verbosity" :: xs) ->
                match xs with
                | "disabled" :: xss -> { options with Verbosity = Disabled } |> parseNextArg xss errors (idx + 1) state
                | "quiet" :: xss -> { options with Verbosity = Quiet } |> parseNextArg xss errors (idx + 1) state
                | "normal" :: xss -> { options with Verbosity = Normal } |> parseNextArg xss errors (idx + 1) state
                | "all" :: xss -> { options with Verbosity = All } |> parseNextArg xss errors (idx + 1) state
                | x :: xss ->
                    options
                    |> parseNextArg xss (InvalidOptionParam ("-v, --verbosity", x) :: errors) (idx + 1) state
                | xss ->
                    options
                    |> parseNextArg xss (OptionParamMissing "-v, --verbosity" :: errors) (idx + 1) state
            | (NormalArgs _, "-o" :: xs)
            | (NormalArgs _, "--console-output" :: xs) ->
                match xs with
                | "ansi" :: xss -> { options with ConsoleOutput = Ansi } |> parseNextArg xss errors (idx + 1) state
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
            | (NormalArgs _, "-n" :: xs)
            | (NormalArgs _, "--no-logo" :: xs) -> { options with NoLogo = true } |> parseNextArg xs errors (idx + 1) state
            | (NormalArgs _, "--" :: xs) ->
                // there was a breaking change in .NET 6 with dotnet fsi parsing "--help" everywhere.
                // to work around this, "--" needs to be passed to dotnet fsi.
                // we need to expect the format `dotnet fsi build.fsx -- pipeline -o ansi -- extra_arg` (double --) now.
                match (idx, options.Executable) with
                | (1, Some (ExecutableType.Fsx _)) -> options |> parseNextArg xs errors (idx + 1) (NormalArgs true)
                | _ -> options |> parseNextArg xs errors (idx + 1) ExtraArgs
            | (NormalArgs leadingDashes, x :: xs) ->
                match (idx, options.Executable) with
                | (0, None) when x.EndsWith (".fsx") ->
                    { options with
                        Executable = Some (ExecutableType.Fsx x)
                    }
                    |> parseNextArg xs errors (idx + 1) state
                | (0, None) when x.EndsWith (".dll") ->
                    { options with
                        Executable = Some (ExecutableType.Dll x)
                    }
                    |> parseNextArg xs errors (idx + 1) state
                | (0, None) // no .dll or .fsx arg, fall through to next
                | (1, Some _) when not <| leadingDashes -> { options with Pipeline = Some x } |> parseNextArg xs errors (idx + 1) state
                | (2, Some _) when leadingDashes -> { options with Pipeline = Some x } |> parseNextArg xs errors (idx + 1) state
                | _ -> options |> parseNextArg xs (InvalidArgument x :: errors) (idx + 1) state
            | (ExtraArgs, x :: xs) ->
                { options with
                    ExtraArgs = options.ExtraArgs @ [ x ]
                }
                |> parseNextArg xs errors (idx + 1) state

        let (args, errors) =
            {
                PrintHelp = false
                Executable = None
                Pipeline = None
                ConsoleOutput = Standard
                Verbosity = Normal
                NoLogo = false
                ExtraArgs = []
            }
            |> parseNextArg args [] 0 (NormalArgs false)

        if errors.IsEmpty then Ok args else Error (args, errors)

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
        { PrintHelp: bool
          Pipeline: string option
          ConsoleOutput: ConsoleOutput
          Verbosity: Verbosity }

    let printUsage (writer: Console.IWriter) (errors: ParseError list) : unit =
        let assembly = Assembly.GetExecutingAssembly ()
        let assemblyAttr = assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute> ()
        let version = assemblyAttr.InformationalVersion.ToString ()

        let usage = @"
Usage: dotnet fsi <script>.fsx [pipeline] [options]

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
            errors |> List.iter (ParseError.toConsoleMessage >> writer.WriteLine)

        Console.Important |> Console.message usage |> writer.WriteLine

    let parseArgs (args: string array) : Result<Args, ParseError list> =
        let args = args |> List.ofArray

        let rec parseNextArg remArgs errors options =
            match remArgs with
            | [] -> (options, errors)
            | "--help" :: _ -> ({ options with PrintHelp = true }, [])
            | "-v" :: xs
            | "--verbosity" :: xs ->
                match xs with
                | "disabled" :: xss -> { options with Verbosity = Disabled } |> parseNextArg xss errors
                | "quiet" :: xss -> { options with Verbosity = Quiet } |> parseNextArg xss errors
                | "normal" :: xss -> { options with Verbosity = Normal } |> parseNextArg xss errors
                | "all" :: xss -> { options with Verbosity = All } |> parseNextArg xss errors
                | x :: xss ->
                    options
                    |> parseNextArg xss (InvalidOptionParam ("-v, --verbosity", x) :: errors)
                | xss -> options |> parseNextArg xss (OptionParamMissing "-v, --verbosity" :: errors)
            | "-o" :: xs
            | "--console-output" :: xs ->
                match xs with
                | "ansi" :: xss -> { options with ConsoleOutput = Ansi } |> parseNextArg xss errors
                | "standard" :: xss ->
                    { options with
                          ConsoleOutput = Standard }
                    |> parseNextArg xss errors
                | x :: xss ->
                    options
                    |> parseNextArg xss (InvalidOptionParam ("-o, --console-output", x) :: errors)
                | xss ->
                    options
                    |> parseNextArg xss (OptionParamMissing "-o, --console-output" :: errors)
            | x :: xs ->
                if args.[0] = x then
                    { options with Pipeline = Some x } |> parseNextArg xs errors
                else
                    options |> parseNextArg xs (InvalidArgument x :: errors)

        let (args, errors) =
            { PrintHelp = false
              Pipeline = None
              ConsoleOutput = Standard
              Verbosity = Normal }
            |> parseNextArg args []

        if errors.IsEmpty then Ok args else Error errors

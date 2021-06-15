namespace FsMake

open System
open System.Diagnostics
open System.Text

/// <summary>
/// Module for building and executing commands or processes.
/// </summary>
/// <example>
/// <code lang="fsharp">
/// do! Cmd.createWithArgs "dotnet" [ "build" ] |> Cmd.run
/// let! output = Cmd.createWithArgs "dotnet" [ "gitversion" ] |> Cmd.result
/// </code>
/// </example>
module Cmd =

    /// <summary>
    ///  Argument types.
    /// </summary>
    type Arg =
        /// <summary>
        /// Ordinary plain text argument
        /// </summary>
        /// <param name="arg">The argument.</param>
        | ArgText of arg: string

        /// <summary>
        /// Secret argument that should always be masked in output
        /// </summary>
        /// <param name="arg">The argument.</param>
        | ArgSecret of arg: string

    module internal Arg =
        let toUnsafeStrList (args: Arg list) : string list =
            args
            |> List.map
                (function
                | ArgText x -> x
                | ArgSecret x -> x)

        let toSafeStrList (args: Arg list) : string list =
            args
            |> List.map
                (function
                | ArgText x -> x
                | ArgSecret _ -> "**masked**")

    /// <summary>
    /// Prefix options, sets when to prefix the output with the step name.
    /// </summary>
    type PrefixOption =
        /// Always prefix the output
        | PrefixAlways
        /// Never prefix the output
        | PrefixNever
        /// **Default**. Use the pipeline prefix setting.
        | PrefixPipeline

    /// <summary>
    /// Output redirection options.
    /// </summary>
    type RedirectOption =
        /// <summary>
        /// Redirect the process output.
        /// This allows you to capture the output with <see cref="M:Cmd.result" />, but does not print the output the console.
        /// </summary>
        | Redirect
        /// <summary>
        /// Redirects the process output and prints to the console.
        /// This allows you to capture the output with <see cref="M:Cmd.result" /> and prints the output to the console.
        /// </summary>
        | RedirectToBoth

    /// <summary>
    /// Type representing the arguments passed to an output processor function.
    /// </summary>
    type OutputProcessorArgs = OutputProcessorArgs of exitCode: int * std: string * stdErr: string

    /// <summary>
    /// Contains the redirected std and stderr output of a process.
    /// </summary>
    type RedirectedOutput = { Std: string; StdErr: string }

    /// <summary>
    /// Contains the exit code and output of a process (if redirected).
    /// </summary>
    type ProcessResult<'a> = { ExitCode: int; Output: 'a }

    /// <summary>
    /// Process exit code check options.
    /// </summary>
    type ExitCodeCheckOption =
        /// <summary>
        /// Does not check the exit code.
        /// </summary>
        | CheckCodeNone
        /// <summary>
        /// **Default**. Checks that the exit code is <c>0</c>.
        /// </summary>
        | CheckCodeZero
        /// <summary>
        /// Checks that the exit code is <c>0</c> and prints the specified message when it is not.
        /// </summary>
        /// <param name="message">The message to print when the exit code is not <c>0</c>.</param>
        | CheckCodeZeroWithMessage of message: string
        /// <summary>
        /// Checks that the exit code is equal to the specified exit code.
        /// </summary>
        /// <param name="code">The exit code to check for.</param>
        | CheckCode of code: int
        /// <summary>
        /// Checks that the exit code is equal to the specified exit code and prints the specified message when it is not.
        /// </summary>
        /// <param name="code">The exit code to check for.</param>
        /// <param name="message">The message to print when not exit code is not matched.</param>
        | CheckCodeWithMessage of code: int * message: string

    /// <summary>
    /// Contains all the options used to run a process.
    /// </summary>
    type CmdOptions<'a> =
        { Command: string
          Args: Arg list
          EnvVars: (string * string) list
          WorkingDirectory: string option
          Timeout: TimeSpan option
          Prefix: PrefixOption
          ExitCodeCheck: ExitCodeCheckOption
          Redirect: RedirectOption option
          OutputProcessor: OutputProcessorArgs -> ProcessResult<'a> }

    /// <summary>
    /// Creates a <see cref="T:Cmd.CmdOption" /> with a command to run.
    /// </summary>
    /// <param name="cmd">The command to run.</param>
    /// <returns>The new <see cref="T:Cmd.CmdOption" />.</returns>
    let create (cmd: string) : CmdOptions<unit> =
        { Command = cmd
          Args = []
          EnvVars = []
          WorkingDirectory = None
          Timeout = None
          Prefix = PrefixPipeline
          ExitCodeCheck = CheckCodeZero
          Redirect = None
          OutputProcessor = fun (OutputProcessorArgs (exitCode, _, _)) -> { ExitCode = exitCode; Output = () } }

    /// <summary>
    /// Creates a <see cref="T:Cmd.CmdOption" /> with a command to run.
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// let cmd = Cmd.createWithArgs "dotnet" [ "build"; "--no-restore" ]
    /// </code>
    /// </example>
    /// <param name="cmd">The command to run.</param>
    /// <param name="args">A list of arguments to be passed to the command.</param>
    /// <returns>The new <see cref="T:Cmd.CmdOption" />.</returns>
    let createWithArgs (cmd: string) (args: string list) : CmdOptions<unit> =
        { create cmd with
              Args = args |> List.map ArgText }

    /// <summary>
    /// **Appends** the given arguments to the <see cref="T:Cmd.CmdOption" />.
    /// </summary>
    /// <param name="args">The arguments to append.</param>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" /> to append to.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The updated <see cref="T:Cmd.CmdOption" />.</returns>
    let args (args: string list) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with
              Args = opts.Args @ (args |> List.map ArgText) }

    /// <summary>
    /// **Appends** the given arguments to the <see cref="T:Cmd.CmdOption" /> if the condition is <c>true</c>.
    /// </summary>
    /// <param name="cond">The condition.</param>
    /// <param name="args">The arguments to append.</param>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" /> to append to.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The updated <see cref="T:Cmd.CmdOption" />.</returns>
    let argsMaybe (cond: bool) (args: string list) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        if cond then
            { opts with
                  Args = opts.Args @ (args |> List.map ArgText) }
        else
            opts

    /// <summary>
    /// **Appends** the given argument to the <see cref="T:Cmd.CmdOption" /> if the condition is <c>true</c>.
    /// </summary>
    /// <param name="cond">The condition.</param>
    /// <param name="arg">The argument to append.</param>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" /> to append to.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The updated <see cref="T:Cmd.CmdOption" />.</returns>
    let argMaybe (cond: bool) (arg: string) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        opts |> argsMaybe cond [ arg ]

    /// <summary>
    /// **Appends** the given arguments to the <see cref="T:Cmd.CmdOption" /> if the option <c>Some</c>.
    /// </summary>
    /// <param name="args'">The arguments to append.</param>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" /> to append to.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The updated <see cref="T:Cmd.CmdOption" />.</returns>
    let argsOption (args': string list option) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        match args' with
        | Some x -> opts |> args x
        | None -> opts

    /// <summary>
    /// **Appends** the given argument to the <see cref="T:Cmd.CmdOption" /> if the option <c>Some</c>.
    /// </summary>
    /// <param name="arg">The argument to append.</param>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" /> to append to.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The updated <see cref="T:Cmd.CmdOption" />.</returns>
    let argOption (arg: string option) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        opts |> argsOption (arg |> Option.map (fun x -> [ x ]))

    /// <summary>
    /// **Appends** the given *secret* argument to the <see cref="T:Cmd.CmdOption" />.
    /// Secret arguments are masked in console output.
    /// </summary>
    /// <param name="arg">The argument to append.</param>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" /> to append to.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The updated <see cref="T:Cmd.CmdOption" />.</returns>
    let argSecret (arg: string) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with
              Args = opts.Args @ [ ArgSecret arg ] }

    /// <summary>
    /// **Appends** the given environment variables to the <see cref="T:Cmd.CmdOption" />.
    /// </summary>
    /// <param name="envVars">A <c>key * value</c> tuple list of environment variables to append.</param>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" /> to append to.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The updated <see cref="T:Cmd.CmdOption" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// Cmd.createWithArgs "docker-compose" [ "build" ]
    /// |> Cmd.envVars [ ("DEBUG", "0"); ("TAG", "1.0.0") ]
    /// </code>
    /// </example>
    let envVars (envVars: (string * string) list) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with EnvVars = opts.EnvVars @ envVars }

    /// <summary>
    /// Sets the working directory on the <see cref="T:Cmd.CmdOption" />.
    /// </summary>
    /// <param name="path">The path of the working directory.</param>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" />.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The updated <see cref="T:Cmd.CmdOption" />.</returns>
    let workingDir (path: string) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with
              WorkingDirectory = Some path }

    /// <summary>
    /// Sets the prefix option on the <see cref="T:Cmd.CmdOption" />.
    /// This sets if the console output will be prefixed with the step name.
    /// </summary>
    /// <param name="prefix">The prefix option.</param>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" />.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The updated <see cref="T:Cmd.CmdOption" />.</returns>
    let prefix (prefix: PrefixOption) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with Prefix = prefix }

    /// <summary>
    /// Sets the timeout option on the <see cref="T:Cmd.CmdOption" />.
    /// This is the time in seconds before the command times out and is terminated.
    /// The timeout being reached causes a step failure.
    /// </summary>
    /// <param name="seconds">The timeout in seconds.</param>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" />.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The updated <see cref="T:Cmd.CmdOption" />.</returns>
    let timeout (seconds: int) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with
              Timeout = Some (TimeSpan.FromSeconds (seconds |> float)) }

    /// <summary>
    /// Sets the redirect option on the <see cref="T:Cmd.CmdOptions" />.
    /// This controls how the output of the process should be redirected.
    /// Check the options in <see cref="T:Cmd.RedirectOption" /> for more information on each option.
    /// </summary>
    /// <param name="redirect">The redirect option.</param>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" />.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The updated <see cref="T:Cmd.CmdOption" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let! output =
    ///     Cmd.createWithArgs "dotnet" [ "build" ]
    ///     |> Cmd.redirectOutput Cmd.RedirectToBoth
    ///     |> Cmd.result
    /// </code>
    /// </example>
    let redirectOutput (redirect: RedirectOption) (opts: CmdOptions<'a>) : CmdOptions<RedirectedOutput> =
        { Command = opts.Command
          Args = opts.Args
          EnvVars = opts.EnvVars
          WorkingDirectory = opts.WorkingDirectory
          Timeout = opts.Timeout
          Prefix = opts.Prefix
          ExitCodeCheck = opts.ExitCodeCheck
          Redirect = Some redirect
          OutputProcessor =
              fun (OutputProcessorArgs (exitCode, std, stdErr)) ->
                  { ExitCode = exitCode
                    Output = { Std = std; StdErr = stdErr } } }

    /// <summary>
    /// Sets the check exit code option on the <see cref="T:Cmd.CmdOptions" />.
    /// This sets if the exit code returned by the process should be checked.
    /// Check the options in <see cref="T:Cmd.ExitCodeCheckOption" /> for more information on each option.
    /// The default is <see cref="T:Cmd.ExitCodeCheckOption.CheckCodeZero" />.
    /// </summary>
    /// <param name="check">The exit code check option.</param>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" />.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The updated <see cref="T:Cmd.CmdOption" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let! output =
    ///     Cmd.createWithArgs "dotnet" [ "build" ]
    ///     |> Cmd.checkExitCode Cmd.CheckCodeNone
    ///     |> Cmd.result
    /// </code>
    /// </example>
    let checkExitCode (check: ExitCodeCheckOption) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with ExitCodeCheck = check }

    [<AutoOpen>]
    module internal Internal =
        let prettyCommand (command: string) (args: string list) =
            if args.Length = 0 then
                command
            else
                let sb = StringBuilder ()
                sb.Append command |> ignore
                sb.Append " [ " |> ignore

                args
                |> List.iteri (fun i x ->
                    if i <> 0 then sb.Append ' ' |> ignore

                    sb.Append '"' |> ignore
                    sb.Append x |> ignore
                    sb.Append '"' |> ignore

                    if i <> args.Length - 1 then sb.Append ';' |> ignore
                )

                sb.Append " ]" |> ignore
                sb.ToString ()

        type RedirectDecision =
            | ToConsole
            | ToProcess
            | ToBoth
            | NoRedirect

        let getRedirectDecision (shouldPrefix: bool) (redirect: RedirectOption option) : RedirectDecision =
            match redirect with
            | Some x ->
                match x with
                | Redirect -> ToProcess
                | RedirectToBoth -> ToBoth
            | None when shouldPrefix -> ToConsole
            | None -> NoRedirect

        let createProcessStartInfo (redirectDecision: RedirectDecision) (opts: CmdOptions<'a>) : ProcessStartInfo =
            let startInfo = ProcessStartInfo (opts.Command)

            opts.Args
            |> Arg.toUnsafeStrList
            |> List.iter (fun x -> startInfo.ArgumentList.Add (x))

            startInfo.UseShellExecute <- false
            startInfo.CreateNoWindow <- true

            match redirectDecision with
            | NoRedirect -> ()
            | _ ->
                startInfo.RedirectStandardOutput <- true
                startInfo.RedirectStandardError <- true

            opts.EnvVars
            |> List.iter (fun (key, value) -> startInfo.EnvironmentVariables.[key] <- value)

            match opts.WorkingDirectory with
            | Some x -> startInfo.WorkingDirectory <- x
            | _ -> ()

            startInfo

        type WriteOutputOptions =
            | WithPrefix of prefix: Console.TextPart
            | NoPrefix

        let writeOutput (opt: WriteOutputOptions) (console: Console.IWriter) (msgOpts: Console.Message -> Console.Message) : unit =
            match opt with
            | WithPrefix prefix ->
                Console.Info
                |> Console.messageEmpty Console.infoColor
                |> Console.prefix prefix
                |> msgOpts
                |> console.WriteLine
            | NoPrefix ->
                Console.Info
                |> Console.messageEmpty Console.infoColor
                |> msgOpts
                |> console.WriteLine

        type ExitCodeDecision =
            | UnexpectedExitCode of msg: Console.Message
            | ExpectedExitCode

        let exitCodeDecision (exitCodeCheck: ExitCodeCheckOption) (fullCommand: string) (exitCode: int) : ExitCodeDecision =
            let checkCode expected message =
                if exitCode <> expected then
                    UnexpectedExitCode message
                else
                    ExpectedExitCode

            match exitCodeCheck with
            | CheckCodeNone -> ExpectedExitCode
            | CheckCodeZero ->
                Console.error ""
                |> Console.appendToken fullCommand
                |> Console.append " failed with "
                |> Console.appendToken (exitCode.ToString ())
                |> Console.append " exit code"
                |> checkCode 0
            | CheckCodeZeroWithMessage msg -> Console.error msg |> checkCode 0
            | CheckCode code ->
                Console.error ""
                |> Console.appendToken fullCommand
                |> Console.append " failed with "
                |> Console.appendToken (exitCode.ToString ())
                |> Console.append " exit code, expected "
                |> Console.appendToken (code.ToString ())
                |> checkCode code
            | CheckCodeWithMessage (code, msg) -> Console.error msg |> checkCode code

    /// <summary>
    /// Runs a command/process with the specified options and returns a <see cref="T:Cmd.ProcessResult" />.
    /// The <see cref="T:Cmd.ProcessResult" /> contains the exit code and optionally the redirected output if a redirect option was set.
    /// This method cannot be used if the output has not been redirected.
    /// </summary>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" />.</param>
    /// <param name="ctx">The <see cref="T:StepContext" /> of the current step.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The result.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let myStep =
    ///     Step.create "myStep" {
    ///         let! semverResult =
    ///             Cmd.createWithArgs "dotnet" [ "gitversion"; "/showvariable"; "semver" ]
    ///             |> Cmd.redirectOutput Cmd.Redirect
    ///             |> Cmd.result
    ///
    ///         let semver = semverResult.Output.Std
    ///     }
    /// </code>
    /// </example>
    let result (opts: CmdOptions<'a>) : StepPart<ProcessResult<'a>> =
        fun (ctx: StepContext) ->
            let shouldPrefix =
                match opts.Prefix with
                | PrefixNever -> false
                | PrefixAlways -> true
                | PrefixPipeline -> Prefix.Internal.shouldPrefix ctx.IsParallel ctx.PrefixOption

            let redirectDecision = opts.Redirect |> getRedirectDecision shouldPrefix
            let startInfo = opts |> createProcessStartInfo redirectDecision
            use proc = new Process ()
            proc.StartInfo <- startInfo

            let fullCommand = prettyCommand opts.Command (opts.Args |> Arg.toSafeStrList)

            let writeOutputOpts =
                match shouldPrefix with
                | true -> WithPrefix ctx.Prefix
                | _ -> NoPrefix

            let writeOutput = writeOutput writeOutputOpts ctx.Console

            let cmdText = sprintf "> %s" fullCommand
            writeOutput (fun x -> x |> Console.appendColor Console.infoColor cmdText)

            proc.Start () |> ignore

            try
                ctx.ProcessMonitor |> ProcessMonitor.add proc

                let stdBuilder = StringBuilder ()
                let stdErrBuilder = StringBuilder ()

                let evNotNullThen action (ev: DataReceivedEventArgs) =
                    if not <| isNull ev.Data then action ev.Data

                let addOutputConsoleWriters (process': Process) =
                    process'.OutputDataReceived.Add (evNotNullThen (Console.append >> writeOutput))
                    process'.ErrorDataReceived.Add (evNotNullThen (Console.append >> writeOutput))
                    process'

                let strBuilderAppend (sb: StringBuilder) (line: string) =
                    if sb.Length = 0 then
                        sb.Append line |> ignore
                    else
                        sb.Append (Environment.NewLine + line) |> ignore

                let addOutputBuilderWriters (process': Process) =
                    process'.OutputDataReceived.Add (evNotNullThen (strBuilderAppend stdBuilder))
                    process'.ErrorDataReceived.Add (evNotNullThen (strBuilderAppend stdErrBuilder))
                    process'

                let beginDataRead (process': Process) =
                    process'.BeginOutputReadLine ()
                    process'.BeginErrorReadLine ()

                match redirectDecision with
                | ToConsole -> proc |> addOutputConsoleWriters |> beginDataRead
                | ToProcess -> proc |> addOutputBuilderWriters |> beginDataRead
                | ToBoth -> proc |> (addOutputConsoleWriters >> addOutputBuilderWriters) |> beginDataRead
                | NoRedirect -> ()

                let processCompleted =
                    match opts.Timeout with
                    | Some x -> proc.WaitForExit (x.TotalMilliseconds |> int)
                    | None ->
                        proc.WaitForExit ()
                        true

                if not processCompleted then
                    ctx.ProcessMonitor |> ProcessMonitor.kill proc

                    [ Console.error ""
                      |> Console.appendToken fullCommand
                      |> Console.append " failed to complete before timeout expired" ]
                    |> StepError
                    |> Error
                else if ctx.ProcessMonitor |> ProcessMonitor.isKilled proc then
                    [ Console.error ""
                      |> Console.appendToken fullCommand
                      |> Console.append " was aborted" ]
                    |> StepAbort
                    |> Error
                else
                    let exitCode = proc.ExitCode

                    let exitCodeDecision = exitCode |> exitCodeDecision opts.ExitCodeCheck fullCommand

                    match exitCodeDecision with
                    | UnexpectedExitCode x -> StepError [ x ] |> Error
                    | ExpectedExitCode ->
                        OutputProcessorArgs (exitCode, stdBuilder.ToString (), stdErrBuilder.ToString ())
                        |> opts.OutputProcessor
                        |> Ok
            finally
                ctx.ProcessMonitor |> ProcessMonitor.remove proc

    /// <summary>
    /// Runs a command/process with the specified options.
    /// </summary>
    /// <param name="opts">The <see cref="T:Cmd.CmdOption" />.</param>
    /// <param name="ctx">The <see cref="T:StepContext" /> of the current step.</param>
    /// <typeparam name="'a">The command output type.</typeparam>
    /// <returns>The result.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let build =
    ///     Step.create "build" {
    ///         do! Cmd.createWithArgs "dotnet" [ "build" ] |> Cmd.run
    ///     }
    /// </code>
    /// </example>
    let run (opts: CmdOptions<'a>) : StepPart<unit> =
        fun (ctx: StepContext) ->
            let procResult = result opts ctx

            match procResult with
            | Ok _ -> Ok ()
            | Error x -> Error x

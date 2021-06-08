namespace FsMake

open System
open System.Diagnostics
open System.Runtime.InteropServices
open System.Text

module Cmd =
    type PrefixOption =
        | PrefixAlways
        | PrefixNever
        | PrefixWhenParallel

    type RedirectOption =
        | Redirect
        | RedirectToBoth

    type OutputProcessorArgs = OutputProcessorArgs of exitCode: int * std: string * stdErr: string
    type RedirectedOutput = { Std: string; StdErr: string }
    type ProcessResult<'a> = { ExitCode: int; Output: 'a }

    type ExitCodeCheckOption =
        | CheckCodeZero
        | CheckCodeZeroWithMessage of message: string
        | CheckCode of code: int
        | CheckCodeWithMessage of code: int * message: string

    type CmdOptions<'a> =
        { Command: string
          Args: string list
          EnvVars: (string * string) list
          WorkingDirectory: string option
          UseMono: bool
          Timeout: TimeSpan option
          Prefix: PrefixOption
          ExitCodeCheck: ExitCodeCheckOption option
          Redirect: RedirectOption option
          OutputProcessor: OutputProcessorArgs -> ProcessResult<'a> }

    let create (cmd: string) : CmdOptions<unit> =
        { Command = cmd
          Args = []
          EnvVars = []
          WorkingDirectory = None
          UseMono = false
          Timeout = None
          Prefix = PrefixWhenParallel
          ExitCodeCheck = Some CheckCodeZero
          Redirect = None
          OutputProcessor = fun (OutputProcessorArgs (exitCode, _, _)) -> { ExitCode = exitCode; Output = () } }

    let createWithArgs (cmd: string) (args: string list) : CmdOptions<unit> =
        { create cmd with Args = args }

    let args (args: string list) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with Args = args }

    let argsMaybe (cond: bool) (args: string list) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        if cond then { opts with Args = opts.Args @ args } else opts

    let argMaybe (cond: bool) (arg: string) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        opts |> argsMaybe cond [ arg ]

    let argsOption (arg: string list option) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        match arg with
        | Some x -> { opts with Args = opts.Args @ x }
        | None -> opts

    let argOption (arg: string option) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        opts |> argsOption (arg |> Option.map (fun x -> [ x ]))

    let envVars (envVars: (string * string) list) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with EnvVars = envVars }

    let workingDir (path: string) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with
              WorkingDirectory = Some path }

    let useMono (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with UseMono = true }

    let prefix (prefix: PrefixOption) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with Prefix = prefix }

    let timeout (seconds: int) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with
              Timeout = Some (TimeSpan.FromSeconds (seconds |> float)) }

    let redirectOutput (redirect: RedirectOption) (opts: CmdOptions<'a>) : CmdOptions<RedirectedOutput> =
        { Command = opts.Command
          Args = opts.Args
          EnvVars = opts.EnvVars
          WorkingDirectory = opts.WorkingDirectory
          UseMono = opts.UseMono
          Timeout = opts.Timeout
          Prefix = opts.Prefix
          ExitCodeCheck = opts.ExitCodeCheck
          Redirect = Some redirect
          OutputProcessor =
              fun (OutputProcessorArgs (exitCode, std, stdErr)) ->
                  { ExitCode = exitCode
                    Output = { Std = std; StdErr = stdErr } } }

    let checkExitCode (check: ExitCodeCheckOption) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with ExitCodeCheck = Some check }

    [<AutoOpen>]
    module internal Internal =
        let prefixColors =
            [ Console.Cyan
              Console.DarkCyan
              Console.DarkYellow
              Console.Green
              Console.Magenta
              Console.Yellow ]

        let createCmdArgs (isWindows: bool) (opts: CmdOptions<'a>) =
            if opts.UseMono && (isWindows |> not) then
                ("mono", opts.Command :: opts.Args)
            else
                (opts.Command, opts.Args)

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

        let createProcessStartInfo (isWindows: bool) (shouldPrefix: bool) (opts: CmdOptions<'a>) : ProcessStartInfo =
            let (command, args) = opts |> createCmdArgs isWindows

            let startInfo = ProcessStartInfo (command)
            args |> List.iter (fun x -> startInfo.ArgumentList.Add (x))

            startInfo.UseShellExecute <- false
            startInfo.CreateNoWindow <- true

            match opts.Redirect with
            | Some _
            | None when shouldPrefix ->
                startInfo.RedirectStandardOutput <- true
                startInfo.RedirectStandardError <- true
            | _ -> ()

            opts.EnvVars
            |> List.iter (fun (key, value) -> startInfo.EnvironmentVariables.[key] <- value)

            match opts.WorkingDirectory with
            | Some x -> startInfo.WorkingDirectory <- x
            | _ -> ()

            startInfo

        type WriteOutputOptions =
            | WithPrefix of color: Console.Color * prefix: string
            | NoPrefix

        let writeOutput (opt: WriteOutputOptions) (console: Console.IWriter) (msgOpts: Console.Message -> Console.Message) : unit =
            match opt with
            | WithPrefix (color, prefix) ->
                Console.Info
                |> Console.messageColor color prefix
                |> msgOpts
                |> console.WriteLine
            | NoPrefix ->
                Console.Info
                |> Console.messageEmpty Console.infoColor
                |> msgOpts
                |> console.WriteLine

        let createOutputPrefix (stepMaxLength: int) (stepName: string) : (Console.Color * string) =
            let prefix = sprintf "%-*s | " stepMaxLength stepName
            let prefixColorIdx = Math.Abs (stepName.GetHashCode ()) % prefixColors.Length
            let prefixColor = prefixColors.[prefixColorIdx]
            (prefixColor, prefix)

        type RedirectDecision =
            | ToConsole
            | ToProcess
            | ToBoth
            | NoRedirect

        let redirectDecision (shouldPrefix: bool) (redirect: RedirectOption option) : RedirectDecision =
            match redirect with
            | Some x ->
                match x with
                | Redirect -> ToProcess
                | RedirectToBoth -> ToBoth
            | None when shouldPrefix -> ToConsole
            | None -> NoRedirect

        type ExitCodeDecision =
            | UnexpectedExitCode of msg: Console.Message
            | ExpectedExitCode

        let exitCodeDecision (exitCodeCheck: ExitCodeCheckOption option) (fullCommand: string) (exitCode: int) : ExitCodeDecision =
            match exitCodeCheck with
            | Some x ->
                let checkCode expected message =
                    if exitCode <> expected then
                        UnexpectedExitCode message
                    else
                        ExpectedExitCode

                match x with
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
            | None -> ExpectedExitCode

    let runAndGetResult (opts: CmdOptions<'a>) : StepPart<ProcessResult<'a>> =
        fun (ctx: StepContext) ->
            let shouldPrefix =
                match opts.Prefix with
                | PrefixNever -> false
                | PrefixAlways -> true
                | PrefixWhenParallel -> ctx.IsParallel

            let isWindows = RuntimeInformation.IsOSPlatform (OSPlatform.Windows)

            let startInfo = opts |> createProcessStartInfo isWindows shouldPrefix
            use proc = new Process ()
            proc.StartInfo <- startInfo

            let (command, args) = (startInfo.FileName, startInfo.ArgumentList :> string seq |> List.ofSeq)
            let (prefixColor, prefix) = ctx.StepName |> createOutputPrefix ctx.LongestStepNameLength
            let fullCommand = prettyCommand command args

            let writeOutputOpts =
                match shouldPrefix with
                | true -> WithPrefix (prefixColor, prefix)
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

                let addOutputBuilderWriters (process': Process) =
                    process'.OutputDataReceived.Add (evNotNullThen (stdBuilder.AppendLine >> ignore))
                    process'.ErrorDataReceived.Add (evNotNullThen (stdErrBuilder.AppendLine >> ignore))
                    process'

                let beginDataRead (process': Process) =
                    process'.BeginOutputReadLine ()
                    process'.BeginErrorReadLine ()

                let redirectDecision = opts.Redirect |> redirectDecision shouldPrefix

                match redirectDecision with
                | ToConsole -> proc |> addOutputConsoleWriters |> beginDataRead
                | ToProcess -> proc |> addOutputBuilderWriters |> beginDataRead
                | ToBoth -> proc |> (addOutputConsoleWriters >> addOutputConsoleWriters) |> beginDataRead
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
                    |> StepUserAbort
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

    let run (opts: CmdOptions<'a>) : StepPart<unit> =
        fun (ctx: StepContext) ->
            let result = runAndGetResult opts ctx

            match result with
            | Ok _ -> Ok ()
            | Error x -> Error x

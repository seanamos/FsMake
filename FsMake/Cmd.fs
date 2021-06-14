namespace FsMake

open System
open System.Diagnostics
open System.Text

module Cmd =
    type Arg =
        | ArgText of arg: string
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
                | ArgSecret _ -> "****")

    type PrefixOption =
        | PrefixAlways
        | PrefixNever
        | PrefixPipeline

    type RedirectOption =
        | Redirect
        | RedirectToBoth

    type OutputProcessorArgs = OutputProcessorArgs of exitCode: int * std: string * stdErr: string
    type RedirectedOutput = { Std: string; StdErr: string }
    type ProcessResult<'a> = { ExitCode: int; Output: 'a }

    type ExitCodeCheckOption =
        | CheckCodeNone
        | CheckCodeZero
        | CheckCodeZeroWithMessage of message: string
        | CheckCode of code: int
        | CheckCodeWithMessage of code: int * message: string

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

    let createWithArgs (cmd: string) (args: string list) : CmdOptions<unit> =
        { create cmd with
              Args = args |> List.map ArgText }

    let args (args: string list) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with
              Args = opts.Args @ (args |> List.map ArgText) }

    let argsMaybe (cond: bool) (args: string list) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        if cond then
            { opts with
                  Args = opts.Args @ (args |> List.map ArgText) }
        else
            opts

    let argMaybe (cond: bool) (arg: string) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        opts |> argsMaybe cond [ arg ]

    let argsOption (arg: string list option) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        match arg with
        | Some x -> opts |> args x
        | None -> opts

    let argOption (arg: string option) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        opts |> argsOption (arg |> Option.map (fun x -> [ x ]))

    let argSecret (arg: string) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with
              Args = opts.Args @ [ ArgSecret arg ] }

    let envVars (envVars: (string * string) list) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with EnvVars = envVars }

    let workingDir (path: string) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with
              WorkingDirectory = Some path }

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
          Timeout = opts.Timeout
          Prefix = opts.Prefix
          ExitCodeCheck = opts.ExitCodeCheck
          Redirect = Some redirect
          OutputProcessor =
              fun (OutputProcessorArgs (exitCode, std, stdErr)) ->
                  { ExitCode = exitCode
                    Output = { Std = std; StdErr = stdErr } } }

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

    let run (opts: CmdOptions<'a>) : StepPart<unit> =
        fun (ctx: StepContext) ->
            let procResult = result opts ctx

            match procResult with
            | Ok _ -> Ok ()
            | Error x -> Error x

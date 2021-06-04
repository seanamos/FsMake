namespace FsMake

open System
open System.Diagnostics
open System.Runtime.InteropServices
open System.Text

module Cmd =
    type RedirectOption =
        | Redirect
        | RedirectToBoth

    type OutputProcessorArgs = OutputProcessorArgs of exitCode: int * std: string * stdErr: string
    type RedirectedOutput = { Std: string; StdErr: string }
    type ProcessResult<'a> = { ExitCode: int; Output: 'a }

    type ExitCodeCheckOption =
        | Zero
        | ZeroWithMessage of message: string
        | Code of code: int
        | CodeWithMessage of code: int * message: string

    type CmdOptions<'a> =
        { Command: string
          Args: string list
          EnvVars: (string * string) list
          WorkingDirectory: string option
          UseMono: bool
          Timeout: TimeSpan option
          Prefix: bool
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
          Prefix = true
          ExitCodeCheck = None
          Redirect = None
          OutputProcessor = fun (OutputProcessorArgs (exitCode, _, _)) -> { ExitCode = exitCode; Output = () } }

    let createWithArgs (args: string list) (cmd: string) : CmdOptions<unit> =
        { create cmd with Args = args }

    let args (args: string list) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with Args = args }

    let argsMaybe (cond: bool) (args: string list) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        if cond then { opts with Args = opts.Args @ args } else opts

    let argMaybe (cond: bool) (arg: string) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        opts |> argsMaybe cond [ arg ]

    let envVars (envVars: (string * string) list) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with EnvVars = envVars }

    let workingDir (path: string option) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with WorkingDirectory = path }

    let mono (useMono: bool) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with UseMono = useMono }

    let prefix (usePrefix: bool) (opts: CmdOptions<'a>) : CmdOptions<'a> =
        { opts with Prefix = usePrefix }

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
            if opts.UseMono && isWindows |> not then
                ("mono", opts.Command :: opts.Args)
            else
                (opts.Command, opts.Args)

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
            | UnexpectedExitCode of msg: string
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
                | Zero -> sprintf "\"%s\" failed with %i exit code" fullCommand exitCode |> checkCode 0
                | ZeroWithMessage msg -> msg |> checkCode 0
                | Code code ->
                    sprintf "\"%s\" failed with %i exit code, expected %i" fullCommand exitCode code
                    |> checkCode code
                | CodeWithMessage (code, msg) -> checkCode code msg
            | None -> ExpectedExitCode

    let run (ctx: StepContext) (opts: CmdOptions<'a>) : ProcessResult<'a> =
        let shouldPrefix = ctx.IsParallel && opts.Prefix
        let isWindows = RuntimeInformation.IsOSPlatform (OSPlatform.Windows)

        let startInfo = opts |> createProcessStartInfo isWindows shouldPrefix
        use proc = new Process ()
        proc.StartInfo <- startInfo

        let (command, args) = (startInfo.FileName, startInfo.ArgumentList |> Seq.map id |> List.ofSeq)
        let fullCommand = command :: args |> String.concat " "

        let (prefixColor, prefix) = ctx.StepName |> createOutputPrefix ctx.LongestStepNameLength

        let writeOutputOpts =
            match shouldPrefix with
            | true -> WithPrefix (prefixColor, prefix)
            | _ -> NoPrefix

        let writeOutput = writeOutput writeOutputOpts ctx.Console

        let cmdText = sprintf "> %s" fullCommand
        writeOutput (fun x -> x |> Console.appendColor Console.infoColor cmdText)

        proc.Start () |> ignore
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
            failwithf "\"%s\" failed to complete before timeout expired" fullCommand

        if ctx.ProcessMonitor |> ProcessMonitor.isKilled proc then
            failwithf "\"%s\" was aborted" fullCommand

        let exitCode = proc.ExitCode

        let exitCodeDecision = exitCode |> exitCodeDecision opts.ExitCodeCheck fullCommand

        match exitCodeDecision with
        | UnexpectedExitCode x -> failwith x
        | _ -> ()

        OutputProcessorArgs (exitCode, stdBuilder.ToString (), stdErrBuilder.ToString ())
        |> opts.OutputProcessor

namespace FsMake

open System
open System.Diagnostics

/// <summary>
/// Module for working with the <see cref="T:ProcessMonitor.Agent" />
/// This is used to track external processes launched by a pipeline.
/// </summary>
module ProcessMonitor =
    [<AutoOpen>]
    module Internal =
        type Message =
            | Add of proc: Process * reply: AsyncReplyChannel<unit>
            | Remove of proc: Process * reply: AsyncReplyChannel<unit>
            | Kill of proc: Process * reply: AsyncReplyChannel<unit>
            | IsKilled of proc: Process * reply: AsyncReplyChannel<bool>
            | KillAll of reply: AsyncReplyChannel<unit>
            | Shutdown of reply: AsyncReplyChannel<unit>

        type State =
            { Processes: (int * Process) list
              Killed: int list }

        let killProcess (console: Console.IWriter) (proc: Process) : bool =
            if not proc.HasExited then
                try
                    proc.Kill ()
                with ex ->
                    Console.warn "Failed to kill process "
                    |> Console.appendParts [ proc.Id.ToString () |> Console.Token
                                             $". Exception: {Environment.NewLine}" |> Console.Text
                                             ex.ToString () |> Console.Token ]
                    |> console.WriteLine

                true
            else
                false

    /// <summary>
    /// The ProcessMonitor agent (<see cref="Microsoft.FSharp.Control.FSharpMailboxProcessor" />) that tracks external processes
    /// launched by a pipeline.
    /// </summary>
    /// <param name="console">The <see cref="Console.IWriter" /> to use.</param>
    type Agent(console: Console.IWriter) =
        let mailbox =
            MailboxProcessor.Start
            <| fun inbox ->
                let rec receiveNext state =
                    async {
                        let! msg = inbox.Receive ()

                        match msg with
                        | Add (proc, replyChannel) ->
                            if not proc.HasExited then
                                let newState =
                                    { state with
                                          Processes = (proc.Id, proc) :: state.Processes }

                                replyChannel.Reply ()

                                return! newState |> receiveNext
                            else
                                replyChannel.Reply ()
                                return! receiveNext state
                        | Remove (proc, replyChannel) ->
                            let newState =
                                { state with
                                      Processes = state.Processes |> List.filter (fun (pid, _) -> pid <> proc.Id) }

                            replyChannel.Reply ()

                            return! newState |> receiveNext
                        | Kill (proc, replyChannel) ->
                            if proc |> killProcess console then
                                let newState =
                                    { state with
                                          Processes = state.Processes |> List.filter (fun (pid, _) -> pid <> proc.Id)
                                          Killed = proc.Id :: state.Killed }

                                replyChannel.Reply ()

                                return! newState |> receiveNext
                            else
                                replyChannel.Reply ()
                                return! receiveNext state
                        | IsKilled (proc, replyChannel) ->
                            state.Killed
                            |> List.tryFind (fun x -> x = proc.Id)
                            |> Option.isSome
                            |> replyChannel.Reply

                            return! receiveNext state
                        | KillAll replyChannel ->
                            let results =
                                state.Processes
                                |> List.fold (fun fstate (pid, p) -> if p |> killProcess console then pid :: fstate else fstate) []

                            let newState =
                                { state with
                                      Killed = results @ state.Killed }

                            replyChannel.Reply ()

                            return! newState |> receiveNext
                        | Shutdown replyChannel ->
                            replyChannel.Reply ()

                            return ()
                    }

                receiveNext { Processes = []; Killed = [] }

        member internal _.Post(msg: Message) : unit =
            msg |> mailbox.Post

        member internal _.PostAndReply(builder: AsyncReplyChannel<'T> -> Message) : 'T =
            builder |> mailbox.PostAndReply

        interface IDisposable with
            /// <summary>
            /// Disposes the agent.
            /// </summary>
            member _.Dispose() =
                (mailbox :> IDisposable).Dispose ()

    /// <summary>
    /// Creates a new <see cref="T:ProcessMonitor.Agent" />.
    /// </summary>
    /// <param name="console">The <see cref="T:Console.IWriter" /> to use.</param>
    /// <returns>The new <see cref="T:ProcessMonitor.Agent" />.</returns>
    let create (console: Console.IWriter) : Agent =
        new Agent (console)

    /// <summary>
    /// Adds a process to be tracked.
    /// <para>
    /// Blocks until the message has been processed by the agent.
    /// </para>
    /// </summary>
    /// <param name="proc">The process to track.</param>
    /// <param name="monitor">The <see cref="T:ProcessMonitor.Agent" />.</param>
    let add (proc: Process) (monitor: Agent) : unit =
        monitor.PostAndReply (fun x -> Add (proc, x))


    /// <summary>
    /// Removes a process from being tracked.
    /// <para>
    /// Blocks until the message has been processed by the agent.
    /// </para>
    /// </summary>
    /// <param name="proc">The process to remove.</param>
    /// <param name="monitor">The <see cref="T:ProcessMonitor.Agent" />.</param>
    let remove (proc: Process) (monitor: Agent) : unit =
        monitor.PostAndReply (fun x -> Remove (proc, x))

    /// <summary>
    /// Kills the specified process and tracks that it has been killed.
    /// <para>
    /// Blocks until the message has been processed by the agent.
    /// </para>
    /// </summary>
    /// <param name="proc">The process to kill.</param>
    /// <param name="monitor">The <see cref="T:ProcessMonitor.Agent" />.</param>
    let kill (proc: Process) (monitor: Agent) : unit =
        monitor.PostAndReply (fun x -> Kill (proc, x))

    /// <summary>
    /// Checks if the specified process has been killed.
    /// <para>
    /// Blocks until the message has been processed by the agent.
    /// </para>
    /// </summary>
    /// <param name="proc">The process to kill.</param>
    /// <param name="monitor">The <see cref="T:ProcessMonitor.Agent" />.</param>
    /// <returns><c>true</c> if killed, <c>false</c> if not.</returns>
    let isKilled (proc: Process) (monitor: Agent) : bool =
        monitor.PostAndReply (fun x -> IsKilled (proc, x))

    /// <summary>
    /// Kills all processes tracked by the <see cref="T:ProcessMonitor.Agent" />.
    /// <para>
    /// Blocks until the message has been processed by the agent.
    /// </para>
    /// </summary>
    /// <param name="monitor">The <see cref="T:ProcessMonitor.Agent" />.</param>
    let killAll (monitor: Agent) : unit =
        KillAll |> monitor.PostAndReply

    /// <summary>
    /// Shuts down the <see cref="T:ProcessMonitor.Agent" />.
    /// It will no longer function after being shut down.
    /// <para>
    /// Blocks until the message has been processed by the agent.
    /// </para>
    /// </summary>
    /// <param name="monitor">The <see cref="T:ProcessMonitor.Agent" />.</param>
    let shutdown (monitor: Agent) : unit =
        Shutdown |> monitor.PostAndReply

namespace FsMake

open System
open System.Diagnostics

module ProcessMonitor =
    type Message =
        | Add of proc: Process * reply: AsyncReplyChannel<unit>
        | Remove of proc: Process * reply: AsyncReplyChannel<unit>
        | Kill of proc: Process * reply: AsyncReplyChannel<unit>
        | IsKilled of proc: Process * reply: AsyncReplyChannel<bool>
        | KillAll of reply: AsyncReplyChannel<unit>
        | Shutdown of reply: AsyncReplyChannel<unit>

    [<AutoOpen>]
    module Internal =
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

        member internal _.PostAndReply(builder: AsyncReplyChannel<'a> -> Message) : 'a =
            builder |> mailbox.PostAndReply

        interface IDisposable with
            member _.Dispose() =
                (mailbox :> IDisposable).Dispose ()

    let create (console: Console.IWriter) : Agent =
        new Agent (console)

    let add (proc: Process) (monitor: Agent) : unit =
        monitor.PostAndReply (fun x -> Add (proc, x))

    let remove (proc: Process) (monitor: Agent) : unit =
        monitor.PostAndReply (fun x -> Remove (proc, x))

    let kill (proc: Process) (monitor: Agent) : unit =
        monitor.PostAndReply (fun x -> Kill (proc, x))

    let isKilled (proc: Process) (monitor: Agent) : bool =
        monitor.PostAndReply (fun x -> IsKilled (proc, x))

    let killAll (monitor: Agent) : unit =
        KillAll |> monitor.PostAndReply

    let shutdown (monitor: Agent) : unit =
        Shutdown |> monitor.PostAndReply

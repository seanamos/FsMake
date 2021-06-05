namespace FsMake

open System
open System.Diagnostics

module ProcessMonitor =
    type Message =
        | Add of proc: Process
        | Remove of proc: Process
        | Kill of proc: Process * reply: AsyncReplyChannel<unit>
        | IsKilled of proc: Process * reply: AsyncReplyChannel<bool>
        | KillAll of reply: AsyncReplyChannel<unit>
        | Shutdown of reply: AsyncReplyChannel<unit>

    type internal State =
        { Processes: Process list
          Killed: int list }

    [<AutoOpen>]
    module Internal =
        let killProcess (console: Console.IWriter) (proc: Process) : bool =
            if not proc.HasExited then
                try
                    proc.Kill ()
                with ex ->
                    Console.Verbose
                    |> Console.statusMessage Console.warnColor "Failed to kill process "
                    |> Console.appendParts [ proc.Id.ToString () |> Console.Token
                                             sprintf " exception: %s" Environment.NewLine |> Console.Text
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
                        | Add proc ->
                            if not proc.HasExited then
                                proc.EnableRaisingEvents <- true
                                proc.Exited.Add (fun _ -> Remove proc |> inbox.Post)

                                return!
                                    { state with
                                          Processes = proc :: state.Processes }
                                    |> receiveNext
                            else
                                return! receiveNext state
                        | Remove proc ->
                            return!
                                { state with
                                      Processes = state.Processes |> List.filter (fun x -> x.Id <> proc.Id) }
                                |> receiveNext
                        | Kill (proc, replyChannel) ->
                            Remove proc |> inbox.Post

                            if proc |> killProcess console then
                                replyChannel.Reply ()

                                return!
                                    { state with
                                          Killed = proc.Id :: state.Killed }
                                    |> receiveNext
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
                                |> List.fold (fun fstate x -> if x |> killProcess console then x.Id :: fstate else fstate) []

                            replyChannel.Reply ()

                            return!
                                { state with
                                      Killed = results @ state.Killed }
                                |> receiveNext
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
        Add proc |> monitor.Post

    let kill (proc: Process) (monitor: Agent) : unit =
        monitor.PostAndReply (fun x -> Kill (proc, x))

    let isKilled (proc: Process) (monitor: Agent) : bool =
        monitor.PostAndReply (fun x -> IsKilled (proc, x))

    let killAll (monitor: Agent) : unit =
        KillAll |> monitor.PostAndReply

    let shutdown (monitor: Agent) : unit =
        Shutdown |> monitor.PostAndReply

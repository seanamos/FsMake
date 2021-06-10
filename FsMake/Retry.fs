namespace FsMake

module Retry =
    let retry (attempts: int) (retryFunc: unit -> StepPart<'T>) : StepPart<'T> =
        let rec nextRetry attempt ctx =
            let errorMessage (messages: Console.Message list) =
                messages
                |> Prefix.Internal.addOptionalPrefixes ctx.IsParallel ctx.PrefixOption ctx.Prefix
                |> ctx.Console.WriteLines

            let retryMessage () =
                Console.warn "Retrying, attempt "
                |> Console.appendToken ((attempt + 1).ToString ())
                |> Prefix.Internal.addOptionalPrefix ctx.IsParallel ctx.PrefixOption ctx.Prefix
                |> ctx.Console.WriteLine

            try
                let part = retryFunc ()
                let result = part ctx

                match result with
                | Ok _ as x -> x
                | Error (StepAbort _) as x -> x
                | Error x ->
                    if attempt < attempts then
                        x |> StepError.toConsoleMessage |> errorMessage
                        retryMessage ()

                        ctx |> nextRetry (attempt + 1)
                    else
                        Error x
            with ex ->
                if attempt < attempts then
                    ex |> Exception.toConsoleMessage |> errorMessage
                    retryMessage ()

                    ctx |> nextRetry (attempt + 1)
                else
                    StepUnhandledEx ex |> Error

        fun ctx -> ctx |> nextRetry 1

    [<Sealed>]
    type Builder(attempts: int) =
        inherit StepPart.BaseBuilder()

        member _.Run(generator: unit -> StepPart<'T>) : StepPart<'T> =
            generator |> retry attempts

[<AutoOpen>]
module RetryBuilder =
    let retry (attempts: int) : Retry.Builder =
        Retry.Builder (attempts)

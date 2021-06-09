namespace FsMake

module Retry =
    let retry (attempts: int) (retryFunc: unit -> StepPart<'T>) : StepPart<'T> =
        fun ctx ->
            let rec nextRetry attempt =
                let retryMessage () =
                    Console.warn "Retrying, attempt "
                    |> Console.appendToken ((attempt + 1).ToString ())
                    |> Prefix.Internal.addOptionalPrefix ctx.IsParallel ctx.PrefixOption ctx.Prefix
                    |> ctx.Console.WriteLine

                try
                    let part = retryFunc ()
                    let result = part ctx

                    match result with
                    | Ok x -> Ok x
                    | Error x ->
                        if attempt < attempts then
                            StepError.toConsoleMessage x
                            |> Prefix.Internal.addOptionalPrefixes ctx.IsParallel ctx.PrefixOption ctx.Prefix
                            |> ctx.Console.WriteLines

                            retryMessage ()

                            nextRetry (attempt + 1)
                        else
                            Error x
                with ex ->
                    if attempt < attempts then
                        Exception.toConsoleMessage ex
                        |> Prefix.Internal.addOptionalPrefixes ctx.IsParallel ctx.PrefixOption ctx.Prefix
                        |> ctx.Console.WriteLines

                        retryMessage ()

                        nextRetry (attempt + 1)
                    else
                        StepUnhandledEx ex |> Error

            nextRetry 1

    [<Sealed>]
    type Builder(attempts: int) =
        inherit StepPart.BaseBuilder()

        member _.Run(generator: unit -> StepPart<'T>) : StepPart<'T> =
            generator |> retry attempts

[<AutoOpen>]
module RetryBuilder =
    let retry (attempts: int) : Retry.Builder =
        Retry.Builder (attempts)

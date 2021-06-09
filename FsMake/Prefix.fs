namespace FsMake

open System

module Prefix =
    type PrefixOption =
        | Always
        | Never
        | WhenParallel

    [<AutoOpen>]
    module internal Internal =
        let prefixColors =
            [ Console.Cyan
              Console.DarkCyan
              Console.DarkYellow
              Console.Green
              Console.Magenta
              Console.Yellow ]

        let createPrefix (stepMaxLength: int) (stepName: string) : (Console.TextPart) =
            let prefix = sprintf "%-*s | " stepMaxLength stepName
            let prefixColorIdx = Math.Abs (stepName.GetHashCode ()) % prefixColors.Length
            let prefixColor = prefixColors.[prefixColorIdx]

            Console.Colorized (prefix, prefixColor)

        let shouldPrefix (isParallel: bool) (pipelineOpt: PrefixOption) : bool =
            match pipelineOpt with
            | Always -> true
            | Never -> false
            | WhenParallel -> isParallel

        let addOptionalPrefixes (isParallel: bool) (pipelineOpt: PrefixOption) (prefix: Console.TextPart) (messages: Console.Message list) : Console.Message list =
            if shouldPrefix isParallel pipelineOpt then
                messages |> List.map (Console.prefix prefix)
            else
                messages

        let addOptionalPrefix (isParallel: bool) (pipelineOpt: PrefixOption) (prefix: Console.TextPart) (message: Console.Message) : Console.Message =
            if shouldPrefix isParallel pipelineOpt then
                message |> Console.prefix prefix
            else
                message


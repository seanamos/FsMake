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

        type OptionalPrefixArgs =
            { IsParallel: bool
              PrefixOption: PrefixOption
              Prefix: Console.TextPart }

        let addOptionalPrefixes (args: OptionalPrefixArgs) (messages: Console.Message list) : Console.Message list =
            if shouldPrefix args.IsParallel args.PrefixOption then
                messages |> List.map (Console.prefix args.Prefix)
            else
                messages

        let addOptionalPrefix (args: OptionalPrefixArgs) (message: Console.Message) : Console.Message =
            if shouldPrefix args.IsParallel args.PrefixOption then
                message |> Console.prefix args.Prefix
            else
                message

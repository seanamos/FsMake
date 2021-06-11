namespace FsMake

open System
open System.Text

module Console =
    type Level =
        | Error
        | Important
        | Warn
        | Info
        | Verbose

    type Verbosity =
        | Disabled
        | Quiet
        | Normal
        | All

    module internal Verbosity =
        let matchLevel (verbosity: Verbosity) (level: Level) : bool =
            let containsLevel levels =
                levels |> List.tryFind (fun x -> level = x) |> Option.isSome

            match verbosity with
            | Disabled -> false
            | Quiet -> [ Error; Important ] |> containsLevel
            | Normal -> [ Error; Important; Warn; Info ] |> containsLevel
            | All -> true

    type Color =
        | Cyan
        | DarkCyan
        | DarkYellow
        | Gray
        | Green
        | Magenta
        | Red
        | White
        | Yellow

    module internal Color =
        let toConsoleColor (color: Color) =
            match color with
            | Cyan -> ConsoleColor.Cyan
            | DarkCyan -> ConsoleColor.DarkCyan
            | DarkYellow -> ConsoleColor.DarkYellow
            | Gray -> ConsoleColor.Gray
            | Green -> ConsoleColor.Green
            | Magenta -> ConsoleColor.Magenta
            | Red -> ConsoleColor.Red
            | White -> ConsoleColor.White
            | Yellow -> ConsoleColor.Yellow

        let toAnsiCode (color: Color) =
            match color with
            | Cyan -> "\u001b[38;5;14m"
            | DarkCyan -> "\u001b[38;5;44m"
            | DarkYellow -> "\u001b[38;5;3m"
            | Gray -> "\u001b[38;5;7m"
            | Green -> "\u001b[38;5;10m"
            | Magenta -> "\u001b[38;5;13m"
            | Red -> "\u001b[38;5;9m"
            | White -> "\u001b[38;5;15m"
            | Yellow -> "\u001b[38;5;11m"

    let infoColor = Cyan
    let successColor = Green
    let warnColor = Yellow
    let errorColor = Red

    type TextPart =
        | Text of text: string
        | Token of text: string
        | Colorized of text: string * color: Color

    type Message =
        { Level: Level
          Prefix: TextPart option
          TextParts: TextPart list
          TokenColor: Color }

    type OutputType =
        | Ansi
        | Standard

    type IWriter =
        abstract member Write : messages: Message list -> unit

    [<AutoOpen>]
    module internal Internal =
        let checkLevel (verbosity: Verbosity) (level: Level) =
            match verbosity with
            | _ when level |> Verbosity.matchLevel verbosity -> true
            | _ -> false

        [<Sealed>]
        type AnsiWriter(verbosity: Verbosity) =
            let locker = obj ()
            let ansiReset = "\u001b[0m"

            let formatTextPart tokenColor textPart =
                match textPart with
                | Text x -> x
                | Token x -> $"{tokenColor}{x}{ansiReset}"
                | Colorized (text, color) ->
                    let ansiColor = color |> Color.toAnsiCode
                    $"{ansiColor}{text}{ansiReset}"

            let textPartsToStringBuilder tokenColor textParts =
                let sb = StringBuilder ()

                textParts
                |> List.fold (fun (sb: StringBuilder) x -> x |> formatTextPart tokenColor |> sb.Append) sb

            interface IWriter with
                member _.Write(messages: Message list) : unit =
                    lock locker
                    <| fun () ->
                        messages
                        |> List.iter (fun message ->
                            if message.Level |> checkLevel verbosity then
                                let tokenColor = message.TokenColor |> Color.toAnsiCode

                                let sb = message.TextParts |> textPartsToStringBuilder tokenColor

                                match message.Prefix with
                                | Some x ->
                                    let prefixfmt = x |> formatTextPart tokenColor
                                    sb.Insert (0, prefixfmt) |> ignore
                                | None -> ()

                                Console.Write (sb.ToString ())
                    )

        [<Sealed>]
        type StandardWriter(verbosity: Verbosity) =
            let locker = obj ()

            let writeColor color (text: string) =
                let prevColor = Console.ForegroundColor
                Console.ForegroundColor <- color
                Console.Write (text)
                Console.ForegroundColor <- prevColor

            let writeTextPart tokenColor textPart =
                match textPart with
                | Text x -> Console.Write (x)
                | Token x -> x |> writeColor tokenColor
                | Colorized (text, color) ->
                    let consoleColor = color |> Color.toConsoleColor
                    text |> writeColor consoleColor

            interface IWriter with
                member _.Write(messages: Message list) =
                    lock locker
                    <| fun () ->
                        messages
                        |> List.iter (fun message ->
                            if message.Level |> checkLevel verbosity then
                                let tokenColor = message.TokenColor |> Color.toConsoleColor
                                let writeTextPart = writeTextPart tokenColor

                                match message.Prefix with
                                | Some x -> writeTextPart x
                                | None -> ()

                                message.TextParts |> List.iter (writeTextPart)
                        )

        let defaultWriter = StandardWriter (Normal) :> IWriter

    let createWriter (outputType: OutputType) (verbosity: Verbosity) : IWriter =
        match outputType with
        | Ansi -> AnsiWriter (verbosity) :> IWriter
        | Standard -> StandardWriter (verbosity) :> IWriter

    let messageEmpty (tokenColor: Color) (level: Level) : Message =
        { Level = level
          Prefix = None
          TokenColor = tokenColor
          TextParts = [] }

    let messageParts (tokenColor: Color) (textParts: TextPart list) (level: Level) : Message =
        { Level = level
          Prefix = None
          TokenColor = tokenColor
          TextParts = textParts }

    let messageColor (color: Color) (text: string) (level: Level) : Message =
        level |> messageParts color [ Colorized (text, color) ]

    let message (text: string) (level: Level) : Message =
        level |> messageParts infoColor [ Text text ]

    let statusMessage (color: Color) (text: string) (level: Level) : Message =
        level |> messageParts color [ Token "==> "; Text text ]

    let appendParts (textParts: TextPart list) (message: Message) : Message =
        { message with
              TextParts = message.TextParts @ textParts }

    let appendPart (textPart: TextPart) (message: Message) : Message =
        { message with
              TextParts = message.TextParts @ [ textPart ] }

    let appendColor (color: Color) (text: string) (message: Message) : Message =
        message |> appendParts [ Colorized (text, color) ]

    let appendToken (text: string) (message: Message) : Message =
        message |> appendParts [ Token text ]

    let append (text: string) (message: Message) : Message =
        message |> appendParts [ Text text ]

    let prefix (prefix: TextPart) (message: Message) : Message =
        { message with Prefix = Some prefix }

    let error (text: string) : Message =
        Error |> statusMessage errorColor text

    let warn (text: string) : Message =
        Warn |> statusMessage warnColor text

    let info (text: string) : Message =
        Info |> statusMessage infoColor text

    let success (text: string) : Message =
        Info |> statusMessage successColor text

[<AutoOpen>]
module IWriterExtensions =
    type Console.IWriter with
        member this.Write(message: Console.Message) : unit =
            this.Write [ message ]

        member this.WriteLine(level: Console.Level) : unit =
            level |> Console.message Environment.NewLine |> this.Write

        member this.WriteLine(message: Console.Message) : unit =
            [ { message with
                    TextParts = message.TextParts @ [ Console.Text Environment.NewLine ] } ]
            |> this.Write

        member this.WriteLines(messages: Console.Message list) : unit =
            messages
            |> List.map (fun x ->
                { x with
                      TextParts = x.TextParts @ [ Console.Text Environment.NewLine ] }
            )
            |> this.Write

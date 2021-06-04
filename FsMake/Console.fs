namespace FsMake

open System

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
        { TextParts: TextPart list
          TokenColor: Color
          Level: Level }

    type OutputType =
        | Ansi
        | Standard

    type IWriter =
        abstract member Write : message: Message -> unit
        abstract member WriteLine : level: Level -> unit
        abstract member WriteLine : message: Message -> unit

    let private locker = obj ()

    type AnsiWriter(verbosity: Verbosity) =
        let ansiReset = "\u001b[0m"

        let checkLevel level =
            match verbosity with
            | _ when level |> Verbosity.matchLevel verbosity -> true
            | _ -> false

        interface IWriter with
            member _.Write message =
                lock locker
                <| fun () ->
                    if message.Level |> checkLevel then
                        let tokenColor = message.TokenColor |> Color.toAnsiCode

                        message.TextParts
                        |> List.iter
                            (function
                            | Text x -> Console.Write (x)
                            | Token x -> Console.Write ($"{tokenColor}{x}{ansiReset}")
                            | Colorized (text, color) ->
                                let ansiColor = color |> Color.toAnsiCode
                                Console.Write ($"{ansiColor}{text}{ansiReset}"))

            member _.WriteLine level =
                if level |> checkLevel then Console.WriteLine ()

            member this.WriteLine message =
                let writer = (this :> IWriter)

                { message with
                      TextParts = message.TextParts @ [ Text Environment.NewLine ] }
                |> writer.Write

    type StandardWriter(verbosity: Verbosity) =
        let checkLevel level =
            match verbosity with
            | _ when level |> Verbosity.matchLevel verbosity -> true
            | _ -> false

        let writeColor color (text: string) =
            let prevColor = Console.ForegroundColor
            Console.ForegroundColor <- color
            Console.Write (text)
            Console.ForegroundColor <- prevColor

        interface IWriter with
            member _.Write message =
                lock locker
                <| fun () ->
                    if message.Level |> checkLevel then
                        let tokenColor = message.TokenColor |> Color.toConsoleColor

                        message.TextParts
                        |> List.iter
                            (function
                            | Text x -> Console.Write (x)
                            | Token x -> x |> writeColor tokenColor
                            | Colorized (text, color) ->
                                let consoleColor = color |> Color.toConsoleColor
                                text |> writeColor consoleColor)

            member _.WriteLine level =
                if level |> checkLevel then Console.WriteLine ()

            member this.WriteLine message =
                let writer = (this :> IWriter)

                { message with
                      TextParts = message.TextParts @ [ Text Environment.NewLine ] }
                |> writer.Write

    let defaultWriter = StandardWriter (Normal) :> IWriter

    let createWriter (outputType: OutputType) (verbosity: Verbosity) : IWriter =
        match outputType with
        | Ansi -> AnsiWriter (verbosity) :> IWriter
        | Standard -> StandardWriter (verbosity) :> IWriter

    let messageEmpty (tokenColor: Color) (level: Level) : Message =
        { TokenColor = tokenColor
          TextParts = []
          Level = level }

    let messageParts (tokenColor: Color) (textParts: TextPart list) (level: Level) : Message =
        { TokenColor = tokenColor
          TextParts = textParts
          Level = level }

    let messageColor (color: Color) (text: string) (level: Level) : Message =
        level |> messageParts color [ Colorized (text, color) ]

    let message (text: string) (level: Level) : Message =
        level |> messageParts infoColor [ Text text ]

    let prefix (color: Color) (text: string) (level: Level) : Message =
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

    let error (text: string) : Message =
        Error |> prefix errorColor text

    let warn (text: string) : Message =
        Warn |> prefix warnColor text

    let info (text: string) : Message =
        Info |> prefix infoColor text

    let success (text: string) : Message =
        Info |> prefix successColor text

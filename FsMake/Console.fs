namespace FsMake

open System
open System.Text

/// <summary>
/// Module for writing to the console.
/// </summary>
module Console =

    /// <summary>
    /// The level of a console message.
    /// </summary>
    type Level =
        /// <summary>Highest message level.</summary>
        | Error
        /// <summary>Highest message level, shared with <c>Error</c>.</summary>
        | Important
        /// <summary>Second highest message level.</summary>
        | Warn
        /// <summary>Normal message level.</summary>
        | Info
        /// <summary>Lowest message level.</summary>
        | Verbose

    /// <summary>
    /// The verbosity of a console writer.
    /// </summary>
    type Verbosity =
        /// <summary>
        /// All console output is disabled.
        /// </summary>
        | Disabled
        /// <summary>
        /// Only <c>Error</c> and <c>Important</c> messages will be written to the console.
        /// </summary>
        | Quiet
        /// <summary>
        /// **Default**. All messages except <c>Verbose</c> will be written.
        /// </summary>
        | Normal
        /// <summary>
        /// Messages of all <see cref="T:FsMake.Console.Level" /> will be written.
        /// </summary>
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

    /// <summary>
    /// Console colors.
    /// </summary>
    type Color =
        | Blue
        | Cyan
        | DarkBlue
        | DarkCyan
        | DarkGreen
        | DarkMagenta
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
            | Blue -> ConsoleColor.Blue
            | Cyan -> ConsoleColor.Cyan
            | DarkBlue -> ConsoleColor.DarkBlue
            | DarkCyan -> ConsoleColor.DarkCyan
            | DarkGreen -> ConsoleColor.DarkGreen
            | DarkMagenta -> ConsoleColor.DarkMagenta
            | DarkYellow -> ConsoleColor.DarkYellow
            | Gray -> ConsoleColor.Gray
            | Green -> ConsoleColor.Green
            | Magenta -> ConsoleColor.Magenta
            | Red -> ConsoleColor.Red
            | White -> ConsoleColor.White
            | Yellow -> ConsoleColor.Yellow

        let toAnsiCode (color: Color) =
            match color with
            | Blue -> "\u001b[38;5;12m"
            | Cyan -> "\u001b[38;5;14m"
            | DarkBlue -> "\u001b[38;5;4m"
            | DarkCyan -> "\u001b[38;5;6m"
            | DarkGreen -> "\u001b[38;5;2m"
            | DarkMagenta -> "\u001b[38;5;5m"
            | DarkYellow -> "\u001b[38;5;3m"
            | Gray -> "\u001b[38;5;7m"
            | Green -> "\u001b[38;5;10m"
            | Magenta -> "\u001b[38;5;13m"
            | Red -> "\u001b[38;5;9m"
            | White -> "\u001b[38;5;15m"
            | Yellow -> "\u001b[38;5;11m"

    /// <summary>
    /// The info status message <see cref="T:Console.Color" />.
    /// </summary>
    let infoColor = Cyan

    /// <summary>
    /// The success status message <see cref="T:Console.Color" />.
    /// </summary>
    let successColor = Green

    /// <summary>
    /// The warn status message <see cref="T:Console.Color" />.
    /// </summary>
    let warnColor = Yellow

    /// <summary>
    /// The error status message <see cref="T:Console.Color" />.
    /// </summary>
    let errorColor = Red

    /// <summary>
    /// Represents text that makes up a message.
    /// </summary>
    type TextPart =
        /// <summary>
        /// Plain text.
        /// </summary>
        /// <param name="text">The text.</param>
        | Text of text: string
        /// <summary>
        /// A token that should be colorized with the <see cref="T:FsMake.Console.Message" />'s <c>TokenColor</c>.
        /// </summary>
        /// <param name="text">The token's text.</param>
        | Token of text: string
        /// <summary>
        /// Text that should the written with the specified <c>color</c>.
        /// </summary>
        /// <param name="text">The text.</param>
        /// <param name="color">The color to write the text in.</param>
        | Colorized of text: string * color: Color

    /// <summary>
    /// A message to write the console.
    /// </summary>
    type Message =
        {
            Level: Level
            Prefix: TextPart option
            TextParts: TextPart list
            TokenColor: Color
        }

    /// <summary>
    /// The output of the console writer.
    /// </summary>
    type OutputType =
        /// <summary>
        /// Outputs formatting is ansi escape codes.
        /// </summary>
        | Ansi
        /// <summary>
        /// Output formatting using <see cref="T:System.Console" /> properties.
        /// </summary>
        | Standard

    /// <summary>
    /// An abstraction for different types of console writers.
    /// </summary>
    type IWriter =
        /// <summary>
        /// Writes a list of messages to the console.
        /// </summary>
        /// <param name="messages">The messages to write.</param>
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

    /// <summary>
    /// Creates a <see cref="T:FsMake.Console.IWriter" />.
    /// </summary>
    /// <param name="outputType">The <see cref="T:Console.OutputType" /> of the console.</param>
    /// <param name="verbosity">The <see cref="T:Console.Verbosity" /> of the console.</param>
    /// <returns>The new <see cref="T:FsMake.Console.IWriter" />.</returns>
    let createWriter (outputType: OutputType) (verbosity: Verbosity) : IWriter =
        match outputType with
        | Ansi -> AnsiWriter (verbosity) :> IWriter
        | Standard -> StandardWriter (verbosity) :> IWriter

    /// <summary>
    /// Creates an empty <see cref="Console.Message" />.
    /// </summary>
    /// <param name="tokenColor">The color of <see cref="Console.TokenPart" />s in the message.</param>
    /// <param name="level">The level of the message.</param>
    /// <returns>The new message.</returns>
    let messageEmpty (tokenColor: Color) (level: Level) : Message =
        {
            Level = level
            Prefix = None
            TokenColor = tokenColor
            TextParts = []
        }

    /// <summary>
    /// Creates a <see cref="Console.Message" /> from the given parts.
    /// </summary>
    /// <param name="tokenColor">The color of <see cref="Console.TokenPart" />s in the message.</param>
    /// <param name="textParts">The list of parts that make up message.</param>
    /// <param name="level">The level of the message.</param>
    /// <returns>The new message.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let msg =
    ///     Console.Info
    ///     |> Console.messageParts
    ///         Console.infoColor
    ///         [ Console.Text "Hello "
    ///           Console.Colorized ("world!", Console.Yellow) ]
    /// </code>
    /// </example>
    let messageParts (tokenColor: Color) (textParts: TextPart list) (level: Level) : Message =
        {
            Level = level
            Prefix = None
            TokenColor = tokenColor
            TextParts = textParts
        }

    /// <summary>
    /// Creates a message with colorized text.
    /// </summary>
    /// <param name="color">The color of the text.</param>
    /// <param name="text">The message text.</param>
    /// <param name="level">The level of the message.</param>
    /// <returns>The new message.</returns>
    let messageColor (color: Color) (text: string) (level: Level) : Message =
        level |> messageParts color [ Colorized (text, color) ]

    /// <summary>
    /// Creates a message with text.
    /// </summary>
    /// <param name="text">The message text.</param>
    /// <param name="level">The level of the message.</param>
    /// <returns>The new message.</returns>
    let message (text: string) (level: Level) : Message =
        level |> messageParts infoColor [ Text text ]

    /// <summary>
    /// Creates a status message.
    /// </summary>
    /// <param name="color">The color of tokens in the message.</param>
    /// <param name="text">The message text.</param>
    /// <param name="level">The level of the message.</param>
    /// <returns>The new message.</returns>
    let statusMessage (color: Color) (text: string) (level: Level) : Message =
        level |> messageParts color [ Token "==> "; Text text ]

    /// <summary>
    /// Appends <see cref="T:Console.Textpart" />s to a message.
    /// </summary>
    /// <param name="textParts">The parts to append.</param>
    /// <param name="message">The message to append the parts to.</param>
    /// <returns>The updated message.</returns>
    /// <example>
    /// <code>
    /// let msg =
    ///     Console.Info
    ///     |> Console.message "Hello "
    ///     |> Console.appendParts [ Console.Text "world!" ]
    /// </code>
    /// </example>
    let appendParts (textParts: TextPart list) (message: Message) : Message =
        { message with
            TextParts = message.TextParts @ textParts
        }

    /// <summary>
    /// Appends a <see cref="T:Console.Textpart" /> to a message.
    /// </summary>
    /// <param name="textPart">The part to append.</param>
    /// <param name="message">The message to append the part to.</param>
    /// <returns>The updated message.</returns>
    /// <example>
    /// <code>
    /// let msg =
    ///     Console.Info
    ///     |> Console.message "Hello "
    ///     |> Console.appendPart (Console.Text "world!")
    /// </code>
    /// </example>
    let appendPart (textPart: TextPart) (message: Message) : Message =
        { message with
            TextParts = message.TextParts @ [ textPart ]
        }

    /// <summary>
    /// Appends colorized text to a message.
    /// </summary>
    /// <param name="color">The color of the text.</param>
    /// <param name="text">The text to append.</param>
    /// <param name="message">The message to append the colored text to.</param>
    /// <returns>The updated message.</returns>
    let appendColor (color: Color) (text: string) (message: Message) : Message =
        message |> appendParts [ Colorized (text, color) ]

    /// <summary>
    /// Appends a token to a message.
    /// </summary>
    /// <param name="text">The token text.</param>
    /// <param name="message">The message to append the token to.</param>
    /// <returns>The updated message.</returns>
    let appendToken (text: string) (message: Message) : Message =
        message |> appendParts [ Token text ]

    /// <summary>
    /// Append text to a message.
    /// </summary>
    /// <param name="text">The text to append.</param>
    /// <param name="message">The message to append the text to.</param>
    /// <returns>The updated message.</returns>
    let append (text: string) (message: Message) : Message =
        message |> appendParts [ Text text ]

    /// <summary>
    /// Sets a message prefix.
    /// </summary>
    /// <param name="prefix">Prefix for the beginning of the message.</param>
    /// <param name="message">The message to set the prefix on.</param>
    /// <returns>The updated message.</returns>
    let prefix (prefix: TextPart) (message: Message) : Message =
        { message with Prefix = Some prefix }

    /// <summary>
    /// Creates an error status message.
    /// </summary>
    /// <param name="text">The error text.</param>
    /// <returns>The new message.</returns>
    let error (text: string) : Message =
        Error |> statusMessage errorColor text

    /// <summary>
    /// Creates a warning status message.
    /// </summary>
    /// <param name="text">The warning text.</param>
    /// <returns>The new message.</returns>
    let warn (text: string) : Message =
        Warn |> statusMessage warnColor text

    /// <summary>
    /// Creates an info status message.
    /// </summary>
    /// <param name="text">The info text.</param>
    /// <returns>The new message.</returns>
    let info (text: string) : Message =
        Info |> statusMessage infoColor text

    /// <summary>
    /// Creates a success status message.
    /// </summary>
    /// <param name="text">The success text.</param>
    /// <returns>The new message.</returns>
    let success (text: string) : Message =
        Info |> statusMessage successColor text

/// <summary>
/// <see cref="T:FsMake.Console.IWriter" /> extension methods.
/// </summary>
[<AutoOpen>]
module IWriterExtensions =
    type Console.IWriter with
        /// <summary>
        /// Writes a <see cref="T:Console.Message" /> to the console.
        /// </summary>
        /// <param name="message">The message to be written.</param>
        member this.Write(message: Console.Message) : unit =
            this.Write [ message ]

        /// <summary>
        /// Writes a newline to the console.
        /// </summary>
        /// <param name="level">The level of the newline.</param>
        /// <remarks>
        /// If the <c>level</c> is not high enough for the <see cref="T:Console.Verbosity" />, the line will not be written.
        /// </remarks>
        member this.WriteLine(level: Console.Level) : unit =
            level |> Console.message Environment.NewLine |> this.Write

        /// <summary>
        /// Writes a <see cref="T:Console.Message" /> to the console, with a newline at the end.
        /// </summary>
        /// <param name="message">The message to write.</param>
        member this.WriteLine(message: Console.Message) : unit =
            [
                { message with
                    TextParts = message.TextParts @ [ Console.Text Environment.NewLine ]
                }
            ]
            |> this.Write

        /// <summary>
        /// Writes a <see cref="T:Console.Message" /> <c>list</c> to the console, with newlines at the end of each.
        /// </summary>
        /// <param name="messages">The messages to write.</param>
        member this.WriteLines(messages: Console.Message list) : unit =
            messages
            |> List.map (fun x ->
                { x with
                    TextParts = x.TextParts @ [ Console.Text Environment.NewLine ]
                }
            )
            |> this.Write

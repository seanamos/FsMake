module FsMake.UnitTests.ConsoleTests

open Expecto
open Expecto.Flip
open FsMake

[<Tests>]
let tests =
    testList
        "Console tests"
        [
            test "createWriter creates StandardWriter" {
                let writer = Console.createWriter Console.Standard Console.Normal

                let isType = writer :? Console.Internal.StandardWriter

                isType
                |> Expect.isTrue $"Expected writer to be of type {nameof (Console.Internal.StandardWriter)}"
            }

            test "createWriter creates AnsiWriter" {
                let writer = Console.createWriter Console.Ansi Console.Normal

                let isType = writer :? Console.Internal.AnsiWriter

                isType
                |> Expect.isTrue $"Expected writer to be of type {nameof (Console.Internal.AnsiWriter)}"
            }

            test "messageEmpty create empty message" {
                let message = Console.Error |> Console.messageEmpty Console.Red

                let expected : Console.Message =
                    {
                        Level = Console.Error
                        Prefix = None
                        TokenColor = Console.Red
                        TextParts = []
                    }

                teste <@ message = expected @>
            }

            test "messageParts creates a message with parts" {
                let message =
                    Console.Info
                    |> Console.messageParts Console.DarkBlue [ Console.Colorized ("Hello", Console.Magenta); Console.Text " world!" ]

                let expected : Console.Message =
                    {
                        Level = Console.Info
                        Prefix = None
                        TokenColor = Console.DarkBlue
                        TextParts = [ Console.Colorized ("Hello", Console.Magenta); Console.Text " world!" ]
                    }

                teste <@ message = expected @>
            }

            test "messageColor creates a messages with color part" {
                let message =
                    Console.Info
                    |> Console.messageColor Console.Magenta "Hello world!"

                let expected : Console.Message =
                    {
                        Level = Console.Info
                        Prefix = None
                        TokenColor = Console.Magenta
                        TextParts = [ Console.Colorized ("Hello world!", Console.Magenta) ]
                    }

                teste <@ message = expected @>
            }

            test "message creates a message with text part" {
                let message = Console.Info |> Console.message "Hello world!"

                let expected : Console.Message =
                    {
                        Level = Console.Info
                        Prefix = None
                        TokenColor = Console.infoColor
                        TextParts = [ Console.Text "Hello world!" ]
                    }

                teste <@ message = expected @>
            }

            test "statusMessage creates a message with token and text" {
                let message =
                    Console.Info
                    |> Console.statusMessage Console.Red "Hello world!"

                let expected : Console.Message =
                    {
                        Level = Console.Info
                        Prefix = None
                        TokenColor = Console.Red
                        TextParts = [ Console.Token "==> "; Console.Text "Hello world!" ]
                    }

                teste <@ message = expected @>
            }

            test "appendParts adds parts to message" {
                let message =
                    Console.Info
                    |> Console.messageEmpty Console.Red
                    |> Console.appendParts [ Console.Text "Hello" ]
                    |> Console.appendParts [ Console.Text " world!" ]

                let expected : Console.Message =
                    {
                        Level = Console.Info
                        Prefix = None
                        TokenColor = Console.Red
                        TextParts = [ Console.Text "Hello"; Console.Text " world!" ]
                    }

                teste <@ message = expected @>
            }

            test "appendPart adds part to message" {
                let message =
                    Console.Info
                    |> Console.message "Hello"
                    |> Console.appendPart (Console.Text " world!")

                let expected : Console.Message =
                    {
                        Level = Console.Info
                        Prefix = None
                        TokenColor = Console.infoColor
                        TextParts = [ Console.Text "Hello"; Console.Text " world!" ]
                    }

                teste <@ message = expected @>
            }

            test "appendColor adds colorized part to message" {
                let message =
                    Console.Info
                    |> Console.message "Hello"
                    |> Console.appendColor Console.DarkCyan " world!"

                let expected : Console.Message =
                    {
                        Level = Console.Info
                        Prefix = None
                        TokenColor = Console.infoColor
                        TextParts = [ Console.Text "Hello"; Console.Colorized (" world!", Console.DarkCyan) ]
                    }

                teste <@ message = expected @>
            }

            test "appendToken adds token part to message" {
                let message =
                    Console.Info
                    |> Console.message "Hello"
                    |> Console.appendToken " world!"

                let expected : Console.Message =
                    {
                        Level = Console.Info
                        Prefix = None
                        TokenColor = Console.infoColor
                        TextParts = [ Console.Text "Hello"; Console.Token " world!" ]
                    }

                teste <@ message = expected @>
            }

            test "append adds text part to message" {
                let message =
                    Console.Info
                    |> Console.message "Hello"
                    |> Console.append " world!"

                let expected : Console.Message =
                    {
                        Level = Console.Info
                        Prefix = None
                        TokenColor = Console.infoColor
                        TextParts = [ Console.Text "Hello"; Console.Text " world!" ]
                    }

                teste <@ message = expected @>
            }

            test "prefix sets prefix on message" {
                let message =
                    Console.Info
                    |> Console.messageEmpty Console.Red
                    |> Console.prefix (Console.Text "Prefix")

                let expected : Console.Message =
                    {
                        Level = Console.Info
                        Prefix = Some (Console.Text "Prefix")
                        TokenColor = Console.Red
                        TextParts = []
                    }

                teste <@ message = expected @>
            }

            test "error creates error status message" {
                let message = Console.error "oh no!"

                let expected : Console.Message =
                    {
                        Level = Console.Error
                        Prefix = None
                        TokenColor = Console.errorColor
                        TextParts = [ Console.Token "==> "; Console.Text "oh no!" ]
                    }

                teste <@ message = expected @>
            }

            test "warn creates warn status message" {
                let message = Console.warn "warning"

                let expected : Console.Message =
                    {
                        Level = Console.Warn
                        Prefix = None
                        TokenColor = Console.warnColor
                        TextParts = [ Console.Token "==> "; Console.Text "warning" ]
                    }

                teste <@ message = expected @>
            }

            test "info creates info status message" {
                let message = Console.info "info"

                let expected : Console.Message =
                    {
                        Level = Console.Info
                        Prefix = None
                        TokenColor = Console.infoColor
                        TextParts = [ Console.Token "==> "; Console.Text "info" ]
                    }

                teste <@ message = expected @>
            }

            test "success creates success status message" {
                let message = Console.success "success"

                let expected : Console.Message =
                    {
                        Level = Console.Info
                        Prefix = None
                        TokenColor = Console.successColor
                        TextParts = [ Console.Token "==> "; Console.Text "success" ]
                    }

                teste <@ message = expected @>
            }
        ]

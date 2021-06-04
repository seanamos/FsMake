namespace FsMake

exception FsMakeException of message: string

[<AutoOpen>]
module Errors =
    let abort (message: string) =
        FsMakeException (message) |> raise

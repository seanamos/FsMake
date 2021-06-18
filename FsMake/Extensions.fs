namespace FsMake

open System

module internal Exception =
    let toConsoleMessage (ex: Exception) : Console.Message list =
        let split = ex.ToString().Split (Environment.NewLine)

        Console.error $"Exception:"
        :: [
            for exLine in split ->
                Console.Error
                |> Console.messageColor Console.errorColor exLine
        ]

module internal List =
    let foldi (folder: int -> 'State -> 'T -> 'State) (state: 'State) (list: 'T list) : 'State =
        list
        |> List.fold
            (fun (idx, state) x ->
                let innerState = folder idx state x
                (idx + 1, innerState)
            )
            (0, state)
        |> snd

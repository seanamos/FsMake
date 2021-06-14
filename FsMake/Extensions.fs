namespace FsMake

open System

module internal Exception =
    let toConsoleMessage (ex: Exception) : Console.Message list =
        let split = ex.ToString().Split (Environment.NewLine)

        Console.error $"Exception:"
        :: [ for exLine in split -> Console.Error |> Console.messageColor Console.errorColor exLine ]

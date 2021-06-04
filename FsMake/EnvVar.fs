namespace FsMake

open System
open System.ComponentModel

module EnvVar =
    let get (name: string) : string =
        Environment.GetEnvironmentVariable (name)

    let getOption (name: string) : Option<string> =
        let var = get name

        if isNull var then None else Some var

    let getAs<'a when 'a: struct> (name: string) : 'a =
        let var = get name

        let converter = TypeDescriptor.GetConverter (typeof<'a>)
        converter.ConvertFromString (var) :?> 'a

    let getOptionAs<'a when 'a: struct> (name: string) =
        try
            let var = getAs<'a> name

            Some var
        with _ -> None

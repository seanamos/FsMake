namespace FsMake

open System
open System.ComponentModel

module EnvVar =
    let get (name: string) : string =
        Environment.GetEnvironmentVariable (name)

    let getOption (name: string) : Option<string> =
        let var = get name

        if isNull var then None else Some var

    let getAs<'T when 'T: struct> (name: string) : 'T =
        let var = get name

        let converter = TypeDescriptor.GetConverter (typeof<'T>)
        converter.ConvertFromString (var) :?> 'T

    let getOptionAs<'T when 'T: struct> (name: string) =
        try
            let var = getAs<'T> name

            Some var
        with _ -> None

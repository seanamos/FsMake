namespace FsMake

open System
open System.ComponentModel

/// <summary>
/// Module for working with environment variables.
/// </summary>
module EnvVar =

    /// <summary>
    /// Gets an environment variable by name.
    /// </summary>
    /// <param name="name">The name of the env var to get.</param>
    /// <returns>The value of the env var.</returns>
    /// <exception cref="T:System.Exception">Thrown when an env var with the specified name does not exist.</exception>
    let get (name: string) : string =
        let var = Environment.GetEnvironmentVariable (name)

        if isNull var then
            Exception ($"Could not get a value for the environment variable \"{name}\"")
            |> raise
        else
            var

    /// <summary>
    /// Gets an environment variable by name.
    /// </summary>
    /// <param name="name">The name of the env var to get.</param>
    /// <returns><c>Some</c> if env var exists, <c>None</c> if it does not.</returns>
    let getOption (name: string) : Option<string> =
        let var = Environment.GetEnvironmentVariable (name)

        if isNull var then None else Some var

    /// <summary>
    /// Gets an environment variable by name and converts it to the specified type.
    /// </summary>
    /// <param name="name">The name of the env var to get.</param>
    /// <typeparam name="'T">The type to convert the env var to.</typeparam>
    /// <returns>The converted env var.</returns>
    /// <exception cref="System.Exception">Thrown when the env var does not exist.</exception>
    /// <exception cref="System.NotSupportedException">Thrown when the env var cannot be converted to the specified type.</exception>
    let getAs<'T when 'T: struct> (name: string) : 'T =
        let var = get name

        let converter = TypeDescriptor.GetConverter (typeof<'T>)
        converter.ConvertFromString (var) :?> 'T

    /// <summary>
    /// Gets an environment variable by name and converts it to the specified type.
    /// </summary>
    /// <param name="name">The name of the env var to get.</param>
    /// <typeparam name="'T">The type to convert the env var to.</typeparam>
    /// <returns><c>Some</c> if env var exists and can be converted, <c>None</c> if it does not or cannot be converted.</returns>
    let getOptionAs<'T when 'T: struct> (name: string) =
        try
            let var = getAs<'T> name

            Some var
        with _ -> None

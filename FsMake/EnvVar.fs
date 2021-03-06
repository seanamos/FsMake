namespace FsMake

open System
open System.ComponentModel

/// <summary>
/// Module for working with environment variables.
/// </summary>
module EnvVar =

    /// <summary>
    /// The exception that is thrown when an environment variable with a given name could not be found.
    /// </summary>
    /// <param name="envVar">The name of the environment variable.</param>
    type NotFoundException(envVar: string) =
        inherit Exception($"Could not get a value for the environment variable \"{envVar}\"")

    /// <summary>
    /// Gets an environment variable by name.
    /// </summary>
    /// <param name="name">The name of the env var to get.</param>
    /// <returns>The value of the env var.</returns>
    /// <exception cref="T:EnvVar.NotFoundException">Thrown when an env var with the specified name does not exist.</exception>
    let get (name: string) : string =
        let var = Environment.GetEnvironmentVariable (name)

        if isNull var then
            NotFoundException (name) |> raise
        else
            var

    /// <summary>
    /// Gets an environment variable by name.
    /// </summary>
    /// <param name="name">The name of the env var to get.</param>
    /// <returns><c>Some</c> if env var exists, <c>None</c> if it does not exist.</returns>
    let getOption (name: string) : Option<string> =
        let var = Environment.GetEnvironmentVariable (name)

        if isNull var then None else Some var

    /// <summary>
    /// Gets an environment variable by name and converts it to the specified type.
    /// </summary>
    /// <param name="name">The name of the env var to get.</param>
    /// <typeparam name="'T">The type to convert the env var to.</typeparam>
    /// <returns>The converted env var.</returns>
    /// <exception cref="T:EnvVar.NotFoundException">Thrown when the env var does not exist.</exception>
    /// <exception cref="T:System.NotSupportedException">Thrown when the env var cannot be converted to the specified type.</exception>
    let getAs<'T when 'T: struct> (name: string) : 'T =
        let var = get name

        let converter = TypeDescriptor.GetConverter (typeof<'T>)
        converter.ConvertFromString (var) :?> 'T

    /// <summary>
    /// Gets an environment variable by name and converts it to the specified type.
    /// </summary>
    /// <param name="name">The name of the env var to get.</param>
    /// <typeparam name="'T">The type to convert the env var to.</typeparam>
    /// <returns><c>Some</c> if the env var exists and can be converted, <c>None</c> if it does not exist or cannot be converted.</returns>
    let getOptionAs<'T when 'T: struct> (name: string) =
        try
            let var = getAs<'T> name

            Some var
        with
        | _ -> None

    let internal someOrFail (envVar: string) (opt: 'T option) : Make<'T> =
        make {
            match opt with
            | Some x -> return x
            | None ->
                return!
                    [
                        Console.error "Could not get environment variable "
                        |> Console.appendToken envVar
                    ]
                    |> Make.failMessages
        }

    /// <summary>
    /// Gets an environment variable by name.
    /// <para>
    /// This creates a <see cref="T:Make`1" /> that can be used in a step.
    /// When unable to get the env var, the step will fail.
    /// </para>
    /// </summary>
    /// <param name="name">The name of the env var to get.</param>
    /// <returns>The env var if it exists, or an <c>Error</c> if it does not.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let getToken = EnvVar.getOrFail "GITHUB_TOKEN"
    ///
    /// let step = Step.create "release-notes" {
    ///     let! token = getToken
    ///     ...
    /// }
    /// </code>
    /// </example>
    let getOrFail (name: string) : Make<string> =
        let opt = getOption name

        opt |> someOrFail name

    /// <summary>
    /// Gets an environment variable by name and converts it to the specified type.
    /// <para>
    /// This creates a <see cref="T:Make`1" /> that can be used in a step.
    /// When unable to get the env var, the step will fail.
    /// </para>
    /// </summary>
    /// <param name="name">The name of the env var to get.</param>
    /// <typeparam name="'T">The type to convert the env var to.</typeparam>
    /// <returns>The env var if it exists and can be converted, an <c>Error</c> if it does not or cannot be converted.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let getMyBool = EnvVar.getAsOrFail&lt;bool&gt; "MY_BOOL"
    ///
    /// let step = Step.create "release-notes" {
    ///     let! myBool = getMyBool
    ///     ...
    /// }
    /// </code>
    /// </example>
    let getAsOrFail<'T when 'T: struct> (name: string) : Make<'T> =
        let opt = getOptionAs<'T> name

        opt |> someOrFail name

    /// <summary>
    /// Sets the specified environment variable.
    /// </summary>
    /// <param name="envVar">A pair representing the name and value of the environment variable.</param>
    /// <returns>A <see cref="T:unit" /> when complete.</returns>
    let set (envVar: string * string) : unit =
        envVar |> Environment.SetEnvironmentVariable

    /// <summary>
    /// Sets the specified <see cref="T:seq`1" /> of environment variables.
    /// </summary>
    /// <param name="envVars">A <see cref="T:seq`1" /> of pairs representing the names and values of the environment variables.</param>
    /// <returns>A <see cref="T:unit" /> when complete.</returns>
    let setMany (envVars: (string * string) seq) : unit =
        envVars |> Seq.iter set

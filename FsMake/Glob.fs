namespace FsMake

open System
open System.IO
open System.Text.RegularExpressions

/// <summary>
/// Represents a file/directory glob.
/// </summary>
type Glob =
    {
        /// <summary>
        /// Gets the root directory of the <see cref="T:Glob" />.
        /// </summary>
        RootDirectory: string

        /// <summary>
        /// Gets the include patterns.
        /// </summary>
        Include: string list

        /// <summary>
        /// Gets the exclude patterns.
        /// </summary>
        Exclude: string list
    }

/// <summary>
/// Module for creating and working with a <see cref="T:Glob" />.
/// <para>A <see cref="T:Glob" /> represents a set of patterns to include/exclude files.</para>
/// </summary>
/// <example>
/// <code lang="fsharp">
/// // search recursively for all files with a .dll file extension
/// let dlls = Glob.create "**/*.dll" |> Glob.toPaths
/// </code>
/// </example>
module Glob =
    [<AutoOpen>]
    module internal Internal =
        let defaultDirectory = lazy (Path.GetFullPath ".")

        let matchWildcardRegex = Regex (@"^.*(\*|\?).*$")

        let normalizePathSeperator (path: string) : string =
            path
                .Replace('\\', Path.DirectorySeparatorChar)
                .Replace ('/', Path.DirectorySeparatorChar)

        type PatternToken =
            | DirectoryToken of dir: string
            | DirectoryWildcardToken
            | FileOrDirectoryToken of token: string
            | FileOrDirectoryWildcardToken of pattern: string
            | RecursiveToken
            | RootDirectoryToken of dir: string
            | RootToken

        type ParsedPattern = private ParsedPattern of PatternToken list

        module ParsedPattern =
            let create (rootDir: string) (pattern: string) : ParsedPattern =
                let normalizedPattern = pattern |> normalizePathSeperator

                let initialParts =
                    if normalizedPattern.StartsWith (Path.DirectorySeparatorChar) then
                        [ RootToken ]
                    else
                        [ RootDirectoryToken rootDir ]

                let splitPattern =
                    normalizedPattern.Split ('/', StringSplitOptions.RemoveEmptyEntries)
                    |> List.ofArray

                let rec parseNext parts rem =
                    match rem with
                    | [] -> parts
                    | "**" :: xs -> xs |> parseNext (RecursiveToken :: parts)
                    | [ x ] when x = "*" -> FileOrDirectoryWildcardToken x :: parts
                    | "*" :: xs -> xs |> parseNext (DirectoryWildcardToken :: parts)
                    | [ x ] when matchWildcardRegex.IsMatch (x) -> FileOrDirectoryWildcardToken x :: parts
                    | [ x ] -> FileOrDirectoryToken x :: parts
                    | x :: xs -> xs |> parseNext (DirectoryToken x :: parts)

                splitPattern |> parseNext initialParts |> List.rev |> ParsedPattern

            let value (ParsedPattern value) : PatternToken list =
                value

            let toRegex (parsed: ParsedPattern) : Regex =
                let escape (str: string) =
                    Regex.Escape(str).Replace ("/", "\\/")

                let tokens = parsed |> value
                let seperator = Path.DirectorySeparatorChar.ToString () |> escape
                let starWildcardRegexStr = @"([^\\\/]*)"
                let recursiveRegexStr = ".*"

                let rec nextPart regex rem =
                    match rem with
                    | [] -> regex
                    | RootToken :: xs -> xs |> nextPart seperator
                    | RootDirectoryToken x :: xs -> xs |> nextPart $"{regex}{escape x}{seperator}"
                    | [ DirectoryToken x ] -> $"{regex}{escape x}"
                    | DirectoryToken x :: xs -> xs |> nextPart $"{regex}{escape x}{seperator}"
                    | [ DirectoryWildcardToken ] -> $"{regex}{starWildcardRegexStr}"
                    | DirectoryWildcardToken :: xs -> xs |> nextPart $"{regex}{starWildcardRegexStr}{seperator}"
                    | FileOrDirectoryToken x :: xs -> xs |> nextPart $"{regex}{escape x}"
                    | FileOrDirectoryWildcardToken x :: xs ->
                        let escaped = escape x

                        let wildcard = escaped.Replace(@"\?", ".").Replace (@"\*", ".*")

                        xs |> nextPart ($"{regex}{wildcard}")
                    | [ RecursiveToken ] -> $"{regex}{recursiveRegexStr}"
                    | RecursiveToken :: xs -> xs |> nextPart $"{regex}{recursiveRegexStr}{seperator}"

                let regex = tokens |> nextPart ""

                Regex ($"^{regex}$")

            let toPaths (parsed: ParsedPattern) : string list =
                let tokens = parsed |> value

                let rec nextToken paths remTokens =
                    match remTokens with
                    | [] -> paths
                    | RootToken :: xs -> xs |> nextToken (DirectoryInfo("/").FullName :: paths)
                    | RootDirectoryToken x :: xs -> xs |> nextToken (x :: paths)
                    | DirectoryToken x :: xs ->
                        let dirs =
                            paths
                            |> List.collect (fun dir ->
                                let path = Path.Combine (dir, x)
                                let di = DirectoryInfo (path)

                                if di.Exists then [ di.FullName ] else []
                            )

                        xs |> nextToken dirs
                    | DirectoryWildcardToken :: xs ->
                        let dirs =
                            paths
                            |> List.collect (fun dir ->
                                Directory.EnumerateDirectories (dir, "*", SearchOption.TopDirectoryOnly)
                                |> List.ofSeq
                            )

                        xs |> nextToken (paths @ dirs)
                    | FileOrDirectoryToken x :: xs ->
                        let dirs =
                            paths
                            |> List.collect (fun dir ->
                                let path = Path.Combine (dir, x)
                                let fi = FileInfo (path)
                                let di = DirectoryInfo (path)

                                match (fi.Exists, di.Exists) with
                                | (true, _) -> [ fi.FullName ]
                                | (_, true) -> [ di.FullName ]
                                | _ -> []
                            )

                        xs |> nextToken dirs
                    | FileOrDirectoryWildcardToken x :: xs ->
                        let files =
                            paths
                            |> List.collect (fun dir ->
                                Directory.EnumerateFileSystemEntries (dir, x, SearchOption.TopDirectoryOnly)
                                |> List.ofSeq
                            )

                        xs |> nextToken files
                    | [ RecursiveToken ] ->
                        let dirs =
                            paths
                            |> List.collect (fun dir ->
                                Directory.EnumerateFileSystemEntries (dir, "*", SearchOption.AllDirectories)
                                |> List.ofSeq
                            )

                        [] |> nextToken dirs
                    | RecursiveToken :: xs ->
                        let dirs =
                            paths
                            |> List.collect (fun dir ->
                                Directory.EnumerateDirectories (dir, "*", SearchOption.AllDirectories)
                                |> List.ofSeq
                            )

                        xs |> nextToken dirs

                tokens |> nextToken []

    /// <summary>
    /// Used to represent the type of path.
    /// </summary>
    type PathType =
        | File of path: string
        | Directory of path: string

    /// <summary>
    /// Creates a <see cref="T:Glob" /> from a pattern.
    /// </summary>
    /// <param name="pattern">The pattern of paths to include.</param>
    /// <returns>The new <see cref="T:Glob" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let dlls = Glob.create "**/*.dll"
    /// </code>
    /// </example>
    let create (pattern: string) : Glob =
        {
            RootDirectory = defaultDirectory.Value
            Include = [ pattern ]
            Exclude = []
        }

    /// <summary>
    /// Creates a <see cref="T:Glob" /> from a pattern with a root directory.
    /// The root directory is the path of a directory to start searching from.
    /// </summary>
    /// <param name="directory">The root directory.</param>
    /// <param name="pattern">The pattern of paths to include.</param>
    /// <returns>The new <see cref="T:Glob" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let logs = Glob.createWithRootDir "/tmp" "**/*.log"
    /// </code>
    /// </example>
    let createWithRootDir (directory: string) (pattern: string) : Glob =
        {
            RootDirectory = directory
            Include = [ pattern ]
            Exclude = []
        }

    /// <summary>
    /// Adds a pattern of paths to include to an existing <see cref="T:Glob" />.
    /// </summary>
    /// <param name="pattern">The pattern of paths to include.</param>
    /// <param name="glob">The <see cref="T:Glob" />.</param>
    /// <returns>An updated <see cref="T:Glob" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let cleanDirs =
    ///     Glob.create "**/bin"
    ///     |> Glob.add "**/obj"
    /// </code>
    /// </example>
    let add (pattern: string) (glob: Glob) : Glob =
        { glob with
            Include = pattern :: glob.Include
        }

    /// <summary>
    /// Adds a pattern of paths to exclude to an existing <see cref="T:Glob" />.
    /// </summary>
    /// <param name="pattern">The pattern of paths to exclude.</param>
    /// <param name="glob">The <see cref="T:Glob" />.</param>
    /// <returns>An updated <see cref="T:Glob" />.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let dlls =
    ///     Glob.create "**/*.dll"
    ///     |> Glob.exclude "exclude.dll"
    /// </code>
    /// </example>
    let exclude (pattern: string) (glob: Glob) : Glob =
        { glob with
            Exclude = pattern :: glob.Exclude
        }

    /// <summary>
    /// Creates a sequence of matched paths from a <see cref="T:Glob" />.
    /// </summary>
    /// <param name="glob">The <see cref="T:Glob" /> to create the paths from.</param>
    /// <returns>A sequence of matched paths.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// let dllPaths = Glob.create "**/*.dll" |> Glob.toPaths
    /// </code>
    /// </example>
    let toPaths (glob: Glob) : string seq =
        let rootDir = DirectoryInfo(glob.RootDirectory).FullName

        let includePatterns = glob.Include |> List.map (ParsedPattern.create rootDir)

        let excludePatternRegexes = glob.Exclude |> List.map (ParsedPattern.create rootDir >> ParsedPattern.toRegex)

        seq {
            for pattern in includePatterns do
                let paths = pattern |> ParsedPattern.toPaths

                for path in paths do
                    let excluded = excludePatternRegexes |> List.exists (fun x -> x.IsMatch (path))

                    if not excluded then yield path
        }

    /// <summary>
    /// Creates a sequence of <see cref="T:PathType" /> from a <see cref="T:Glob" />.
    /// </summary>
    /// <param name="glob">The <see cref="T:Glob" /> to create the paths from.</param>
    /// <returns>A sequence of matched paths.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// Glob.create "**/*Things*"
    /// |> Glob.toPathTypes
    /// |> Seq.iter
    ///     (function
    ///     | Glob.File x -> printfn "File path: %s" x
    ///     | Glob.Directory x -> printfn "Directory path: %s" x)
    /// </code>
    /// </example>
    let toPathTypes (glob: Glob) : PathType seq =
        glob
        |> toPaths
        |> Seq.map (fun x ->
            let attrs = File.GetAttributes (x)
            let isDirectory = (attrs &&& FileAttributes.Directory) = FileAttributes.Directory

            if isDirectory then Directory x else File x
        )

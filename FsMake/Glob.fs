namespace FsMake

open System
open System.IO
open System.Text.RegularExpressions

type Glob =
    { RootDirectory: string
      Include: string list
      Exclude: string list }

module Glob =
    [<AutoOpen>]
    module internal Internal =
        let defaultDirectory = lazy Path.GetFullPath "."

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

    type PathType =
        | File of path: string
        | Directory of path: string

    let create (pattern: string) : Glob =
        { RootDirectory = defaultDirectory.Value
          Include = [ pattern ]
          Exclude = [] }

    let createWithRootDir (directory: string) (pattern: string) : Glob =
        { RootDirectory = directory
          Include = [ pattern ]
          Exclude = [] }

    let add (pattern: string) (glob: Glob) : Glob =
        { glob with
              Include = pattern :: glob.Include }

    let exclude (pattern: string) (glob: Glob) : Glob =
        { glob with
              Exclude = pattern :: glob.Exclude }

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

    let toPathTypes (glob: Glob) : PathType seq =
        glob
        |> toPaths
        |> Seq.map (fun x ->
            let attrs = File.GetAttributes (x)
            let isDirectory = (attrs &&& FileAttributes.Directory) = FileAttributes.Directory

            if isDirectory then Directory x else File x
        )

module FsMake.Tests.Unit.Glob

open Expecto
open FsMake
open FsMake.Tests

let globTests =
    testList
        "Glob tests"
        [
            test "create adds include" {
                let glob = Glob.create "inc"

                teste <@ glob.Include = [ "inc" ] @>
            }

            test "createWithRootDir sets RootDirectory" {
                let glob = Glob.createWithRootDir "/test" "inc"

                teste <@ glob.RootDirectory = "/test" @>
            }

            test "add appends include" {
                let glob = Glob.create "inc1" |> Glob.add "inc2"

                teste <@ glob.Include = [ "inc2"; "inc1" ] @>
            }

            test "add multiple appends multiple includes" {
                let glob = Glob.create "inc1" |> Glob.add "inc2" |> Glob.add "inc3"

                teste <@ glob.Include = [ "inc3"; "inc2"; "inc1" ] @>
            }

            test "exclude appends exclude" {
                let glob = Glob.create "inc" |> Glob.exclude "ex"

                teste <@ glob.Exclude = [ "ex" ] @>
            }

            test "exclude multiple appends multiple excludes" {
                let glob = Glob.create "test" |> Glob.exclude "ex1" |> Glob.exclude "ex2"

                teste <@ glob.Exclude = [ "ex2"; "ex1" ] @>
            }
        ]

open FsMake.Glob.Internal

let parseTests =
    testList
        "Glob parse tests"
        [
            test "Root directory parsed" {
                let parsed = ParsedPattern.create "/test" "" |> ParsedPattern.value

                teste <@ parsed = [ RootDirectoryToken "/test" ] @>
            }

            test "Recursive wildcard parsed" {
                let parsed = ParsedPattern.create "/test" "**" |> ParsedPattern.value

                teste <@ parsed = [ RootDirectoryToken "/test"; RecursiveToken ] @>
            }

            test "Directory/file parsed" {
                let parsed = ParsedPattern.create "/test" "dir/file" |> ParsedPattern.value

                teste
                    <@ parsed = [
                        RootDirectoryToken "/test"
                        DirectoryToken "dir"
                        FileOrDirectoryToken "file"
                    ] @>
            }

            test "Directory/file wildcard token parsed" {
                let parsed = ParsedPattern.create "/test" "dir/*file" |> ParsedPattern.value

                teste
                    <@ parsed = [
                        RootDirectoryToken "/test"
                        DirectoryToken "dir"
                        FileOrDirectoryWildcardToken "*file"
                    ] @>
            }

            test "Directory wildcard token parsed" {
                let parsed = ParsedPattern.create "/test" "*/file" |> ParsedPattern.value

                teste
                    <@ parsed = [
                        RootDirectoryToken "/test"
                        DirectoryWildcardToken
                        FileOrDirectoryToken "file"
                    ] @>
            }

            test "Complex test" {
                let parsed = ParsedPattern.create "/test" "dir/*/**/dir2/*.ext" |> ParsedPattern.value

                teste
                    <@ parsed = [
                        RootDirectoryToken "/test"
                        DirectoryToken "dir"
                        DirectoryWildcardToken
                        RecursiveToken
                        DirectoryToken "dir2"
                        FileOrDirectoryWildcardToken "*.ext"
                    ] @>
            }
        ]

let regexTests =
    testList
        "Glob regex tests"
        [
            test "Directory matches" {
                let parsed = ParsedPattern.create "/tmp" "dir"
                printfn "%A" parsed
                let regex = parsed |> ParsedPattern.toRegex

                let path = "/tmp/dir" |> normalizePathSeperator
                teste <@ regex.IsMatch path @>
            }

            test "Directory does not match" {
                let parsed = ParsedPattern.create "/tmp" "dir"
                let regex = parsed |> ParsedPattern.toRegex

                let path = "/tmp/notmatch" |> normalizePathSeperator
                teste <@ not <| regex.IsMatch path @>
            }

            test "Recursive matches" {
                let parsed = ParsedPattern.create "/tmp" "dir/**/file"
                let regex = parsed |> ParsedPattern.toRegex

                let path = "/tmp/dir/dir2/dir3/file" |> normalizePathSeperator
                teste <@ regex.IsMatch path @>
            }

            test "Recursive does not match" {
                let parsed = ParsedPattern.create "/tmp" "dir/**/file"
                let regex = parsed |> ParsedPattern.toRegex

                let path = "/tmp/dir/dir2/dir3/file2" |> normalizePathSeperator
                teste <@ not <| regex.IsMatch path @>
            }

            test "File wildcard matches" {
                let parsed = ParsedPattern.create "/tmp" "dir/*.ext"
                let regex = parsed |> ParsedPattern.toRegex

                let path = "/tmp/dir/file.ext" |> normalizePathSeperator
                teste <@ regex.IsMatch path @>
            }

            test "File wildcard does not match" {
                let parsed = ParsedPattern.create "/tmp" "dir/*.ext"
                let regex = parsed |> ParsedPattern.toRegex

                let path = "/tmp/dir/file.txt" |> normalizePathSeperator
                teste <@ not <| regex.IsMatch path @>
            }

            test "Recursive file token matches" {
                let parsed = ParsedPattern.create "/tmp" "dir/**"
                let regex = parsed |> ParsedPattern.toRegex

                let path1 = "/tmp/dir/dir2/dir3/file.ext" |> normalizePathSeperator
                let path2 = "/tmp/dir/dir2/dir3/dir" |> normalizePathSeperator
                teste <@ regex.IsMatch path1 @>
                teste <@ regex.IsMatch path2 @>
            }

            test "Recursive file token does not match" {
                let parsed = ParsedPattern.create "/tmp" "dir/**"
                let regex = parsed |> ParsedPattern.toRegex

                let path = "/tmp/notmatch/dir/dir2/file.ext" |> normalizePathSeperator
                teste <@ not <| regex.IsMatch path @>
            }

            test "Complex match" {
                let parsed = ParsedPattern.create "/tmp" "dir/**/dir2/*/dir3/*.ext"
                let regex = parsed |> ParsedPattern.toRegex

                let path = "/tmp/dir/x/y/z/dir2/xyz/dir3/file.ext" |> normalizePathSeperator
                teste <@ regex.IsMatch path @>
            }
        ]

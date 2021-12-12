#!/bin/sh

set -e

dotnet tool restore
dotnet paket restore
dotnet restore

dotnet fsi build.fsx "$@"

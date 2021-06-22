namespace FsMake.UnitTests

open Swensen.Unquote

[<AutoOpen>]
module Core =
    // prevent name collision with Expecto.test
    let teste = test

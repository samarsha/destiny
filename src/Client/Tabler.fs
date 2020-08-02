module internal Destiny.Client.Tabler

open Fable.React.Helpers
open Fable.Core
open Fable.Core.JsInterop

type IconProperty =
    | Size of int
    | Color of string
    | StrokeWidth of float

let inline icon name (properties : IconProperty list) =
    ofImport name "tabler-icons-react" (keyValueList CaseRules.LowerFirst properties) []

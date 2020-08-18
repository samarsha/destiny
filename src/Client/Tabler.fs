module internal Destiny.Client.Tabler

open Fable.React.Helpers
open Fable.Core
open Fable.Core.JsInterop
open Fable.React.Props

type IconProperty =
    | Color of string
    | Size of int
    | StrokeWidth of float

type private Properties =
    { Color : string
      Size : int
      StrokeWidth : float }

let private fromList properties =
    let property f defaultValue = properties |> List.choose f |> List.tryHead |> Option.defaultValue defaultValue
    { Color = property (function Color color -> Some color | _ -> None) "currentColor"
      Size = property (function Size size -> Some size | _ -> None) 24
      StrokeWidth = property (function StrokeWidth width -> Some width | _ -> None) 2.0 }

let private svg properties =
    let props = fromList properties
    svgEl "svg"
        [ SVGAttr.Custom ("xmlns", "http://www.w3.org/2000/svg")
          Class "icon icon-tabler icon-tabler-closed-eye"
          SVGAttr.Width props.Size
          SVGAttr.Height props.Size
          ViewBox "0 0 24 24"
          SVGAttr.Stroke props.Color
          SVGAttr.StrokeWidth props.StrokeWidth
          SVGAttr.Fill "none"
          SVGAttr.StrokeLinecap "round"
          SVGAttr.Custom ("strokeLinejoin", "round") ]

let closedEye properties =
    svg properties
        [ svgEl "path" [ SVGAttr.Stroke "none"; D "M0 0h24v24H0z" ] []
          svgEl "path" [ D "M2 12l1.5 2a11 11 0 0 0 17 0l1.5 -2" ] []
          svgEl "path" [ D "M2 12l1.5 -2a11 11 0 0 1 17 0l1.5 2" ] [] ]

let filledStar properties =
    svg properties
        [ svgEl "path" [ SVGAttr.Stroke "none"; D "M0 0h24v24H0z" ] []
          svgEl "path"
              [ D "M12 17.75l-6.172 3.245 1.179-6.873-4.993-4.867 6.9-1.002L12 2l3.086 6.253 6.9 1.002-4.993 4.867 1.179 6.873z"
                SVGAttr.Fill "currentColor" ]
              [] ]

let inline icon name properties =
    match name with
    | "ClosedEye" -> closedEye properties
    | "FilledStar" -> filledStar properties
    | _ -> ofImport name "tabler-icons-react" (keyValueList CaseRules.LowerFirst properties) []

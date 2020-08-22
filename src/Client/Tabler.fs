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

let private width = 24

let private height = 24

let private svg properties =
    svgEl "svg"
        [ SVGAttr.Custom ("xmlns", "http://www.w3.org/2000/svg")
          Class "icon icon-tabler icon-tabler-closed-eye"
          SVGAttr.Width properties.Size
          SVGAttr.Height properties.Size
          ViewBox <| sprintf "0 0 %i %i" width height
          SVGAttr.Stroke properties.Color
          SVGAttr.StrokeWidth properties.StrokeWidth
          SVGAttr.Fill "none"
          SVGAttr.StrokeLinecap "round"
          SVGAttr.Custom ("strokeLinejoin", "round") ]

let closedEye properties =
    let properties' = fromList properties
    let strike strikeProperties =
        svgEl "path"
            ([ D "M 1.4693877,18.155103 C 22.307994,6.1239269 22.551877,5.9831211 22.551877,5.9831211" ]
             @ strikeProperties)
            []
    svg properties'
        [ svgEl "mask"
              [ Id "strike" ]
              [ svgEl "rect" [ X 0; Y 0; SVGAttr.Width width; SVGAttr.Height height; SVGAttr.Fill "white" ] []
                strike [ SVGAttr.StrokeWidth (properties'.StrokeWidth + 4.0); SVGAttr.Stroke "black" ] ]
          svgEl "path" [ SVGAttr.Stroke "none"; D "M0 0h24v24H0z" ] []
          svgEl "circle" [ Cx 12; Cy 12; R 3; SVGAttr.Custom ("mask", "url(#strike)") ] []
          svgEl "path" [ D "M2 12l1.5 2a11 11 0 0 0 17 0l1.5 -2"; SVGAttr.Custom ("mask", "url(#strike)") ] []
          svgEl "path" [ D "M2 12l1.5 -2a11 11 0 0 1 17 0l1.5 2"; SVGAttr.Custom ("mask", "url(#strike)") ] []
          strike [] ]

let filledStar properties =
    svg (fromList properties)
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

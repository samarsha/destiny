module Destiny.Message exposing (empty, view)

import Destiny.Generated.Model exposing (Invoke, Message (..), MessageList)
import Dict.Any
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Uuid

empty : MessageList
empty =
  { ids = []
  , messages = Dict.Any.empty Uuid.toString
  }

view : Message -> Html msg
view message = case message of
  RollMessage roll -> 
    let
      baseDiv = div [ class "roll-line" ]
        [ viewDie roll.statResult
        , " + " ++ String.fromInt roll.statModifier |> text
        , viewAnnotation roll.statName
        ]
      invokeDivs = roll.invokes |> List.map viewInvoke
      total = roll.statResult + roll.statModifier + (roll.invokes |> List.map .result |> List.sum)
      totalDiv = div [] [ " = " ++ String.fromInt total |> text ]
    in
      div [ class "roll" ] <| baseDiv :: invokeDivs ++ [ totalDiv ]

viewInvoke : Invoke -> Html msg
viewInvoke invoke = div [ class "roll-line" ]
  [ text " + "
  , viewDie invoke.result
  , viewAnnotation invoke.source
  ]

viewDie : Int -> Html msg
viewDie die =
  let
    classes = case die of
      1 -> [ class "die", class "die-bad" ]
      6 -> [ class "die", class "die-good" ]
      _ -> [ class "die" ]
  in
    [ die |> String.fromInt |> text ] |> Html.span classes

viewAnnotation : String -> Html msg
viewAnnotation name = span [ class "annotation" ] [ text name ]

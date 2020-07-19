module Destiny.Message exposing (empty, view)

import Destiny.Generated.Message exposing (Invoke, Message (..), MessageList (..))
import Destiny.Generated.Scene exposing (Role (..))
import Dict.Any
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Uuid

empty : MessageList
empty = MessageList [] <| Dict.Any.empty Uuid.toString

view : Message -> Html msg
view (RollMessage roll) =
  let
    baseDiv = div [ class "roll-line" ]
      [ viewDie roll.statResult roll.role
      , " + " ++ String.fromInt roll.statModifier |> text
      , viewAnnotation roll.statName
      ]
    invokeDivs = roll.invokes |> List.map viewInvoke
    total =
      roll.statResult +
      roll.statModifier +
      (roll.invokes |> List.map .result |> List.sum)
    totalDiv = div [] [ " = " ++ String.fromInt total |> text ]
  in
    div [ class "roll" ] <| baseDiv :: invokeDivs ++ [ totalDiv ]

viewInvoke : Invoke -> Html msg
viewInvoke invoke = div
  [ class "roll-line" ]
  [ text " + "
  , viewDie invoke.result invoke.role
  , viewAnnotation invoke.source
  ]

viewDie : Int -> Role -> Html msg
viewDie result role =
  let
    resultClasses = case result of
      1 -> [ class "die", class "die-bad" ]
      6 -> [ class "die", class "die-good" ]
      _ -> [ class "die" ]
    roleClasses = case role of
      Player -> [ class "die-player" ]
      DM -> [ class "die-dm" ]
  in
    [ result |> String.fromInt |> text ]
    |> Html.span (resultClasses ++ roleClasses)

viewAnnotation : String -> Html msg
viewAnnotation name = span [ class "annotation" ] [ text name ]

port module Main exposing (main)

import Browser
import Destiny.Generated.Model exposing
  ( Aspect
  , ClientRequest (..)
  , Entity
  , World
  , jsonDecWorld
  , jsonEncClientRequest
  )
import Html exposing (Html, button, div, input, text, textarea)
import Html.Attributes exposing (checked, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode exposing (Value, decodeValue)

type Message
  = UpdateWorld World
  | DecodeError Json.Decode.Error
  | Request ClientRequest

port receive : (Value -> msg) -> Sub msg

port send : Value -> Cmd msg

main : Program () World Message
main =
  Browser.element
    { init = always ({ entities = [], lastRoll = 0 }, Cmd.none)
    , update = update
    , subscriptions = always receiveWorld
    , view = view
    }

update : Message -> World -> (World, Cmd Message)
update message world =
  case message of
    UpdateWorld newWorld -> (newWorld, Cmd.none)
    DecodeError _ -> (world, Cmd.none)
    Request request ->
      let
        newWorld =
          case request of
            EditAspect aspect -> updateAspect aspect world
            _ -> world
      in
        (newWorld, jsonEncClientRequest request |> send)

receiveWorld : Sub Message
receiveWorld =
  let
    decode value =
      case decodeValue jsonDecWorld value of
        Ok world -> UpdateWorld world
        Err error -> DecodeError error
  in
    receive decode

updateAspect : Aspect -> World -> World
updateAspect aspect world =
  let
    replaceAspect originalAspect =
      if originalAspect.id == aspect.id then aspect
      else originalAspect
    updateEntityAspects entity = { entity | aspects = List.map replaceAspect entity.aspects }
  in
    { world | entities = List.map updateEntityAspects world.entities }

view : World -> Html Message
view world =
  div []
    [ text ("Rolled: " ++ String.fromInt world.lastRoll)
    , button [ onClick (Request AddEntity) ] [ text "+" ]
    , div [] (List.map viewEntity world.entities)
    ]

viewEntity : Entity -> Html Message
viewEntity entity =
  List.append
    [ text "Entity"
    , button [ onClick (ToggleEntity entity |> Request) ]
             [ text (if entity.collapsed then "Show" else "Hide") ]
    , button [ onClick (RemoveEntity entity |> Request) ] [ text "Remove" ]
    , button [ onClick (AddAspect entity |> Request) ] [ text "+" ]
    ]
    ( if entity.collapsed then []
      else List.map viewAspect entity.aspects
    )
  |> div []

viewAspect : Aspect -> Html Message
viewAspect aspect =
  let
    die index selected =
      input
        [ type_ "checkbox"
        , checked selected
        , onCheck (ToggleDie aspect index >> Request)
        ]
        []
    edit text = EditAspect { aspect | text = text } |> Request
  in
    List.concat
      [ [ textarea [ value aspect.text, onInput edit ] []
        , button [ onClick (RemoveAspect aspect |> Request) ] [ text "Remove" ]
        , button [ onClick (AddDie aspect |> Request) ] [ text "+" ]
        , button [ onClick (RemoveDie aspect |> Request) ] [ text "-" ]
        ]
      , List.indexedMap die aspect.dice
      , [ button [ onClick (Roll aspect |> Request) ] [ text "Roll" ] ]
      ]
    |> div []

port module Main exposing (main)

import Browser
import Destiny.Drag as Drag
import Destiny.Generated.Model exposing
  ( ClientRequest (..)
  , Message (..)
  , MessageList (..)
  , WorldSnapshot
  , jsonDecWorldSnapshot
  )
import Destiny.Message as Message
import Destiny.Scene as Scene
import Destiny.Utils exposing (joinedMap)
import Dict.Any
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed
import Json.Decode
import Maybe.Extra
import Uuid exposing (Uuid)

type alias Model =
  { scene : Scene.Model
  , messages : MessageList
  }

type Event
  = UpdateWorld WorldSnapshot
  | DecodeError Json.Decode.Error
  | SceneEvent Scene.Event

port receive : (Json.Decode.Value -> msg) -> Sub msg

port send : Json.Decode.Value -> Cmd msg

port drag : (Json.Decode.Value -> msg) -> Sub msg

main : Program () Model Event
main = Browser.element
  { init = always
      ( { scene = Scene.empty, messages = Message.empty }
      , Cmd.none
      )
  , update = update
  , subscriptions = always <| Sub.batch
      [ receive <| decodeEvent jsonDecWorldSnapshot UpdateWorld
      , drag <| decodeEvent Drag.eventDecoder (Scene.Drag >> SceneEvent)
      ]
  , view = view
  }

decodeEvent : Json.Decode.Decoder a -> (a -> Event) -> Json.Decode.Value -> Event
decodeEvent decoder toEvent value = case Json.Decode.decodeValue decoder value of
  Ok result -> toEvent result
  Err error -> DecodeError error

update : Event -> Model -> (Model, Cmd Event)
update message model = case message of
  UpdateWorld newWorld ->
    let
      sceneModel = model.scene
      newSceneModel = { sceneModel | scene = newWorld.scene }
      newModel = { model | scene = newSceneModel, messages = newWorld.messages }
    in
      (newModel, Cmd.none)
  DecodeError error -> "Decoding error: " ++ Json.Decode.errorToString error |> Debug.todo
  SceneEvent event ->
    let (newScene, cmd) = Scene.update send model.scene event
    in ({ model | scene = newScene }, cmd |> Cmd.map SceneEvent)

view : Model -> Html Event
view model =
  let
    entityElement entity =
      ( Uuid.toString entity.id
      , Scene.viewEntity model.scene entity
        |> Html.map SceneEvent
      )
    (messageIds, messageMap) = case model.messages of
      MessageList ids map -> (ids, map)
  in
    (Maybe.Extra.toList <| viewRollBar <| Maybe.Extra.isJust model.scene.rolling) ++
    div [ class "main" ]
      [ div [ class "board" ]
          [ button [ onClick <| SceneEvent <| Scene.Request AddEntity ] [ text "+" ]
          , button [ onClick <| SceneEvent <| Scene.Request Undo ] [ text "Undo" ]
          , button [ onClick <| SceneEvent <| Scene.Request Redo ] [ text "Redo" ]
          , Html.Keyed.node "div" [ class "entities" ] <| joinedMap entityElement
              model.scene.scene.entities
              model.scene.scene.board
          ]
      , div [ class "messages" ] <| joinedMap Message.view messageMap messageIds
      ]
    :: (model.scene.drag
        |> Drag.view (viewDrag model.scene)
        |> Maybe.Extra.toList)
    |> div [ class "app" ]

viewRollBar : Bool -> Maybe (Html Event)
viewRollBar isRolling =
  if isRolling
  then
    Just <| div
      [ class "active-roll" ]
      [ text "You're on a roll!"
      , button [ onClick <| SceneEvent <| Scene.EndRoll ] [ text "Finish" ]
      ]
  else Nothing

viewDrag : Scene.Model -> Uuid -> Html Event
viewDrag model id =
  let
    entity =
      Dict.Any.get id model.scene.entities
      |> Maybe.map (withoutDrag model |> Scene.viewEntity)
    aspect =
      Dict.Any.get id model.scene.aspects
      |> Maybe.map (withoutDrag model |> Scene.viewAspect)
    invalid () = "Invalid ID: " ++ Uuid.toString id |> Debug.todo
  in
    entity
    |> Maybe.Extra.orElse aspect
    |> Maybe.Extra.unpack invalid (Html.map SceneEvent)

withoutDrag : Scene.Model -> Scene.Model
withoutDrag model =
  let
    dragModel = model.drag
    newDragModel = { dragModel | dragging = Nothing }
  in
    { model | drag = newDragModel }

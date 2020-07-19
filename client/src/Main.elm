port module Main exposing (main)

import Browser
import Destiny.Drag as Drag
import Destiny.Generated.Message exposing (Message (..), MessageList (..))
import Destiny.Generated.Server as Server exposing (ServerCommand (..), jsonEncServerCommand)
import Destiny.Generated.Scene exposing (Role (..))
import Destiny.Generated.World as World exposing (Snapshot, jsonDecSnapshot)
import Destiny.Message as Message
import Destiny.Scene as Scene
import Destiny.Utils exposing (joinedMap)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (class, checked, type_)
import Html.Events exposing (onClick)
import Json.Decode
import Maybe.Extra

type alias Model =
  { scene : Scene.Model
  , messages : MessageList
  , role : Role
  }

type Event
  = UpdateWorld Snapshot
  | DecodeError Json.Decode.Error
  | ServerCommand ServerCommand
  | SceneEvent Scene.Event

port receive : (Json.Decode.Value -> msg) -> Sub msg

port send : Json.Decode.Value -> Cmd msg

port drag : (Json.Decode.Value -> msg) -> Sub msg

main : Program () Model Event
main = Browser.element
  { init = always
      ( { scene = Scene.empty, messages = Message.empty, role = Player }
      , Cmd.none
      )
  , update = update
  , subscriptions = always <| Sub.batch
      [ receive <| decodeEvent jsonDecSnapshot UpdateWorld
      , drag <| decodeEvent Drag.eventDecoder (Scene.drag >> SceneEvent)
      ]
  , view = view
  }

decodeEvent : Json.Decode.Decoder a -> (a -> Event) -> Json.Decode.Value -> Event
decodeEvent decoder toEvent value = case Json.Decode.decodeValue decoder value of
  Ok result -> toEvent result
  Err error -> DecodeError error

update : Event -> Model -> (Model, Cmd Event)
update message model = case message of
  UpdateWorld world ->
    ( { model
      | scene = Scene.replace model.scene world.scene
      , messages = world.messages
      }
    , Cmd.none
    )
  DecodeError error -> "Decoding error: " ++ Json.Decode.errorToString error |> Debug.todo
  ServerCommand command ->
    let
      newModel = case command of
        SetRole role -> { model | scene = Scene.setRole model.scene role, role = role }
        _ -> model
    in
      (newModel, sendServerCommand command)
  SceneEvent event ->
    let (newScene, cmd) = Scene.update sendWorldCommand model.scene event
    in ({ model | scene = newScene }, cmd |> Cmd.map SceneEvent)

sendServerCommand : ServerCommand -> Cmd msg
sendServerCommand = jsonEncServerCommand >> send

sendWorldCommand : World.Command -> Cmd msg
sendWorldCommand = Server.WorldCommand >> sendServerCommand

view : Model -> Html Event
view model =
  let
    (messageIds, messageMap) = case model.messages of MessageList ids map -> (ids, map)
    dmCheckbox = label []
      [ input
          [ type_ "checkbox"
          , checked <| model.role == DM
          , onClick <| ServerCommand <| SetRole <| flipRole model.role
          ]
          []
      , text "DM"
      ]
  in
    ( Scene.viewRollBar model.scene
      |> Maybe.map (Html.map SceneEvent)
      |> Maybe.Extra.toList
    )
    ++
    div [ class "main" ]
      [ div [ class "board" ]
          [ dmCheckbox
          , Scene.viewBoard model.scene |> Html.map SceneEvent
          ]
      , div [ class "messages" ] <| joinedMap Message.view messageMap messageIds
      ]
    ::
    ( Scene.viewDrag model.scene 
      |> Maybe.map (Html.map SceneEvent)
      |> Maybe.Extra.toList
    )
    |> div [ class "app" ]

flipRole : Role -> Role
flipRole role = case role of
  Player -> DM
  DM -> Player

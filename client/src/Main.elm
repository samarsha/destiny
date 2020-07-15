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
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode
import Maybe.Extra

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
  SceneEvent event ->
    let (newScene, cmd) = Scene.update send model.scene event
    in ({ model | scene = newScene }, cmd |> Cmd.map SceneEvent)

view : Model -> Html Event
view model =
  let
    (messageIds, messageMap) = case model.messages of
      MessageList ids map -> (ids, map)
  in
    ( Scene.viewRollBar model.scene
      |> Maybe.map (Html.map SceneEvent)
      |> Maybe.Extra.toList
    )
    ++ div [ class "main" ]
      [ Scene.viewBoard model.scene |> Html.map SceneEvent
      , div [ class "messages" ] <| joinedMap Message.view messageMap messageIds
      ]
    :: ( Scene.viewDrag model.scene 
         |> Maybe.map (Html.map SceneEvent)
         |> Maybe.Extra.toList
       )
    |> div [ class "app" ]

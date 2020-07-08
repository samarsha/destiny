port module Main exposing (main)

import Browser
import Destiny.Drag as Drag
import Destiny.Generated.Model exposing
  ( ClientRequest (..)
  , Message (..)
  , MessageId
  , Scene
  , Stat (..)
  , StatGroup (..)
  , StatId
  , WorldSnapshot
  , jsonDecWorldSnapshot
  , jsonEncClientRequest
  )
import Destiny.Message as Message
import Destiny.Scene as Scene
import Destiny.Utils exposing (joinedMap)
import Dict.Any
import Flip exposing (flip)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed
import Json.Decode
import Maybe.Extra
import Random
import Uuid exposing (Uuid)

type alias ClientState =
  { world : WorldSnapshot
  , activeRoll : Maybe MessageId
  , drag : Drag.State
  }

type Event
  = UpdateWorld WorldSnapshot
  | DecodeError Json.Decode.Error
  | StartRoll StatId MessageId
  | EndRoll
  | Request ClientRequest
  | SceneEvent Scene.Event

port receive : (Json.Decode.Value -> msg) -> Sub msg

port send : Json.Decode.Value -> Cmd msg

port drag : (Json.Decode.Value -> msg) -> Sub msg

main : Program () ClientState Event
main = Browser.element
  { init = always
      ( { world = emptyWorld, activeRoll = Nothing, drag = Drag.emptyState }
      , Cmd.none
      )
  , update = update
  , subscriptions = always <| Sub.batch
      [ receive <| decodeEvent jsonDecWorldSnapshot UpdateWorld
      , drag <| decodeEvent Drag.eventDecoder (Scene.Drag >> SceneEvent)
      ]
  , view = view
  }

emptyWorld : WorldSnapshot
emptyWorld =
  { scene = Scene.empty
  , messages = Message.empty
  }

decodeEvent : Json.Decode.Decoder a -> (a -> Event) -> Json.Decode.Value -> Event
decodeEvent decoder toEvent value =
  case Json.Decode.decodeValue decoder value of
    Ok result -> toEvent result
    Err error -> DecodeError error

update : Event -> ClientState -> (ClientState, Cmd Event)
update message model = case message of
  UpdateWorld newWorld -> ({ model | world = newWorld }, Cmd.none)
  DecodeError error -> "Decoding error: " ++ Json.Decode.errorToString error |> Debug.todo
  StartRoll statId rollId ->
    let cmd = RollStat statId rollId |> jsonEncClientRequest |> send
    in ({ model | activeRoll = Just rollId }, cmd)
  EndRoll -> ({ model | activeRoll = Nothing }, Cmd.none)
  Request request -> handleRequest request model
  SceneEvent sceneEvent -> handleSceneEvent sceneEvent model

handleRequest : ClientRequest -> ClientState -> (ClientState, Cmd Event)
handleRequest request model =
  let
    newWorld = case request of
      SetEntityName id name -> updateScene
        (Scene.updateEntity (\entity -> { entity | name = name }) id)
        model.world
      SetStatGroupName id name -> updateScene
        (Scene.updateStatGroup (\(StatGroup sgid _ stats) -> StatGroup sgid name stats) id)
        model.world
      SetStatName id name -> updateScene
        (Scene.updateStat (\(Stat sid _ score) -> Stat sid name score) id)
        model.world
      SetAspectText id text -> updateScene
        (Scene.updateAspect (\aspect -> { aspect | text = text }) id)
        model.world
      MoveEntity id index -> updateScene (Scene.moveEntity id index) model.world
      _ -> model.world
  in
    ({ model | world = newWorld }, jsonEncClientRequest request |> send)

handleSceneEvent : Scene.Event -> ClientState -> (ClientState, Cmd Event)
handleSceneEvent event state = case event of
  Scene.Request request -> update (Request request) state
  Scene.Drag dragEvent -> handleDragEvent dragEvent state
  Scene.GenerateRollId statId -> (state, Uuid.uuidGenerator |> Random.generate (StartRoll statId))
  Scene.ContinueRoll aspectId ->
    let
      cmd = case state.activeRoll of
        Just rollId -> RollAspect aspectId rollId |> jsonEncClientRequest |> send
        Nothing -> Cmd.none
    in (state, cmd)

handleDragEvent : Drag.Event -> ClientState -> (ClientState, Cmd Event)
handleDragEvent event state =
  let newDrag = Drag.update event state.drag
  in case newDrag.dragging |> Maybe.andThen (Scene.get state.world.scene) of
    Just (Scene.EntityObject _) -> dragEntity { state | drag = newDrag }
    Just (Scene.AspectObject _) -> dragAspect { state | drag = newDrag }
    _ -> ({ state | drag = newDrag }, Cmd.none)

dragEntity : ClientState -> (ClientState, Cmd Event)
dragEntity state =
  let
    targetIndex =
      state.drag.targets
      |> List.filterMap (Scene.entityIndex state.world.scene)
      |> List.head
  in case (state.drag.dragging, targetIndex) of
    (Just id, Just index) -> update (MoveEntity id index |> Request) state
    _ -> (state, Cmd.none)

dragAspect : ClientState -> (ClientState, Cmd Event)
dragAspect state =
  let
    aspectTarget =
      state.drag.targets
      |> List.filterMap (flip Dict.Any.get state.world.scene.aspects)
      |> List.head
    entityTarget =
      state.drag.targets
      |> List.filterMap (flip Dict.Any.get state.world.scene.entities)
      |> List.head
  in case (state.drag.dragging, aspectTarget, entityTarget) of
    (Just id, Just target, Just parent) ->
      case Scene.aspectIndex state.world.scene parent.id target.id of
        Just index ->
          -- TODO: Move to index.
          (state, Cmd.none)
        Nothing -> (state, Cmd.none)
    (Just id, Nothing, Just parent) ->
      -- TODO: Move to index 0.
      (state, Cmd.none)
    _ -> (state, Cmd.none)

updateScene : (Scene -> Scene) -> WorldSnapshot -> WorldSnapshot
updateScene f world = { world | scene = f world.scene }

view : ClientState -> Html Event
view model =
  let
    rolling = Maybe.Extra.isJust model.activeRoll
    entityElement entity =
      ( Uuid.toString entity.id
      , Scene.viewEntity model.world.scene rolling model.drag.dragging entity
        |> Html.map SceneEvent
      )
  in
    (Maybe.Extra.toList <| viewRollBar rolling) ++
    div [ class "main" ]
      [ div [ class "board" ]
          [ button [ onClick <| Request AddEntity ] [ text "+" ]
          , button [ onClick <| Request Undo ] [ text "Undo" ]
          , button [ onClick <| Request Redo ] [ text "Redo" ]
          , Html.Keyed.node "div" [ class "entities" ] <| joinedMap entityElement
              model.world.scene.entities
              model.world.scene.board
          ]
      , div [ class "messages" ] <| joinedMap Message.view
          model.world.messages.map
          model.world.messages.list
      ]
    :: (model.drag
        |> Drag.view (viewDrag model.world.scene rolling)
        |> Maybe.Extra.toList)
    |> div [ class "app" ]

viewRollBar : Bool -> Maybe (Html Event)
viewRollBar rolling =
  if rolling
  then
    Just <| div
      [ class "active-roll" ]
      [ text "You're on a roll!", button [ onClick EndRoll ] [ text "Finish" ] ]
  else Nothing

viewDrag : Scene -> Bool -> Uuid -> Html Event
viewDrag scene rolling id =
  case Dict.Any.get id scene.entities of
    Just entity -> Scene.viewEntity scene rolling Nothing entity |> Html.map SceneEvent
    Nothing -> case Dict.Any.get id scene.aspects of
      Just aspect -> Scene.viewAspect rolling Nothing aspect |> Html.map SceneEvent
      Nothing -> "Invalid ID: " ++ Uuid.toString id |> Debug.todo

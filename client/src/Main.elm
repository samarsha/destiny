port module Main exposing (main)

import Browser
import Destiny.Generated.Model exposing
  ( Aspect
  , AspectId
  , ClientRequest (..)
  , Entity
  , EntityId
  , Message (..)
  , MessageId
  , MessageList
  , Scene
  , Stat (..)
  , StatGroup (..)
  , StatGroupId
  , StatId
  , WorldSnapshot
  , jsonDecEntityId
  , jsonDecWorldSnapshot
  , jsonEncClientRequest
  )
import Destiny.Message exposing (viewMessage)
import Dict
import Dict.Any exposing (AnyDict)
import Html exposing (Html, button, div, input, text, textarea)
import Html.Attributes exposing (attribute, class, disabled, placeholder, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Html.Keyed
import Json.Decode as Decode
import Json.Helpers exposing (decodeSumObjectWithSingleField)
import Maybe.Extra
import Random
import Uuid exposing (Uuid)

type alias ClientState =
  { world : WorldSnapshot
  , activeRoll : Maybe MessageId
  , dragObject : Maybe EntityId
  , dragOffset : Maybe Position
  , dragPosition : Maybe Position
  }

type Event
  = UpdateWorld WorldSnapshot
  | DecodeError Decode.Error
  | Request ClientRequest
  | Drag DragEvent
  | GenerateRollId StatId
  | StartRoll StatId MessageId
  | ContinueRoll AspectId
  | EndRoll

type DragEvent
  = DragPrepare EntityId
  | DragStart Position
  | DragMove Position (List Draggable)
  | DragEnd

type alias Draggable =
  { id : Uuid
  , region : Rectangle
  }

type DragStatus
  = DragWaiting
  | DragRemoved
  | DragDragging

type alias Position =
  { x : Float
  , y : Float
  }

type alias Rectangle =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  }

port receive : (Decode.Value -> msg) -> Sub msg

port send : Decode.Value -> Cmd msg

port drag : (Decode.Value -> msg) -> Sub msg

main : Program () ClientState Event
main =
  Browser.element
    { init = always
        ( { world = emptyWorld
          , activeRoll = Nothing
          , dragObject = Nothing
          , dragOffset = Nothing
          , dragPosition = Nothing
          }
        , Cmd.none
        )
    , update = update
    , subscriptions = always <| Sub.batch
        [ receive <| decodeEvent jsonDecWorldSnapshot UpdateWorld
        , drag <| decodeEvent dragMessageDecoder Drag
        ]
    , view = view
    }

emptyWorld : WorldSnapshot
emptyWorld =
  { scene = emptyScene
  , messages = emptyMessages
  }

emptyScene : Scene
emptyScene =
  { board = []
  , entities = Dict.Any.empty Uuid.toString
  , statGroups = Dict.Any.empty Uuid.toString
  , stats = Dict.Any.empty Uuid.toString
  , aspects = Dict.Any.empty Uuid.toString
  }

emptyMessages : MessageList
emptyMessages =
  { list = []
  , map = Dict.Any.empty Uuid.toString
  }

decodeEvent : Decode.Decoder a -> (a -> Event) -> Decode.Value -> Event
decodeEvent decoder toEvent value =
  case Decode.decodeValue decoder value of
    Ok result -> toEvent result
    Err error -> DecodeError error

update : Event -> ClientState -> (ClientState, Cmd Event)
update message model =
  case message of
    UpdateWorld newWorld -> ({ model | world = newWorld }, Cmd.none)
    DecodeError error -> "Decoding error: " ++ Decode.errorToString error |> Debug.todo
    Request request -> handleRequest request model
    Drag event -> handleDrag event model
    GenerateRollId statId -> (model, Uuid.uuidGenerator |> Random.generate (StartRoll statId))
    StartRoll statId rollId ->
      let cmd = RollStat statId rollId |> jsonEncClientRequest |> send
      in ({ model | activeRoll = Just rollId }, cmd)
    ContinueRoll aspectId ->
      let
        cmd = case model.activeRoll of
          Just rollId -> RollAspect aspectId rollId |> jsonEncClientRequest |> send
          Nothing -> Cmd.none
      in (model, cmd)
    EndRoll -> ({ model | activeRoll = Nothing }, Cmd.none)

modifyScene : (Scene -> Scene) -> WorldSnapshot -> WorldSnapshot
modifyScene f world = { world | scene = f world.scene }

modifyEntity : (Entity -> Entity) -> EntityId -> WorldSnapshot -> WorldSnapshot
modifyEntity f id = modifyScene <| \scene ->
  { scene | entities = Dict.Any.update id (Maybe.map f) scene.entities }

modifyStatGroup : (StatGroup -> StatGroup) -> StatGroupId -> WorldSnapshot -> WorldSnapshot
modifyStatGroup f id = modifyScene <| \scene ->
  { scene | statGroups = Dict.Any.update id (Maybe.map f) scene.statGroups }

modifyStat : (Stat -> Stat) -> StatId -> WorldSnapshot -> WorldSnapshot
modifyStat f id = modifyScene <| \scene ->
  { scene | stats = Dict.Any.update id (Maybe.map f) scene.stats }

modifyAspect : (Aspect -> Aspect) -> AspectId -> WorldSnapshot -> WorldSnapshot
modifyAspect f id = modifyScene <| \scene ->
  { scene | aspects = Dict.Any.update id (Maybe.map f) scene.aspects }

handleRequest : ClientRequest -> ClientState -> (ClientState, Cmd Event)
handleRequest request model =
  let
    newWorld = case request of
      SetEntityName id name ->
        modifyEntity (\entity -> { entity | name = name }) id model.world
      SetStatGroupName id name ->
        modifyStatGroup (\(StatGroup sgid _ stats) -> StatGroup sgid name stats) id model.world
      SetStatName id name ->
        modifyStat (\(Stat sid _ score) -> Stat sid name score) id model.world
      SetAspectText id text ->
        modifyAspect (\aspect -> { aspect | text = text }) id model.world
      _ -> model.world
  in
    ({ model | world = newWorld }, jsonEncClientRequest request |> send)

handleDrag : DragEvent -> ClientState -> (ClientState, Cmd Event)
handleDrag event model =
  case event of
    DragPrepare entity -> ({ model | dragObject = Just entity }, Cmd.none)
    DragStart offset -> ({ model | dragOffset = Just offset }, Cmd.none)
    DragMove position draggables -> moveDragObject position draggables model
    DragEnd ->
      ({ model | dragObject = Nothing, dragOffset = Nothing, dragPosition = Nothing }, Cmd.none)

moveDragObject : Position -> List Draggable -> ClientState -> (ClientState, Cmd Event)
moveDragObject position draggables model =
  let
    currentIndex = model.dragObject |> Maybe.andThen (entityIndex model)
    newIndex =
      draggables
      |> List.filter (.region >> withinRectangle position)
      |> List.filterMap (.id >> entityIndex model)
      |> List.head
    newModel = { model | dragPosition = Just position }
  in
    case (model.dragObject, newIndex) of
      (Just entityId, Just index) ->
        if newIndex /= currentIndex
        then update
          (MoveEntity entityId index |> Request)
          { model | world = moveEntity entityId index newModel.world }
        else (newModel, Cmd.none)
      _ -> (newModel, Cmd.none)

moveEntity : EntityId -> Int -> WorldSnapshot -> WorldSnapshot
moveEntity id index world =
  let
    scene = world.scene
    removed = List.filter ((/=) id) scene.board
    moved = List.take index removed ++ id :: List.drop index removed
  in
    { world | scene = { scene | board = moved } }

joinedMap : (v -> a) -> AnyDict comparable k v -> List k -> List a
joinedMap f dict = List.filterMap (\key -> Dict.Any.get key dict |> Maybe.map f)

view : ClientState -> Html Event
view model =
  let
    dragStatus id =
      case (model.dragObject, model.dragPosition) of
        (Just dragId, Just _) -> if dragId == id then DragRemoved else DragWaiting
        _ -> DragWaiting
    rolling = Maybe.Extra.isJust model.activeRoll
    entityElement entity =
      ( Uuid.toString entity.id
      , viewEntity model.world.scene rolling (dragStatus entity.id) entity
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
      , div [ class "messages" ] <| joinedMap viewMessage
          model.world.messages.map
          model.world.messages.list
      ]
    :: (viewDragBox model |> Maybe.Extra.toList)
    |> div [ class "app" ]

viewRollBar : Bool -> Maybe (Html Event)
viewRollBar rolling =
  if rolling
  then
    Just <| div
      [ class "active-roll" ]
      [ text "You're on a roll!", button [ onClick EndRoll ] [ text "Finish" ] ]
  else Nothing

viewDragBox : ClientState -> Maybe (Html Event)
viewDragBox model =
  let
    box position offset entity = div
      [ class "dragging"
      , style "position" "absolute"
      , style "left" <| String.fromFloat (position.x - offset.x) ++ "px"
      , style "top" <| String.fromFloat (position.y - offset.y) ++ "px"
      ]
      [ viewEntity model.world.scene (Maybe.Extra.isJust model.activeRoll) DragDragging entity ]
  in
    case (model.dragObject, model.dragOffset, model.dragPosition) of
      (Just entityId, Just offset, Just position) ->
        Dict.Any.get entityId model.world.scene.entities |> Maybe.map (box position offset)
      _ -> Nothing

viewEntity : Scene -> Bool -> DragStatus -> Entity -> Html Event
viewEntity scene rolling dragStatus entity =
  let
    attributes =
      List.append
        [ class "entity"
        , on "pointerdown" <| Decode.succeed <| Drag <| DragPrepare entity.id
        ]
        ( case dragStatus of
            DragWaiting -> [ attribute "data-draggable" <| Uuid.toString entity.id ]
            DragRemoved -> [ class "drag-removed" ]
            DragDragging -> []
        )
  in
    div attributes <|
      [ input
          [ class "name"
          , placeholder "Name this entity"
          , value entity.name
          , onInput <| SetEntityName entity.id >> Request
          ]
          []
      , button
          [ onClick (ToggleEntity entity.id |> Request) ]
          [ text <| if entity.collapsed then "Show" else "Hide" ]
      , button [ onClick (RemoveEntity entity.id |> Request) ] [ text "✖" ]
      ] ++
      [ div [ class "stats"] <|
          joinedMap (viewStatGroup scene rolling) scene.statGroups entity.statGroups ++
          [ button [ onClick <| Request <| AddStatGroup entity.id ] [ text "+ Stat Group" ] ]
      , div [ class "aspects" ]
          ( if entity.collapsed
            then []
            else joinedMap (viewAspect rolling) scene.aspects entity.aspects
          )
      , button [ onClick (AddAspect entity.id |> Request) ] [ text "+ Aspect" ]
      ]

viewStatGroup : Scene -> Bool -> StatGroup -> Html Event
viewStatGroup scene rolling group = case group of
  StatGroup id name stats -> div [ class "stat-group" ] <|
    div [ class "stat-group-controls" ]
      [ input
          [ onInput <| SetStatGroupName id >> Request
          , placeholder "Name this stat group"
          , value name
          ]
          []
      , button [ id |> AddStat |> Request |> onClick ] [ text "+" ]
      , button [ id |> RemoveStatGroup |> Request |> onClick ] [ text "✖" ]
      ]
    :: joinedMap (viewStat rolling) scene.stats stats

viewStat : Bool -> Stat -> Html Event
viewStat rolling stat = case stat of
  Stat id name score -> div [ class "stat" ]
    [ input [ onInput <| SetStatName id >> Request, placeholder "Name this stat", value name ] []
    , input
        [ type_ "number"
        , onInput <| String.toInt >> Maybe.withDefault score >> SetStatScore id >> Request
        , value <| String.fromInt score
        ]
        []
    , button [ disabled rolling, id |> GenerateRollId |> onClick ] [ text "🎲" ]
    , button [ id |> RemoveStat |> Request |> onClick ] [ text "✖" ]
    ]

viewAspect : Bool -> Aspect -> Html Event
viewAspect rolling aspect =
  let
    edit text = SetAspectText aspect.id text |> Request
  in
    List.concat
      [ [ div
            [ attribute "data-autoexpand" aspect.text ]
            [ textarea [ placeholder "Describe this aspect.", value aspect.text, onInput edit ] [] ]
        , button [ onClick (RemoveAspect aspect.id |> Request) ] [ text "✖" ]
        , button [ onClick (AddDie aspect.id |> Request) ] [ text "+" ]
        , button [ onClick (RemoveDie aspect.id |> Request) ] [ text "-" ]
        ]
      , List.repeat aspect.dice <| button
          [ disabled <| not rolling
          , onClick <| ContinueRoll aspect.id
          ]
          [ text "🎲" ]
      ]
    |> div [ class "aspect" ]

dragMessageDecoder : Decode.Decoder DragEvent
dragMessageDecoder =
  let
    prepareDecoder = Decode.map DragPrepare jsonDecEntityId
    startDecoder = Decode.map DragStart positionDecoder
    moveDecoder = Decode.map2 DragMove
      (Decode.index 0 positionDecoder)
      (Decode.index 1 <| Decode.list draggableDecoder)
    endDecoder = Decode.succeed DragEnd
  in
    Dict.fromList
      [ ("dragPrepare", prepareDecoder)
      , ("dragStart", startDecoder)
      , ("dragMove", moveDecoder)
      , ("dragEnd", endDecoder)
      ]
    |> decodeSumObjectWithSingleField "DragEvent"

draggableDecoder : Decode.Decoder Draggable
draggableDecoder =
  Decode.map2 Draggable
    (Decode.field "id" Uuid.decoder)
    (Decode.field "region" rectangleDecoder)

positionDecoder : Decode.Decoder Position
positionDecoder =
  Decode.map2 Position
    (Decode.field "x" Decode.float)
    (Decode.field "y" Decode.float)

rectangleDecoder : Decode.Decoder Rectangle
rectangleDecoder =
  Decode.map4 Rectangle
    (Decode.field "x" Decode.float)
    (Decode.field "y" Decode.float)
    (Decode.field "width" Decode.float)
    (Decode.field "height" Decode.float)

withinRectangle : Position -> Rectangle -> Bool
withinRectangle position rect =
  rect.x <= position.x &&
  position.x <= rect.x + rect.width &&
  rect.y <= position.y &&
  position.y <= rect.y + rect.height

entityIndex : ClientState -> Uuid -> Maybe Int
entityIndex model id =
  model.world.scene.board
  |> List.indexedMap (\index entityId -> if entityId == id then Just index else Nothing)
  |> Maybe.Extra.values
  |> List.head

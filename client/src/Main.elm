port module Main exposing (main)

import Browser
import Destiny.Generated.Model exposing
  ( Aspect
  , ClientRequest (..)
  , Entity
  , WorldSnapshot
  , jsonDecEntity
  , jsonDecWorldSnapshot
  , jsonEncClientRequest
  )
import Dict
import Html exposing (Html, button, div, input, text, textarea)
import Html.Attributes exposing (attribute, checked, class, placeholder, style, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput)
import Html.Keyed
import Json.Decode as Decode
import Json.Helpers exposing (decodeSumObjectWithSingleField)
import Maybe.Extra
import Uuid exposing (Uuid)

type alias ClientState =
  { world : WorldSnapshot
  , dragObject : Maybe Entity
  , dragOffset : Maybe Position
  , dragPosition : Maybe Position
  }

type Message
  = UpdateWorld WorldSnapshot
  | DecodeError Decode.Error
  | Request ClientRequest
  | Drag DragEvent

type DragEvent
  = DragPrepare Entity
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

main : Program () ClientState Message
main =
  Browser.element
    { init = always
        ( { world = { entities = [], lastRoll = 0 }
          , dragObject = Nothing
          , dragOffset = Nothing
          , dragPosition = Nothing
          }
        , Cmd.none
        )
    , update = update
    , subscriptions = always <| Sub.batch
        [ receive <| decodeMessage jsonDecWorldSnapshot UpdateWorld
        , drag <| decodeMessage dragMessageDecoder Drag
        ]
    , view = view
    }

decodeMessage : Decode.Decoder a -> (a -> Message) -> Decode.Value -> Message
decodeMessage decoder toMessage value =
  case Decode.decodeValue decoder value of
    Ok result -> toMessage result
    Err error -> DecodeError error

update : Message -> ClientState -> (ClientState, Cmd Message)
update message model =
  case message of
    UpdateWorld newWorld -> ({ model | world = newWorld }, Cmd.none)
    DecodeError error -> "Decoding error: " ++ Decode.errorToString error |> Debug.todo
    Request request -> handleRequest request model
    Drag event -> handleDrag event model

handleRequest : ClientRequest -> ClientState -> (ClientState, Cmd Message)
handleRequest request model =
  let
    newWorld =
      case request of
        SetAspectText id text -> modifyAspect (\aspect -> { aspect | text = text }) id model.world
        _ -> model.world
  in
    ({ model | world = newWorld }, jsonEncClientRequest request |> send)

handleDrag : DragEvent -> ClientState -> (ClientState, Cmd Message)
handleDrag event model =
  case event of
    DragPrepare entity -> ({ model | dragObject = Just entity }, Cmd.none)
    DragStart offset -> ({ model | dragOffset = Just offset }, Cmd.none)
    DragMove position draggables -> moveDragObject position draggables model
    DragEnd ->
      ({ model | dragObject = Nothing, dragOffset = Nothing, dragPosition = Nothing }, Cmd.none)

moveDragObject : Position -> List Draggable -> ClientState -> (ClientState, Cmd Message)
moveDragObject position draggables model =
  let
    currentIndex = model.dragObject |> Maybe.andThen (.id >> entityIndex model)
    newIndex =
      draggables
      |> List.filter (.region >> withinRectangle position)
      |> List.filterMap (.id >> entityIndex model)
      |> List.head
    newModel = { model | dragPosition = Just position }
  in
    case (model.dragObject, newIndex) of
      (Just entity, Just index) ->
        if newIndex /= currentIndex
        then
          update (MoveEntity entity.id index |> Request)
            { model | world = moveEntity entity index newModel.world }
        else (newModel, Cmd.none)
      _ -> (newModel, Cmd.none)

modifyAspect : (Aspect -> Aspect) -> Uuid -> WorldSnapshot -> WorldSnapshot
modifyAspect f id world =
  let modify aspect = if id == aspect.id then f aspect else aspect
  in
    { world
    | entities = List.map
        (\entity -> { entity | aspects = List.map modify entity.aspects })
        world.entities
    }

moveEntity : Entity -> Int -> WorldSnapshot -> WorldSnapshot
moveEntity entity index world =
  let
    removed = List.filter (\e -> e.id /= entity.id) world.entities
    moved = List.take index removed ++ entity :: List.drop index removed
  in
    { world | entities = moved }

view : ClientState -> Html Message
view model =
  let
    dragStatus entity =
      case (model.dragObject, model.dragPosition) of
        (Just dragEntity, Just _) ->
          if dragEntity.id == entity.id then DragRemoved else DragWaiting
        _ -> DragWaiting
    entityElement entity = (Uuid.toString entity.id, viewEntity (dragStatus entity) entity)
  in
    List.append
      [ text ("Rolled: " ++ String.fromInt model.world.lastRoll)
      , button [ onClick <| Request AddEntity ] [ text "+" ]
      , button [ onClick <| Request Undo ] [ text "Undo" ]
      , button [ onClick <| Request Redo ] [ text "Redo" ]
      , Html.Keyed.node "div" [ class "entities" ] (List.map entityElement model.world.entities)
      ]
      (Maybe.Extra.toList <| viewDragBox model)
    |> div []

viewDragBox : ClientState -> Maybe (Html Message)
viewDragBox model =
  case (model.dragObject, model.dragOffset, model.dragPosition) of
    (Just entity, Just offset, Just position) -> Just <|
      div
        [ class "dragging"
        , style "position" "absolute"
        , style "left" <| String.fromFloat (position.x - offset.x) ++ "px"
        , style "top" <| String.fromFloat (position.y - offset.y) ++ "px"
        ]
        [ viewEntity DragDragging entity ]
    _ -> Nothing

viewEntity : DragStatus -> Entity -> Html Message
viewEntity dragStatus entity =
  let
    attributes =
      List.append
        [ class "entity"
        , on "pointerdown" <| Decode.succeed <| Drag <| DragPrepare entity
        ]
        ( case dragStatus of
            DragWaiting -> [ attribute "data-draggable" <| Uuid.toString entity.id ]
            DragRemoved -> [ class "drag-removed" ]
            DragDragging -> []
        )
  in
    div attributes
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
      , button [ onClick (RemoveEntity entity.id |> Request) ] [ text "Remove" ]
      , button [ onClick (AddAspect entity.id |> Request) ] [ text "+" ]
      , div [ class "aspects" ]
          ( if entity.collapsed then []
            else List.map viewAspect entity.aspects
          )
      ]

viewAspect : Aspect -> Html Message
viewAspect aspect =
  let
    die index selected =
      input
        [ type_ "checkbox"
        , checked selected
        , onCheck (SetDie aspect.id index >> Request)
        ]
        []
    edit text = SetAspectText aspect.id text |> Request
  in
    List.concat
      [ [ div
            [ attribute "data-autoexpand" aspect.text ]
            [ textarea [ placeholder "Describe this aspect.", value aspect.text, onInput edit ] [] ]
        , button [ onClick (RemoveAspect aspect.id |> Request) ] [ text "Remove" ]
        , button [ onClick (AddDie aspect.id |> Request) ] [ text "+" ]
        , button [ onClick (RemoveDie aspect.id |> Request) ] [ text "-" ]
        ]
      , List.indexedMap die aspect.dice
      , [ button [ onClick (Roll aspect.id |> Request) ] [ text "Roll" ] ]
      ]
    |> div [ class "aspect" ]

dragMessageDecoder : Decode.Decoder DragEvent
dragMessageDecoder =
  let
    prepareDecoder = Decode.map DragPrepare jsonDecEntity
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
  model.world.entities
  |> List.indexedMap (\index entity -> if entity.id == id then Just index else Nothing)
  |> Maybe.Extra.values
  |> List.head

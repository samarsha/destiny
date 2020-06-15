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
import Html.Attributes exposing (attribute, checked, class, placeholder, style, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput)
import Json.Decode as Decode
import Maybe.Extra
import Uuid exposing (Uuid)

type alias Model =
  { world : World
  , dragObject : Maybe Entity
  , dragPosition : Maybe Position
  }

type Message
  = UpdateWorld World
  | DecodeError Decode.Error
  | Request ClientRequest
  | DragPrepare Entity
  | DragMove Position (List Draggable)
  | DragEnd

type alias DragMessage =
  { position : Position
  , draggables : List Draggable
  }

type alias Draggable =
  { id : Uuid
  , region : Rectangle
  }

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

main : Program () Model Message
main =
  Browser.element
    { init = always
        ( { world = { entities = [], lastRoll = 0 }
          , dragObject = Nothing
          , dragPosition = Nothing
          }
        , Cmd.none
        )
    , update = update
    , subscriptions = always <| Sub.batch
        [ receive <| decodeMessage jsonDecWorld UpdateWorld
        , drag <| decodeMessage dragMessageDecoder (\msg -> DragMove msg.position msg.draggables)
        ]
    , view = view
    }

decodeMessage : Decode.Decoder a -> (a -> Message) -> Decode.Value -> Message
decodeMessage decoder toMessage value =
  case Decode.decodeValue decoder value of
    Ok result -> toMessage result
    Err error -> DecodeError error

update : Message -> Model -> (Model, Cmd Message)
update message model =
  case message of
    UpdateWorld newWorld -> ({ model | world = newWorld }, Cmd.none)
    DecodeError _ -> Debug.todo "Decoding error."
    Request request -> handleRequest request model
    DragPrepare entity -> ({ model | dragObject = Just entity }, Cmd.none)
    DragMove position draggables -> moveDragObject position draggables model
    DragEnd -> ({ model | dragObject = Nothing, dragPosition = Nothing }, Cmd.none)

handleRequest : ClientRequest -> Model -> (Model, Cmd Message)
handleRequest request model =
  let
    newWorld =
      case request of
        EditAspect aspect -> updateAspect aspect model.world
        _ -> model.world
  in
    ({ model | world = newWorld }, jsonEncClientRequest request |> send)

moveDragObject : Position -> List Draggable -> Model -> (Model, Cmd Message)
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
        then update (Request <| MoveEntity entity index)
          { model | world = moveEntity entity index newModel.world }
        else (newModel, Cmd.none)
      _ -> (newModel, Cmd.none)

updateAspect : Aspect -> World -> World
updateAspect aspect world =
  let
    replaceAspect originalAspect =
      if originalAspect.id == aspect.id then aspect
      else originalAspect
    updateEntityAspects entity = { entity | aspects = List.map replaceAspect entity.aspects }
  in
    { world | entities = List.map updateEntityAspects world.entities }

moveEntity : Entity -> Int -> World -> World
moveEntity entity index world =
  let
    removed = List.filter (\e -> e.id /= entity.id) world.entities
    moved = List.take index removed ++ entity :: List.drop index removed
  in
    { world | entities = moved }

view : Model -> Html Message
view model =
  List.append
    [ text ("Rolled: " ++ String.fromInt model.world.lastRoll)
    , button [ onClick (Request AddEntity) ] [ text "+" ]
    , div [ class "entities" ] (List.map (viewEntity True) model.world.entities)
    ]
    (Maybe.Extra.toList <| viewDragBox model)
  |> div [ on "pointerup" <| Decode.succeed DragEnd ]

viewDragBox : Model -> Maybe (Html Message)
viewDragBox model =
  case (model.dragObject, model.dragPosition) of
    (Just entity, Just { x, y }) ->
      div
        [ style "position" "absolute"
        , style "left" <| String.fromFloat x ++ "px"
        , style "top" <| String.fromFloat y ++ "px"
        ]
        [ viewEntity False entity ]
      |> Just
    _ -> Nothing

viewEntity : Bool -> Entity -> Html Message
viewEntity isDraggable entity =
  let
    attributes =
      List.append
        [ class "entity"
        , on "pointerdown" <| Decode.succeed <| DragPrepare entity
        ]
        ( if isDraggable
          then [ attribute "data-draggable" <| Uuid.toString entity.id ]
          else []
        )
  in
    div attributes
      [ div [ class "name" ] [ text "Untitled entity" ]
      , button [ onClick (ToggleEntity entity |> Request) ]
              [ text (if entity.collapsed then "Show" else "Hide") ]
      , button [ onClick (RemoveEntity entity |> Request) ] [ text "Remove" ]
      , button [ onClick (AddAspect entity |> Request) ] [ text "+" ]
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
        , onCheck (ToggleDie aspect index >> Request)
        ]
        []
    edit text = EditAspect { aspect | text = text } |> Request
  in
    List.concat
      [ [ textarea [ value aspect.text, placeholder "Describe this aspect.", onInput edit ] []
        , button [ onClick (RemoveAspect aspect |> Request) ] [ text "Remove" ]
        , button [ onClick (AddDie aspect |> Request) ] [ text "+" ]
        , button [ onClick (RemoveDie aspect |> Request) ] [ text "-" ]
        ]
      , List.indexedMap die aspect.dice
      , [ button [ onClick (Roll aspect |> Request) ] [ text "Roll" ] ]
      ]
    |> div [ class "aspect" ]

dragMessageDecoder : Decode.Decoder DragMessage
dragMessageDecoder =
  Decode.map2 DragMessage
    (Decode.field "position" positionDecoder)
    (Decode.field "draggables" <| Decode.list draggableDecoder)

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

entityIndex : Model -> Uuid -> Maybe Int
entityIndex model id =
  model.world.entities
  |> List.indexedMap (\index entity -> if entity.id == id then Just index else Nothing)
  |> Maybe.Extra.values
  |> List.head

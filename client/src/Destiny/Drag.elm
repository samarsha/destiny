module Destiny.Drag exposing
  ( Event (..)
  , State
  , emptyState
  , eventDecoder
  , update
  , view
  )

import Dict
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Json.Decode
import Json.Helpers
import Uuid exposing (Uuid)

type alias State =
  { dragging : Maybe Uuid
  , target : Maybe Uuid
  , offset : Maybe Position
  , position : Maybe Position
  }

type Event
  = Start Uuid Position
  | Move Position (List Draggable)
  | End

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

emptyState : State
emptyState =
  { dragging = Nothing
  , target = Nothing
  , offset = Nothing
  , position = Nothing
  }

update : Event -> State -> State
update event state = case event of
  Start id offset -> { state | dragging = Just id, target = Nothing, offset = Just offset }
  Move position draggables -> move position draggables state
  End -> { state | dragging = Nothing, offset = Nothing, position = Nothing }

move : Position -> List Draggable -> State -> State
move position draggables model =
  let
    target =
      draggables
      |> List.filter (.region >> withinRectangle position)
      -- If there are overlapping regions, choose the smallest one since it is the most specific.
      |> List.sortBy (.region >> area)
      |> List.head
      |> Maybe.map .id
  in
    { model
    | target = if target == model.dragging then Nothing else target
    , position = Just position
    }

view : (Uuid -> Html msg) -> State -> Maybe (Html msg)
view viewById state =
  let
    box position offset id = div
      [ class "dragging"
      , style "position" "absolute"
      , style "left" <| String.fromFloat (position.x - offset.x) ++ "px"
      , style "top" <| String.fromFloat (position.y - offset.y) ++ "px"
      ]
      [ viewById id ]
  in
    case (state.dragging, state.offset, state.position) of
      (Just id, Just offset, Just position) -> Just <| box position offset id
      _ -> Nothing

eventDecoder : Json.Decode.Decoder Event
eventDecoder =
  let
    startDecoder = Json.Decode.map2 Start
      (Json.Decode.index 0 Uuid.decoder)
      (Json.Decode.index 1 positionDecoder)
    moveDecoder = Json.Decode.map2 Move
      (Json.Decode.index 0 positionDecoder)
      (Json.Decode.index 1 <| Json.Decode.list draggableDecoder)
    endDecoder = Json.Decode.succeed End
  in
    Dict.fromList
      [ ("start", startDecoder)
      , ("move", moveDecoder)
      , ("end", endDecoder)
      ]
    |> Json.Helpers.decodeSumObjectWithSingleField "Event"

draggableDecoder : Json.Decode.Decoder Draggable
draggableDecoder =
  Json.Decode.map2 Draggable
    (Json.Decode.field "id" Uuid.decoder)
    (Json.Decode.field "region" rectangleDecoder)

positionDecoder : Json.Decode.Decoder Position
positionDecoder =
  Json.Decode.map2 Position
    (Json.Decode.field "x" Json.Decode.float)
    (Json.Decode.field "y" Json.Decode.float)

rectangleDecoder : Json.Decode.Decoder Rectangle
rectangleDecoder =
  Json.Decode.map4 Rectangle
    (Json.Decode.field "x" Json.Decode.float)
    (Json.Decode.field "y" Json.Decode.float)
    (Json.Decode.field "width" Json.Decode.float)
    (Json.Decode.field "height" Json.Decode.float)

withinRectangle : Position -> Rectangle -> Bool
withinRectangle position rect =
  rect.x <= position.x &&
  position.x <= rect.x + rect.width &&
  rect.y <= position.y &&
  position.y <= rect.y + rect.height

area : Rectangle -> Float
area rect = rect.width * rect.height

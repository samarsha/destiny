module Destiny.Scene exposing
  ( Event (..)
  , empty
  , entityIndex
  , moveEntity
  , updateAspect
  , updateEntity
  , updateStat
  , updateStatGroup
  , viewEntity
  )

import Destiny.Drag as Drag
import Destiny.Generated.Model exposing
  ( Aspect
  , AspectId
  , ClientRequest (..)
  , Entity
  , EntityId
  , Scene
  , Stat (..)
  , StatId
  , StatGroup (..)
  , StatGroupId
  )
import Destiny.Utils exposing (joinedMap)
import Dict.Any
import Html exposing (Html, button, div, input, text, textarea)
import Html.Attributes exposing (attribute, class, disabled, placeholder, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode
import Maybe.Extra
import Uuid

type Event
  = Request ClientRequest
  | Drag Drag.Event
  | GenerateRollId StatId
  | ContinueRoll AspectId

empty : Scene
empty =
  { board = []
  , entities = Dict.Any.empty Uuid.toString
  , statGroups = Dict.Any.empty Uuid.toString
  , stats = Dict.Any.empty Uuid.toString
  , aspects = Dict.Any.empty Uuid.toString
  }

updateEntity : (Entity -> Entity) -> EntityId -> Scene -> Scene
updateEntity f id scene = { scene | entities = Dict.Any.update id (Maybe.map f) scene.entities }

updateStatGroup : (StatGroup -> StatGroup) -> StatGroupId -> Scene -> Scene
updateStatGroup f id scene =
  { scene | statGroups = Dict.Any.update id (Maybe.map f) scene.statGroups }

updateStat : (Stat -> Stat) -> StatId -> Scene -> Scene
updateStat f id scene = { scene | stats = Dict.Any.update id (Maybe.map f) scene.stats }

updateAspect : (Aspect -> Aspect) -> AspectId -> Scene -> Scene
updateAspect f id scene = { scene | aspects = Dict.Any.update id (Maybe.map f) scene.aspects }

entityIndex : Scene -> EntityId -> Maybe Int
entityIndex scene id =
  scene.board
  |> List.indexedMap (\index entityId -> if entityId == id then Just index else Nothing)
  |> Maybe.Extra.values
  |> List.head

moveEntity : EntityId -> Int -> Scene -> Scene
moveEntity id index scene =
  let
    removed = List.filter ((/=) id) scene.board
    moved = List.take index removed ++ id :: List.drop index removed
  in
    { scene | board = moved }

viewEntity : Scene -> Bool -> Drag.Status -> Entity -> Html Event
viewEntity scene rolling dragging entity =
  let
    attributes =
      List.append
        [ class "entity"
        , on "pointerdown" <| Json.Decode.succeed <| Drag <| Drag.Prepare entity.id
        ]
        <| case dragging of
          Drag.Waiting -> [ attribute "data-draggable" <| Uuid.toString entity.id ]
          Drag.Removed -> [ class "drag-removed" ]
          Drag.Dragging -> []
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

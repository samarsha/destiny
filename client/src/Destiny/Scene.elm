module Destiny.Scene exposing
  ( Event (..)
  , Object (..)
  , aspectIndex
  , empty
  , entityIndex
  , get
  , moveAspect
  , moveEntity
  , updateAspect
  , updateEntity
  , updateStat
  , updateStatGroup
  , viewAspect
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
  , Stat
  , StatId
  , StatGroup
  , StatGroupId
  )
import Destiny.Utils exposing (joinedMap)
import Dict.Any
import Html exposing (Attribute, Html, button, div, input, text, textarea)
import Html.Attributes exposing (attribute, class, disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Maybe.Extra
import Uuid exposing (Uuid)

type Event
  = Request ClientRequest
  | Drag Drag.Event
  | GenerateRollId StatId
  | ContinueRoll AspectId

type Object
  = EntityObject Entity
  | StatGroupObject StatGroup
  | StatObject Stat
  | AspectObject Aspect

empty : Scene
empty =
  { board = []
  , entities = Dict.Any.empty Uuid.toString
  , statGroups = Dict.Any.empty Uuid.toString
  , stats = Dict.Any.empty Uuid.toString
  , aspects = Dict.Any.empty Uuid.toString
  }

get : Scene -> Uuid -> Maybe Object
get scene id =
  Dict.Any.get id scene.entities
  |> Maybe.map EntityObject
  |> Maybe.Extra.orElse (Dict.Any.get id scene.statGroups |> Maybe.map StatGroupObject)
  |> Maybe.Extra.orElse (Dict.Any.get id scene.stats |> Maybe.map StatObject)
  |> Maybe.Extra.orElse (Dict.Any.get id scene.aspects |> Maybe.map AspectObject)

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
entityIndex scene id = List.Extra.elemIndex id scene.board

aspectIndex : Scene -> EntityId -> AspectId -> Maybe Int
aspectIndex scene entityId aspectId =
  Dict.Any.get entityId scene.entities
  |> Maybe.andThen (\entity -> List.Extra.elemIndex aspectId entity.aspects)

moveEntity : EntityId -> Int -> Scene -> Scene
moveEntity id index scene =
  let
    removed = List.filter ((/=) id) scene.board
    moved = List.take index removed ++ id :: List.drop index removed
  in
    { scene | board = moved }

moveAspect : AspectId -> EntityId -> Int -> Scene -> Scene
moveAspect aspectId entityId index scene =
  let
    remove entity = { entity | aspects = List.filter ((/=) aspectId) entity.aspects }
    add entity =
      let (xs, ys) = List.Extra.splitAt index entity.aspects
      in { entity | aspects = xs ++ aspectId :: ys }
  in
    { scene | entities =
        scene.entities
        |> Dict.Any.map (\_ -> remove)
        |> Dict.Any.update entityId (Maybe.map add)
    }

dragAttributes : Uuid -> Maybe Uuid -> List (Attribute msg)
dragAttributes id dragging =
  let draggable = attribute "data-draggable" <| Uuid.toString id
  in case dragging of
    Just dragId -> if id == dragId then [ class "drag-removed" ] else [ draggable ]
    Nothing -> [ draggable ]

viewEntity : Scene -> Bool -> Maybe Uuid -> Entity -> Html Event
viewEntity scene rolling dragging entity =
  let
    attributes = List.append [ class "entity" ] <| dragAttributes entity.id dragging
    content =
      [ div [ class "stats" ] <|
          joinedMap (viewStatGroup scene rolling) scene.statGroups entity.statGroups ++
          [ button [ onClick <| Request <| AddStatGroup entity.id ] [ text "+ Stat Group" ] ]
      , div [ class "aspects" ]
          ( if entity.collapsed
            then []
            else joinedMap (viewAspect rolling dragging) scene.aspects entity.aspects
          )
      , button [ onClick (AddAspect entity.id |> Request) ] [ text "+ Aspect" ]
      ]
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
      (if entity.collapsed then [] else content)

viewStatGroup : Scene -> Bool -> StatGroup -> Html Event
viewStatGroup scene rolling group = div [ class "stat-group" ] <|
  div [ class "stat-group-controls" ]
    [ input
        [ onInput <| SetStatGroupName group.id >> Request
        , placeholder "Name this stat group"
        , value group.name
        ]
        []
    , button [ group.id |> AddStat |> Request |> onClick ] [ text "+" ]
    , button [ group.id |> RemoveStatGroup |> Request |> onClick ] [ text "✖" ]
    ]
  :: joinedMap (viewStat rolling) scene.stats group.stats

viewStat : Bool -> Stat -> Html Event
viewStat rolling stat = div [ class "stat" ]
  [ input
      [ onInput <| SetStatName stat.id >> Request
      , placeholder "Name this stat", value stat.name
      ]
      []
  , input
      [ type_ "number"
      , onInput <| String.toInt >> Maybe.withDefault stat.score >> SetStatScore stat.id >> Request
      , value <| String.fromInt stat.score
      ]
      []
  , button [ disabled rolling, stat.id |> GenerateRollId |> onClick ] [ text "🎲" ]
  , button [ stat.id |> RemoveStat |> Request |> onClick ] [ text "✖" ]
  ]

viewAspect : Bool -> Maybe Uuid -> Aspect -> Html Event
viewAspect rolling dragging aspect =
  let edit text = SetAspectText aspect.id text |> Request
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
    |> div (List.append [ class "aspect" ] <| dragAttributes aspect.id dragging)

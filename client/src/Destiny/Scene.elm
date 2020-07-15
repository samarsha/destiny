module Destiny.Scene exposing
  ( Event (..)
  , Model
  , Object (..)
  , aspectIndex
  , empty
  , entityIndex
  , get
  , moveAspect
  , moveEntity
  , update
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
  , MessageId
  , Scene
  , Stat
  , StatId
  , StatGroup
  , StatGroupId
  , jsonEncClientRequest
  )
import Destiny.Utils exposing (joinedMap)
import Dict.Any
import Flip exposing (flip)
import Html exposing (Attribute, Html, button, div, input, text, textarea)
import Html.Attributes exposing (attribute, class, disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import List.Extra
import Maybe.Extra
import Random
import Uuid exposing (Uuid)

type alias Model =
  { scene : Scene
  , rolling : Maybe Uuid
  , drag : Drag.State
  }

type Object
  = EntityObject Entity
  | StatGroupObject StatGroup
  | StatObject Stat
  | AspectObject Aspect

type Event
  = Request ClientRequest
  | Drag Drag.Event
  | GenerateRollId StatId
  | StartRoll StatId MessageId
  | ContinueRoll AspectId
  | EndRoll

empty : Model
empty =
  let
    scene =
      { board = []
      , entities = Dict.Any.empty Uuid.toString
      , statGroups = Dict.Any.empty Uuid.toString
      , stats = Dict.Any.empty Uuid.toString
      , aspects = Dict.Any.empty Uuid.toString
      }
  in
    { scene = scene
    , rolling = Nothing
    , drag = Drag.emptyState
    }

get : Scene -> Uuid -> Maybe Object
get scene id =
  Dict.Any.get id scene.entities
  |> Maybe.map EntityObject
  |> Maybe.Extra.orElse (Dict.Any.get id scene.statGroups |> Maybe.map StatGroupObject)
  |> Maybe.Extra.orElse (Dict.Any.get id scene.stats |> Maybe.map StatObject)
  |> Maybe.Extra.orElse (Dict.Any.get id scene.aspects |> Maybe.map AspectObject)

update : (Json.Decode.Value -> Cmd Event) -> Model -> Event -> (Model, Cmd Event)
update send model event = case event of
  Request request -> handleRequest send model request
  Drag dragEvent -> handleDragEvent send model dragEvent
  GenerateRollId statId -> (model, Uuid.uuidGenerator |> Random.generate (StartRoll statId))
  StartRoll statId rollId ->
    let cmd = RollStat statId rollId |> jsonEncClientRequest |> send
    in ({ model | rolling = Just rollId }, cmd)
  ContinueRoll aspectId ->
    let
      cmd = case model.rolling of
        Just rollId -> RollAspect aspectId rollId |> jsonEncClientRequest |> send
        Nothing -> Cmd.none
    in (model, cmd)
  EndRoll ->
    ({ model | rolling = Nothing }, Cmd.none)

handleRequest : (Json.Decode.Value -> Cmd msg) -> Model -> ClientRequest -> (Model, Cmd msg)
handleRequest send model request =
  let
    updater = case request of
      SetEntityName id name ->
        updateEntity (\entity -> { entity | name = name }) id
      MoveEntity id index ->
        moveEntity id index
      SetStatGroupName id name ->
        updateStatGroup (\group -> { group | name = name }) id
      SetStatName id name ->
        updateStat (\stat -> { stat | name = name }) id
      SetAspectText id text ->
        updateAspect (\aspect -> { aspect | text = text }) id
      MoveAspect aspectId entityId index ->
        moveAspect aspectId entityId index
      _ -> identity
  in
    ({ model | scene = updater model.scene }, jsonEncClientRequest request |> send)

handleDragEvent : (Json.Decode.Value -> Cmd msg) -> Model -> Drag.Event -> (Model, Cmd msg)
handleDragEvent send model event =
  let
    newModel = { model | drag = Drag.update event model.drag }
    request = case newModel.drag.dragging |> Maybe.andThen (get model.scene) of
      Just (EntityObject _) -> dragEntity newModel
      Just (AspectObject _) -> dragAspect newModel
      _ -> Nothing
  in
    request |> Maybe.Extra.unwrap (newModel, Cmd.none) (handleRequest send newModel)

dragEntity : Model -> Maybe ClientRequest
dragEntity state =
  let
    targetIndex =
      state.drag.targets
      |> List.filterMap (entityIndex state.scene)
      |> List.head
  in case (state.drag.dragging, targetIndex) of
    (Just id, Just index) -> MoveEntity id index |> Just
    _ -> Nothing

dragAspect : Model -> Maybe ClientRequest
dragAspect state =
  let
    aspectTarget =
      state.drag.targets
      |> List.filterMap (flip Dict.Any.get state.scene.aspects)
      |> List.head
    entityTarget =
      state.drag.targets
      |> List.filterMap (flip Dict.Any.get state.scene.entities)
      |> List.head
  in case (state.drag.dragging, aspectTarget, entityTarget) of
    (Just id, Just target, Just parent) ->
      aspectIndex state.scene parent.id target.id
      |> Maybe.map (MoveAspect id parent.id)
    (Just id, Nothing, Just parent) ->
      if List.isEmpty parent.aspects
      then MoveAspect id parent.id 0 |> Just
      else Nothing
    _ -> Nothing

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

viewEntity : Model -> Entity -> Html Event
viewEntity model entity =
  let
    attributes = List.append [ class "entity" ] <| dragAttributes entity.id model.drag.dragging
    content =
      [ div [ class "stats" ] <|
          joinedMap (viewStatGroup model) model.scene.statGroups entity.statGroups ++
          [ button [ onClick <| Request <| AddStatGroup entity.id ] [ text "+ Stat Group" ] ]
      , div [ class "aspects" ]
          ( if entity.collapsed
            then []
            else joinedMap (viewAspect model) model.scene.aspects entity.aspects
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

viewStatGroup : Model -> StatGroup -> Html Event
viewStatGroup model group = div [ class "stat-group" ] <|
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
  :: joinedMap (viewStat model) model.scene.stats group.stats

viewStat : Model -> Stat -> Html Event
viewStat model stat = div [ class "stat" ]
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
  , button
      [ isRolling model stat.id |> disabled
      , stat.id |> GenerateRollId |> onClick
      ]
      [ text "🎲" ]
  , button [ stat.id |> RemoveStat |> Request |> onClick ] [ text "✖" ]
  ]

viewAspect : Model -> Aspect -> Html Event
viewAspect model aspect =
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
          [ isRolling model aspect.id |> not |> disabled
          , ContinueRoll aspect.id |> onClick
          ]
          [ text "🎲" ]
      ]
    |> div (List.append [ class "aspect" ] <| dragAttributes aspect.id model.drag.dragging)

isRolling : Model -> Uuid -> Bool
isRolling model id = model.rolling == Just id

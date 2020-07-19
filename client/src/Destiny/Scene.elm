module Destiny.Scene exposing
  ( Event
  , Model
  , drag
  , empty
  , replace
  , setRole
  , update
  , viewBoard
  , viewDrag
  , viewRollBar
  )

import Destiny.AnyBag as AnyBag
import Destiny.Drag as Drag
import Destiny.Generated.Message exposing (MessageId)
import Destiny.Generated.Scene exposing
  ( Aspect
  , AspectId
  , Die
  , Entity
  , EntityId
  , Role (..)
  , Scene
  , Stat
  , StatId
  , StatGroup
  , StatGroupId
  )
import Destiny.Generated.World exposing (Command (..))
import Destiny.Utils exposing (joinedMap)
import Dict.Any
import Flip exposing (flip)
import Html exposing (Attribute, Html, button, div, input, span, text, textarea)
import Html.Attributes exposing (attribute, class, disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import List.Extra
import Maybe.Extra
import Random
import Uuid exposing (Uuid)

type Object
  = EntityObject Entity
  | StatGroupObject StatGroup
  | StatObject Stat
  | AspectObject Aspect

type Event
  = Command Command
  | Drag Drag.Event
  | GenerateRollId StatId
  | StartRoll StatId MessageId
  | ContinueRoll AspectId
  | EndRoll
  | ToggleEdit EntityId

type Model = Model
  { scene : Scene
  , rolling : Maybe Uuid
  , drag : Drag.State
  , role : Role
  , editing : Maybe EntityId
  }

type Mode = View | Edit

-- Model

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
    Model { scene = scene
          , rolling = Nothing
          , drag = Drag.empty
          , role = Player
          , editing = Nothing
          }

get : Scene -> Uuid -> Maybe Object
get scene id =
  Dict.Any.get id scene.entities
  |> Maybe.map EntityObject
  |> Maybe.Extra.orElse (Dict.Any.get id scene.statGroups |> Maybe.map StatGroupObject)
  |> Maybe.Extra.orElse (Dict.Any.get id scene.stats |> Maybe.map StatObject)
  |> Maybe.Extra.orElse (Dict.Any.get id scene.aspects |> Maybe.map AspectObject)

entityIndex : Scene -> EntityId -> Maybe Int
entityIndex scene id = List.Extra.elemIndex id scene.board

aspectIndex : Scene -> EntityId -> AspectId -> Maybe Int
aspectIndex scene entityId aspectId =
  Dict.Any.get entityId scene.entities
  |> Maybe.andThen (\entity -> List.Extra.elemIndex aspectId entity.aspects)

isRolling : Model -> Bool
isRolling (Model model) = Maybe.Extra.isJust model.rolling

-- View

viewRollBar : Model -> Maybe (Html Event)
viewRollBar (Model model) =
  if isRolling (Model model)
  then
    Just <| div
      [ class "active-roll" ]
      [ text "You're on a roll!"
      , button [ onClick <| EndRoll ] [ text "Finish" ]
      ]
  else Nothing

viewBoard : Model -> Html Event
viewBoard (Model model) =
  let
    mode id = if model.editing == Just id then Edit else View
    entityElement entity =
      ( Uuid.toString entity.id
      , viewEntity (mode entity.id) (Model model) entity
      )
  in
    div []
      [ button [ onClick <| Command AddEntity ] [ text "+" ]
      , button [ onClick <| Command Undo ] [ text "Undo" ]
      , button [ onClick <| Command Redo ] [ text "Redo" ]
      , Html.Keyed.node "div" [ class "entities" ] <| joinedMap entityElement
          model.scene.entities
          model.scene.board
      ]

viewDrag : Model -> Maybe (Html Event)
viewDrag (Model model) =
  let
    -- TODO: Use the current view/edit mode for the entity.
    entity id =
      Dict.Any.get id model.scene.entities
      |> Maybe.map (Model { model | drag = Drag.empty } |> viewEntity View)
    aspect id =
      Dict.Any.get id model.scene.aspects
      |> Maybe.map (Model { model | drag = Drag.empty } |> viewAspect View)
    invalid id = "Invalid ID: " ++ Uuid.toString id |> Debug.todo
    view id =
      entity id
      |> Maybe.Extra.orElse (aspect id)
      |> Maybe.Extra.unpack (\() -> invalid id) identity
  in
    model.drag |> Drag.view view

viewEntity : Mode -> Model -> Entity -> Html Event
viewEntity mode (Model model) entity =
  let
    attributes = class "entity" :: dragAttributes entity.id model.drag.dragging
    name = case mode of
      View -> div [ class "name" ] [ text entity.name ]
      Edit -> input
        [ class "name"
        , placeholder "Name this entity"
        , value entity.name
        , onInput <| SetEntityName entity.id >> Command
        ]
        []
    hideButton = button
      [ onClick <| Command <| ToggleEntity entity.id ]
      [ text <| if entity.collapsed then "Show" else "Hide" ]
    editButton = button
      [ onClick <| ToggleEdit entity.id ]
      [ text "📝" ]
    addGroupButton = whenEdit mode <| button
      [ onClick <| Command <| AddStatGroup entity.id ]
      [ text "+ Stat Group" ]
    stats =
      joinedMap (viewStatGroup mode <| Model model) model.scene.statGroups entity.statGroups
      ++ Maybe.Extra.toList addGroupButton
    aspects =
      if entity.collapsed
      then []
      else joinedMap (viewAspect mode <| Model model) model.scene.aspects entity.aspects
    content = Maybe.Extra.values
      [ Just <| div [ class "stats" ] stats
      , Just <| div [ class "aspects" ] aspects
      , whenEdit mode <| button [ onClick (AddAspect entity.id |> Command) ] [ text "+ Aspect" ]
      ]
  in
    div attributes <| Maybe.Extra.values
      [ Just name
      , Just hideButton
      , Just editButton
      , whenEdit mode <| button [ onClick (RemoveEntity entity.id |> Command) ] [ text "✖" ]
      ]
      ++ (if entity.collapsed then [] else content)

viewStatGroup : Mode -> Model -> StatGroup -> Html Event
viewStatGroup mode (Model model) group =
  let
    name = case mode of
      View -> span [ class "stat-group-name" ] [ text group.name ]
      Edit -> input
        [ class "stat-group-name"
        , onInput <| SetStatGroupName group.id >> Command
        , placeholder "Name this stat group"
        , value group.name
        ]
        []
    controls = Maybe.Extra.values
      [ Just name
      , whenEdit mode <| button [ group.id |> AddStat |> Command |> onClick ] [ text "+" ]
      , whenEdit mode <| button [ group.id |> RemoveStatGroup |> Command |> onClick ] [ text "✖" ]
      ]
    stats = joinedMap (viewStat mode <| Model model) model.scene.stats group.stats
  in
    div [ class "stat-group" ] <|
      div [ class "stat-group-controls" ] controls
      :: stats

viewStat : Mode -> Model -> Stat -> Html Event
viewStat mode model stat =
  let
    name = case mode of
      View -> span [ class "stat-name" ] [ text stat.name ]
      Edit -> input
        [ class "stat-name"
        , onInput <| SetStatName stat.id >> Command
        , placeholder "Name this stat", value stat.name
        ]
        []
    score = case mode of
      View -> span [ class "stat-score" ] [ text <| String.fromInt stat.score ]
      Edit -> input
        [ class "stat-score"
        , type_ "number"
        , onInput <| String.toInt >> Maybe.withDefault stat.score >> SetStatScore stat.id >> Command
        , value <| String.fromInt stat.score
        ]
        []
    rollButton = button
      [ isRolling model |> disabled
      , stat.id |> GenerateRollId |> onClick
      ]
      [ text "🎲" ]
  in
    div [ class "stat" ] <| Maybe.Extra.values
      [ Just name
      , Just score
      , Just rollButton
      , whenEdit mode <| button [ stat.id |> RemoveStat |> Command |> onClick ] [ text "✖" ]
      ]

viewAspect : Mode -> Model -> Aspect -> Html Event
viewAspect mode (Model model) aspect =
  let
    attributes = class "aspect" :: dragAttributes aspect.id model.drag.dragging
    description = case mode of
      View -> div [ class "aspect-description" ] [ text aspect.text ]
      Edit -> div
        [ attribute "data-autoexpand" aspect.text ]
        [ textarea
            [ class "aspect-description"
            , placeholder "Describe this aspect."
            , value aspect.text
            , onInput <| Command << SetAspectText aspect.id
            ]
            []
        ]
    content = Maybe.Extra.values
      [ Just description
      , whenEdit mode <| button [ onClick (RemoveAspect aspect.id |> Command) ] [ text "✖" ]
      , whenEdit mode <| button [ onClick (AddDie aspect.id |> Command) ] [ text "+" ]
      , whenEdit mode <| button [ onClick (RemoveDie aspect.id |> Command) ] [ text "-" ]
      ]
  in
    div attributes
    <| content
    ++ (AnyBag.values aspect.dice |> List.map (viewDie (Model model) aspect))

viewDie : Model -> Aspect -> Die -> Html Event
viewDie (Model model) aspect role =
  let
    roleClass = case role of
      Player -> "die-player"
      DM -> "die-dm"
  in
    button
      [ class roleClass
      , disabled <| (not <| isRolling <| Model model) || model.role /= role
      , onClick <| ContinueRoll aspect.id
      ]
    [ text "🎲" ]

dragAttributes : Uuid -> Maybe Uuid -> List (Attribute msg)
dragAttributes id dragging =
  let draggable = attribute "data-draggable" <| Uuid.toString id
  in case dragging of
    Just dragId -> if id == dragId then [ class "drag-removed" ] else [ draggable ]
    Nothing -> [ draggable ]

whenEdit : Mode -> Html msg -> Maybe (Html msg)
whenEdit mode element = case mode of
  Edit -> Just element
  View -> Nothing

-- Update

update : (Command -> Cmd Event) -> Model -> Event -> (Model, Cmd Event)
update send (Model model) event = case event of
  Command command -> handleRequest send (Model model) command
  Drag dragEvent -> handleDragEvent send (Model model) dragEvent
  GenerateRollId statId -> (Model model, Uuid.uuidGenerator |> Random.generate (StartRoll statId))
  StartRoll statId rollId ->
    let cmd = RollStat statId rollId |> send
    in (Model { model | rolling = Just rollId }, cmd)
  ContinueRoll aspectId ->
    let
      cmd = case model.rolling of
        Just rollId -> RollAspect aspectId rollId |> send
        Nothing -> Cmd.none
    in (Model model, cmd)
  EndRoll ->
    (Model { model | rolling = Nothing }, Cmd.none)
  ToggleEdit entityId ->
    let
      editing =
        if model.editing == Just entityId
        then Nothing
        else Just entityId
    in
      (Model { model | editing = editing }, Cmd.none)

updateEntity : (Entity -> Entity) -> EntityId -> Scene -> Scene
updateEntity f id scene = { scene | entities = Dict.Any.update id (Maybe.map f) scene.entities }

updateStatGroup : (StatGroup -> StatGroup) -> StatGroupId -> Scene -> Scene
updateStatGroup f id scene =
  { scene | statGroups = Dict.Any.update id (Maybe.map f) scene.statGroups }

updateStat : (Stat -> Stat) -> StatId -> Scene -> Scene
updateStat f id scene = { scene | stats = Dict.Any.update id (Maybe.map f) scene.stats }

updateAspect : (Aspect -> Aspect) -> AspectId -> Scene -> Scene
updateAspect f id scene = { scene | aspects = Dict.Any.update id (Maybe.map f) scene.aspects }

replace : Model -> Scene -> Model
replace (Model model) scene = Model { model | scene = scene }

setRole : Model -> Role -> Model
setRole (Model model) role = Model { model | role = role }

handleRequest : (Command -> Cmd msg) -> Model -> Command -> (Model, Cmd msg)
handleRequest send (Model model) request =
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
    (Model { model | scene = updater model.scene }, send request)

handleDragEvent : (Command -> Cmd msg) -> Model -> Drag.Event -> (Model, Cmd msg)
handleDragEvent send (Model model) event =
  let
    newModel = { model | drag = Drag.update event model.drag }
    request = case newModel.drag.dragging |> Maybe.andThen (get model.scene) of
      Just (EntityObject _) -> dragEntity (Model newModel)
      Just (AspectObject _) -> dragAspect (Model newModel)
      _ -> Nothing
  in
    request |> Maybe.Extra.unwrap
      (Model newModel, Cmd.none)
      (handleRequest send (Model newModel))

drag : Drag.Event -> Event
drag = Drag

dragEntity : Model -> Maybe Command
dragEntity (Model model) =
  let
    targetIndex =
      model.drag.targets
      |> List.filterMap (entityIndex model.scene)
      |> List.head
  in case (model.drag.dragging, targetIndex) of
    (Just id, Just index) -> MoveEntity id index |> Just
    _ -> Nothing

dragAspect : Model -> Maybe Command
dragAspect (Model model) =
  let
    aspectTarget =
      model.drag.targets
      |> List.filterMap (flip Dict.Any.get model.scene.aspects)
      |> List.head
    entityTarget =
      model.drag.targets
      |> List.filterMap (flip Dict.Any.get model.scene.entities)
      |> List.head
  in case (model.drag.dragging, aspectTarget, entityTarget) of
    (Just id, Just target, Just parent) ->
      aspectIndex model.scene parent.id target.id
      |> Maybe.map (MoveAspect id parent.id)
    (Just id, Nothing, Just parent) ->
      if List.isEmpty parent.aspects
      then MoveAspect id parent.id 0 |> Just
      else Nothing
    _ -> Nothing

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

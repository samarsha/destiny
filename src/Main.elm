module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text, textarea)
import Html.Attributes exposing (checked, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Random
import Uuid exposing (Uuid, uuidGenerator)

{-| The model. -}
type alias Model =
  { entities : List Entity  -- The entities.
  , lastRoll : Int          -- The result of the last dice roll.
  }

{-| An entity. -}
type alias Entity =
  { id : Uuid              -- The entity ID.
  , aspects : List Aspect  -- The aspects that belong to the entity.
  , collapsed : Bool       -- True if the entity is collapsed.
  }

{-| An aspect. -}
type alias Aspect =
  { id : Uuid         -- The aspect ID.
  , text : String     -- The description of the aspect.
  , dice : List Bool  -- A list of the selected status for each free invoke die.
  }

type Message
  = AddEntity
  | AddEntityWithId Uuid
  | ToggleEntity Entity
  | RemoveEntity Entity
  | AddAspect Entity
  | AddAspectWithId Entity Uuid
  | EditAspect Aspect
  | RemoveAspect Aspect
  | AddDie Aspect
  | ToggleDie Aspect Int Bool
  | RemoveDie Aspect
  | Roll Aspect
  | RollResult Int

main : Program () Model Message
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = always Sub.none
    , view = view
    }

init : () -> (Model, Cmd Message)
init _ = ({ entities = [], lastRoll = 0 }, Cmd.none)

update : Message -> Model -> (Model, Cmd Message)
update message model =
  case message of
    AddEntity ->
      (model, Random.generate AddEntityWithId uuidGenerator)
    AddEntityWithId id ->
      (addEntity id model, Cmd.none)
    ToggleEntity entity ->
      (updateEntity { entity | collapsed = not entity.collapsed } model, Cmd.none)
    RemoveEntity entity ->
      (removeEntity entity model, Cmd.none)
    AddAspect parent ->
      (model, Random.generate (AddAspectWithId parent) uuidGenerator)
    AddAspectWithId parent id ->
      (addAspect parent id model, Cmd.none)
    EditAspect aspect ->
      (updateAspect aspect model, Cmd.none)
    RemoveAspect aspect ->
      (removeAspect aspect model, Cmd.none)
    AddDie aspect ->
      (updateAspect { aspect | dice = False :: aspect.dice } model, Cmd.none)
    ToggleDie aspect index selected ->
      (updateAspect { aspect | dice = replace index selected aspect.dice } model, Cmd.none)
    RemoveDie aspect ->
      (removeDie aspect model, Cmd.none)
    Roll aspect ->
      rollAspect aspect model
    RollResult roll ->
      ({ model | lastRoll = roll }, Cmd.none)

replace : Int -> a -> List a -> List a
replace index item = List.indexedMap (\i x -> if i == index then item else x)

addEntity : Uuid -> Model -> Model
addEntity id model =
  let entity = { id = id, aspects = [], collapsed = False }
  in { model | entities = entity :: model.entities }

updateEntity : Entity -> Model -> Model
updateEntity entity model =
  let
    replaceEntity originalEntity =
      if originalEntity.id == entity.id
      then entity
      else originalEntity
  in
    { model | entities = List.map replaceEntity model.entities }

removeEntity : Entity -> Model -> Model
removeEntity entity model =
  { model | entities = List.filter (\e -> e.id /= entity.id) model.entities }

addAspect : Entity -> Uuid -> Model -> Model
addAspect entity id =
  let aspect = { id = id, text = "Hello, world!", dice = [] }
  in updateEntity { entity | aspects = aspect :: entity.aspects }

updateAspect : Aspect -> Model -> Model
updateAspect aspect model =
  let
    replaceAspect originalAspect =
      if originalAspect.id == aspect.id
      then aspect
      else originalAspect
    updateEntityAspects entity = { entity | aspects = List.map replaceAspect entity.aspects }
  in
    { model | entities = List.map updateEntityAspects model.entities }

removeAspect : Aspect -> Model -> Model
removeAspect aspect model =
  let
    updateEntityAspects entity =
      { entity | aspects = List.filter (\a -> a.id /= aspect.id) entity.aspects }
  in
    { model | entities = List.map updateEntityAspects model.entities }

rollAspect : Aspect -> Model -> (Model, Cmd Message)
rollAspect aspect model =
  let
    newModel = updateAspect { aspect | dice = List.filter not aspect.dice } model
    rolled = List.filter identity aspect.dice |> List.length
    command = Random.generate RollResult (Random.int rolled (6 * rolled))
  in
    (newModel, command)

removeDie : Aspect -> Model -> Model
removeDie aspect =
  let
    newDice =
      case aspect.dice of
        [] -> []
        _ :: xs -> xs
  in
    updateAspect { aspect | dice = newDice }

view : Model -> Html Message
view model =
  div []
    [ text ("Rolled: " ++ String.fromInt model.lastRoll)
    , button [ onClick AddEntity ] [ text "+" ]
    , div [] (List.map viewEntity model.entities)
    ]

viewEntity : Entity -> Html Message
viewEntity entity =
  div [] <| List.append
    [ text "Entity"
    , button [ onClick (ToggleEntity entity) ]
             [ text (if entity.collapsed then "Show" else "Hide") ]
    , button [ onClick (RemoveEntity entity) ] [ text "Remove" ]
    , button [ onClick (AddAspect entity) ] [ text "+" ]
    ]
    ( if entity.collapsed
      then []
      else List.map viewAspect entity.aspects
    )

viewAspect : Aspect -> Html Message
viewAspect aspect =
  let
    die index selected =
      input
        [ type_ "checkbox"
        , checked selected
        , onCheck (ToggleDie aspect index)
        ]
        []
    edit text = EditAspect { aspect | text = text }
  in
    div [] <| List.concat
      [ [ textarea [ value aspect.text, onInput edit ] []
        , button [ onClick (RemoveAspect aspect) ] [ text "Remove" ]
        , button [ onClick (AddDie aspect) ] [ text "+" ]
        , button [ onClick (RemoveDie aspect) ] [ text "-" ]
        ]
      , List.indexedMap die aspect.dice
      , [ button [ onClick (Roll aspect) ] [ text "Roll" ] ]
      ]

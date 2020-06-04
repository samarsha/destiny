module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text, textarea)
import Html.Attributes exposing (checked, type_)
import Html.Events exposing (onCheck, onClick)
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
  | AddAspect Entity
  | AddAspectWithId Entity Uuid
  | AddDie Aspect
  | SelectDie Aspect Int Bool
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
    AddAspect parent ->
      (model, Random.generate (AddAspectWithId parent) uuidGenerator)
    AddAspectWithId parent id ->
      (addAspect parent id model, Cmd.none)
    AddDie aspect ->
      (updateAspect { aspect | dice = False :: aspect.dice } model, Cmd.none)
    SelectDie aspect index selected ->
      (updateAspect { aspect | dice = replace index selected aspect.dice } model, Cmd.none)
    Roll aspect ->
      rollAspect aspect model
    RollResult roll ->
      ({ model | lastRoll = roll }, Cmd.none)

replace : Int -> a -> List a -> List a
replace index item = List.indexedMap (\i x -> if i == index then item else x)

addEntity : Uuid -> Model -> Model
addEntity id model =
  let entity = { id = id, aspects = [] }
  in { model | entities = entity :: model.entities }

addAspect : Entity -> Uuid -> Model -> Model
addAspect parent id model =
  let
    aspect = { id = id, text = "Hello, world!", dice = [] }
    updateEntity entity =
      if entity.id == parent.id
      then { entity | aspects = aspect :: entity.aspects }
      else entity
  in
    { model | entities = List.map updateEntity model.entities }

updateAspect : Aspect -> Model -> Model
updateAspect aspect model =
  let
    replaceAspect originalAspect =
      if originalAspect.id == aspect.id
      then aspect
      else originalAspect
    updateEntity entity = { entity | aspects = List.map replaceAspect entity.aspects }
  in
    { model | entities = List.map updateEntity model.entities }

rollAspect : Aspect -> Model -> (Model, Cmd Message)
rollAspect aspect model =
  let
    newModel = updateAspect { aspect | dice = List.filter not aspect.dice } model
    rolled = List.filter identity aspect.dice |> List.length
    command = Random.generate RollResult (Random.int rolled (6 * rolled))
  in
    (newModel, command)

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
    , button [ onClick (AddAspect entity) ] [ text "+" ]
    ]
    (List.map viewAspect entity.aspects)

viewAspect : Aspect -> Html Message
viewAspect aspect =
  let
    die index selected =
      input
        [ type_ "checkbox"
        , checked selected
        , onCheck (SelectDie aspect index)
        ]
        []
  in
    div [] <| List.concat
      [ [ textarea [] [ text aspect.text ]
        , button [ onClick (AddDie aspect) ] [ text "+" ]
        ]
      , List.indexedMap die aspect.dice
      , [ button [ onClick (Roll aspect) ] [ text "Roll" ] ]
      ]

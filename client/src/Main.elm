module Main exposing (main)

import Browser
import Destiny.Generated.Model exposing (Aspect, Entity, Id, World)
import Html exposing (Html, button, div, input, text, textarea)
import Html.Attributes exposing (checked, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Random

type Message
  = AddEntity
  | AddEntityWithId Id
  | ToggleEntity Entity
  | RemoveEntity Entity
  | AddAspect Entity
  | AddAspectWithId Entity Id
  | EditAspect Aspect
  | RemoveAspect Aspect
  | AddDie Aspect
  | ToggleDie Aspect Int Bool
  | RemoveDie Aspect
  | Roll Aspect
  | RollResult Int

main : Program () World Message
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = always Sub.none
    , view = view
    }

init : () -> (World, Cmd Message)
init _ = ({ entities = [], lastRoll = 0 }, Cmd.none)

update : Message -> World -> (World, Cmd Message)
update message world =
  case message of
    AddEntity ->
      (world, Random.generate AddEntityWithId (Random.int 0 (2^31 - 1)))
    AddEntityWithId id ->
      (addEntity id world, Cmd.none)
    ToggleEntity entity ->
      (updateEntity { entity | collapsed = not entity.collapsed } world, Cmd.none)
    RemoveEntity entity ->
      (removeEntity entity world, Cmd.none)
    AddAspect parent ->
      (world, Random.generate (AddAspectWithId parent) (Random.int 0 (2^31 - 1)))
    AddAspectWithId parent id ->
      (addAspect parent id world, Cmd.none)
    EditAspect aspect ->
      (updateAspect aspect world, Cmd.none)
    RemoveAspect aspect ->
      (removeAspect aspect world, Cmd.none)
    AddDie aspect ->
      (updateAspect { aspect | dice = False :: aspect.dice } world, Cmd.none)
    ToggleDie aspect index selected ->
      (updateAspect { aspect | dice = replace index selected aspect.dice } world, Cmd.none)
    RemoveDie aspect ->
      (removeDie aspect world, Cmd.none)
    Roll aspect ->
      rollAspect aspect world
    RollResult roll ->
      ({ world | lastRoll = roll }, Cmd.none)

replace : Int -> a -> List a -> List a
replace index item = List.indexedMap (\i x -> if i == index then item else x)

addEntity : Id -> World -> World
addEntity id world =
  let entity = { id = id, aspects = [], collapsed = False }
  in { world | entities = entity :: world.entities }

updateEntity : Entity -> World -> World
updateEntity entity world =
  let
    replaceEntity originalEntity =
      if originalEntity.id == entity.id
      then entity
      else originalEntity
  in
    { world | entities = List.map replaceEntity world.entities }

removeEntity : Entity -> World -> World
removeEntity entity world =
  { world | entities = List.filter (\e -> e.id /= entity.id) world.entities }

addAspect : Entity -> Id -> World -> World
addAspect entity id =
  let aspect = { id = id, text = "Hello, world!", dice = [] }
  in updateEntity { entity | aspects = aspect :: entity.aspects }

updateAspect : Aspect -> World -> World
updateAspect aspect world =
  let
    replaceAspect originalAspect =
      if originalAspect.id == aspect.id
      then aspect
      else originalAspect
    updateEntityAspects entity = { entity | aspects = List.map replaceAspect entity.aspects }
  in
    { world | entities = List.map updateEntityAspects world.entities }

removeAspect : Aspect -> World -> World
removeAspect aspect world =
  let
    updateEntityAspects entity =
      { entity | aspects = List.filter (\a -> a.id /= aspect.id) entity.aspects }
  in
    { world | entities = List.map updateEntityAspects world.entities }

rollAspect : Aspect -> World -> (World, Cmd Message)
rollAspect aspect world =
  let
    newWorld = updateAspect { aspect | dice = List.filter not aspect.dice } world
    rolled = List.filter identity aspect.dice |> List.length
    command = Random.generate RollResult (Random.int rolled (6 * rolled))
  in
    (newWorld, command)

removeDie : Aspect -> World -> World
removeDie aspect =
  let
    newDice =
      case aspect.dice of
        [] -> []
        _ :: xs -> xs
  in
    updateAspect { aspect | dice = newDice }

view : World -> Html Message
view world =
  div []
    [ text ("Rolled: " ++ String.fromInt world.lastRoll)
    , button [ onClick AddEntity ] [ text "+" ]
    , div [] (List.map viewEntity world.entities)
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

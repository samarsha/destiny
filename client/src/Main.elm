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
import Html.Attributes exposing (checked, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode exposing (Value, decodeValue)

type Message
  = UpdateWorld World
  | DecodeError Json.Decode.Error
  | Request ClientRequest

port receive : (Value -> msg) -> Sub msg

port send : Value -> Cmd msg

main : Program () World Message
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = always receiveWorld
    , view = view
    }

init : () -> (World, Cmd Message)
init _ = ({ entities = [], lastRoll = 0 }, Cmd.none)

update : Message -> World -> (World, Cmd Message)
update message world =
  case message of
    UpdateWorld newWorld -> (newWorld, Cmd.none)
    DecodeError _ -> (world, Cmd.none)
    Request request -> (world, jsonEncClientRequest request |> send)

receiveWorld : Sub Message
receiveWorld =
  let
    decode value =
      case decodeValue jsonDecWorld value of
        Ok world -> UpdateWorld world
        Err error -> DecodeError error
  in
    receive decode

-- replace : Int -> a -> List a -> List a
-- replace index item = List.indexedMap (\i x -> if i == index then item else x)

-- updateEntity : Entity -> World -> World
-- updateEntity entity world =
--   let
--     replaceEntity originalEntity =
--       if originalEntity.id == entity.id
--       then entity
--       else originalEntity
--   in
--     { world | entities = List.map replaceEntity world.entities }

-- removeEntity : Entity -> World -> World
-- removeEntity entity world =
--   { world | entities = List.filter (\e -> e.id /= entity.id) world.entities }

-- addAspect : Entity -> Id -> World -> World
-- addAspect entity id =
--   let aspect = { id = id, text = "Hello, world!", dice = [] }
--   in updateEntity { entity | aspects = aspect :: entity.aspects }

-- updateAspect : Aspect -> World -> World
-- updateAspect aspect world =
--   let
--     replaceAspect originalAspect =
--       if originalAspect.id == aspect.id
--       then aspect
--       else originalAspect
--     updateEntityAspects entity = { entity | aspects = List.map replaceAspect entity.aspects }
--   in
--     { world | entities = List.map updateEntityAspects world.entities }

-- removeAspect : Aspect -> World -> World
-- removeAspect aspect world =
--   let
--     updateEntityAspects entity =
--       { entity | aspects = List.filter (\a -> a.id /= aspect.id) entity.aspects }
--   in
--     { world | entities = List.map updateEntityAspects world.entities }

-- rollAspect : Aspect -> World -> (World, Cmd Message)
-- rollAspect aspect world =
--   let
--     newWorld = updateAspect { aspect | dice = List.filter not aspect.dice } world
--     rolled = List.filter identity aspect.dice |> List.length
--     command = Random.generate RollResult (Random.int rolled (6 * rolled))
--   in
--     (newWorld, command)

-- removeDie : Aspect -> World -> World
-- removeDie aspect =
--   let
--     newDice =
--       case aspect.dice of
--         [] -> []
--         _ :: xs -> xs
--   in
--     updateAspect { aspect | dice = newDice }

view : World -> Html Message
view world =
  div []
    [ text ("Rolled: " ++ String.fromInt world.lastRoll)
    , button [ onClick (Request AddEntity) ] [ text "+" ]
    , div [] (List.map viewEntity world.entities)
    ]

viewEntity : Entity -> Html Message
viewEntity entity =
  List.append
    [ text "Entity"
    , button [ onClick (ToggleEntity entity |> Request) ]
             [ text (if entity.collapsed then "Show" else "Hide") ]
    , button [ onClick (RemoveEntity entity |> Request) ] [ text "Remove" ]
    , button [ onClick (AddAspect entity |> Request) ] [ text "+" ]
    ]
    ( if entity.collapsed
      then []
      else List.map viewAspect entity.aspects
    )
  |> div []

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
      [ [ textarea [ value aspect.text, onInput edit ] []
        , button [ onClick (RemoveAspect aspect |> Request) ] [ text "Remove" ]
        , button [ onClick (AddDie aspect |> Request) ] [ text "+" ]
        , button [ onClick (RemoveDie aspect |> Request) ] [ text "-" ]
        ]
      , List.indexedMap die aspect.dice
      , [ button [ onClick (Roll aspect |> Request) ] [ text "Roll" ] ]
      ]
    |> div []

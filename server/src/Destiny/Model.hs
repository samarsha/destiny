{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.Model
    ( Aspect
    , ClientRequest
    , Entity
    , World
    , emptyWorld
    , updateWorld
    )
where

import Control.Monad.Random
import Data.UUID
import Destiny.Utils
import Elm.Derive

-- | The world.
data World = World
    { -- | The entities.
      worldEntities :: [Entity]
      -- | The result of the last dice roll.
    , worldLastRoll :: Int
    }

-- | An entity.
data Entity = Entity
    { -- | The entity ID.
      entityId :: UUID
      -- | The aspects that belong to the entity.
    , entityAspects :: [Aspect]
      -- | True if the entity is collapsed.
    , entityCollapsed :: Bool
    }

-- | An aspect.
data Aspect = Aspect
    { -- | The aspect ID.
      aspectId :: UUID
      -- | The description of the aspect.
    , aspectText :: String
      -- | A list of the selected status for each free invoke die.
    , aspectDice :: [Bool]
    }

data ClientRequest
    = AddEntity
    | ToggleEntity Entity
    | RemoveEntity Entity
    | AddAspect Entity
    | EditAspect Aspect
    | RemoveAspect Aspect
    | AddDie Aspect
    | ToggleDie Aspect Int Bool
    | RemoveDie Aspect
    | Roll Aspect

emptyWorld :: World
emptyWorld = World
    { worldEntities = []
    , worldLastRoll = 0
    }

updateWorld :: RandomGen g => ClientRequest -> World -> Rand g World
updateWorld = \case
    AddEntity -> addEntity
    ToggleEntity entity -> return . toggleEntity entity
    RemoveEntity entity -> return . removeEntity entity
    AddAspect entity -> addAspect entity
    EditAspect aspect -> return . updateAspect aspect
    RemoveAspect aspect -> return . removeAspect aspect
    AddDie aspect -> return . addDie aspect
    ToggleDie aspect index selected -> return . toggleDie aspect index selected
    RemoveDie aspect -> return . removeDie aspect
    Roll aspect -> rollDice aspect

addEntity :: RandomGen g => World -> Rand g World
addEntity world@World { worldEntities = entities } = do
    newId <- getRandom
    let entity = Entity { entityId = newId, entityAspects = [], entityCollapsed = False }
    return world { worldEntities = entity : entities }

updateEntity :: Entity -> World -> World
updateEntity entity world@World { worldEntities = entities } = world
    { worldEntities = map replaceEntity entities }
  where
    replaceEntity entity'
        | entityId entity' == entityId entity = entity
        | otherwise = entity'

toggleEntity :: Entity -> World -> World
toggleEntity entity = updateEntity entity
    { entityCollapsed = not (entityCollapsed entity) }

removeEntity :: Entity -> World -> World
removeEntity entity world@World { worldEntities = entities } = world
    { worldEntities = filter (\entity' -> entityId entity' /= entityId entity) entities }

addAspect :: RandomGen g => Entity -> World -> Rand g World
addAspect entity@Entity { entityAspects = aspects } world = do
    newId <- getRandom
    let aspect = Aspect { aspectId = newId, aspectText = "", aspectDice = [] }
    return $ updateEntity entity { entityAspects = aspect : aspects } world

updateAspect :: Aspect -> World -> World
updateAspect aspect world@World { worldEntities = entities } = world
    { worldEntities = map updateEntityAspects entities }
  where
    updateEntityAspects entity@Entity { entityAspects = aspects } = entity
        { entityAspects = map replaceAspect aspects }
    replaceAspect aspect'
        | aspectId aspect' == aspectId aspect = aspect
        | otherwise = aspect'

removeAspect :: Aspect -> World -> World
removeAspect aspect world@World { worldEntities = entities } = world
    { worldEntities = map updateEntityAspects entities }
  where
    updateEntityAspects entity@Entity { entityAspects = aspects } = entity
        { entityAspects = filter (\aspect' -> aspectId aspect' /= aspectId aspect) aspects }

addDie :: Aspect -> World -> World
addDie aspect = updateAspect aspect { aspectDice = False : aspectDice aspect }

toggleDie :: Aspect -> Int -> Bool -> World -> World
toggleDie aspect@Aspect { aspectDice = dice } index selected = updateAspect aspect
    { aspectDice = zipWith replaceIndex [0..] dice }
  where
    replaceIndex i x
        | i == index = selected
        | otherwise = x

removeDie :: Aspect -> World -> World
removeDie aspect = updateAspect aspect { aspectDice = dice' }
  where
    dice' = case aspectDice aspect of
        [] -> []
        _ : xs -> xs

rollDice :: RandomGen g => Aspect -> World -> Rand g World
rollDice aspect@Aspect { aspectDice = dice } world = do
    rolls <- take numRolls <$> getRandomRs (1, 6)
    return world' { worldLastRoll = sum rolls }
  where
    world' = updateAspect aspect { aspectDice = filter not dice } world
    numRolls = length $ filter id dice

deriveBoth (stripFieldPrefixOptions "aspect") ''Aspect
deriveBoth (stripFieldPrefixOptions "entity") ''Entity
deriveBoth (stripFieldPrefixOptions "world") ''World
deriveBoth defaultOptions ''ClientRequest

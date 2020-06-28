{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.Model
    ( Aspect
    , ClientRequest
    , Entity
    , World
    , WorldSnapshot
    , commit
    , emptyWorld
    , updateWorld
    , worldSnapshot
    )
where

import Control.Monad.Random
import Data.Aeson.TH (deriveJSON)
import Data.UUID
import Destiny.Timeline (Timeline)
import Destiny.Utils
import Elm.Derive

import qualified Destiny.Timeline as Timeline

-- | The world.
data World = World
    { -- | The timeline of entities.
      worldTimeline :: Timeline [Entity]
      -- | The result of the last dice roll.
    , worldLastRoll :: Int
    }

-- | A snapshot of the world.
data WorldSnapshot = WorldSnapshot
    { -- | The entities.
      snapshotEntities :: [Entity]
      -- | The result of the last dice roll.
    , snapshotLastRoll :: Int
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
    | MoveEntity Entity Int
    | RemoveEntity Entity
    | AddAspect Entity
    | EditAspect Aspect
    | RemoveAspect Aspect
    | AddDie Aspect
    | ToggleDie Aspect Int Bool
    | RemoveDie Aspect
    | Roll Aspect
    | Undo
    | Redo

deriveJSON (stripFieldPrefixOptions "world") ''World
deriveBoth (stripFieldPrefixOptions "snapshot") ''WorldSnapshot
deriveBoth (stripFieldPrefixOptions "entity") ''Entity
deriveBoth (stripFieldPrefixOptions "aspect") ''Aspect
deriveBoth defaultOptions ''ClientRequest

emptyWorld :: World
emptyWorld = World
    { worldTimeline = Timeline.singleton []
    , worldLastRoll = 0
    }

worldSnapshot :: World -> WorldSnapshot
worldSnapshot World { worldTimeline = timeline, worldLastRoll = lastRoll } = WorldSnapshot
    { snapshotEntities = Timeline.value timeline
    , snapshotLastRoll = lastRoll
    }

updateWorld :: RandomGen g => ClientRequest -> World -> Rand g World
updateWorld = \case
    AddEntity -> addEntity
    ToggleEntity entity -> return . toggleEntity entity
    MoveEntity entity index -> return . moveEntity entity index
    RemoveEntity entity -> return . removeEntity entity
    AddAspect entity -> addAspect entity
    EditAspect aspect -> return . updateAspect aspect
    RemoveAspect aspect -> return . removeAspect aspect
    AddDie aspect -> return . addDie aspect
    ToggleDie aspect index selected -> return . toggleDie aspect index selected
    RemoveDie aspect -> return . removeDie aspect
    Roll aspect -> rollDice aspect
    Undo -> return . undo
    Redo -> return . redo

addEntity :: RandomGen g => World -> Rand g World
addEntity world@World { worldTimeline = timeline } = do
    newId <- getRandom
    let entity = Entity { entityId = newId, entityAspects = [], entityCollapsed = False }
    return world { worldTimeline = Timeline.modify timeline (entity :) }

updateEntity :: Entity -> World -> World
updateEntity entity world@World { worldTimeline = timeline } = world
    { worldTimeline = Timeline.modify timeline $ map replaceEntity }
  where
    replaceEntity entity'
        | entityId entity' == entityId entity = entity
        | otherwise = entity'

toggleEntity :: Entity -> World -> World
toggleEntity entity = updateEntity entity
    { entityCollapsed = not (entityCollapsed entity) }

moveEntity :: Entity -> Int -> World -> World
moveEntity entity index world@World { worldTimeline = timeline } = world
    { worldTimeline = Timeline.modify timeline move }
  where
    move entities = take index removed ++ entity : drop index removed
      where
        removed = filter (\entity' -> entityId entity' /= entityId entity) entities

removeEntity :: Entity -> World -> World
removeEntity entity world@World { worldTimeline = timeline } = world
    { worldTimeline = Timeline.modify timeline $
        filter (\entity' -> entityId entity' /= entityId entity)
    }

addAspect :: RandomGen g => Entity -> World -> Rand g World
addAspect entity@Entity { entityAspects = aspects } world = do
    newId <- getRandom
    let aspect = Aspect { aspectId = newId, aspectText = "", aspectDice = [] }
    return $ updateEntity entity { entityAspects = aspect : aspects } world

updateAspect :: Aspect -> World -> World
updateAspect aspect world@World { worldTimeline = timeline } = world
    { worldTimeline = Timeline.modify timeline $ map updateEntityAspects }
  where
    updateEntityAspects entity@Entity { entityAspects = aspects } = entity
        { entityAspects = map replaceAspect aspects }
    replaceAspect aspect'
        | aspectId aspect' == aspectId aspect = aspect
        | otherwise = aspect'

removeAspect :: Aspect -> World -> World
removeAspect aspect world@World { worldTimeline = timeline } = world
    { worldTimeline = Timeline.modify timeline $ map updateEntityAspects }
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

commit :: World -> World
commit world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.commit timeline }

undo :: World -> World
undo world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.undo timeline }

redo :: World -> World
redo world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.redo timeline }

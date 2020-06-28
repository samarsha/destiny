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
import Data.List
import Data.Maybe
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
      -- | The entity name.
    , entityName :: String
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
    | ToggleEntity UUID          -- Entity ID
    | SetEntityName UUID String  -- Entity ID
    | MoveEntity UUID Int        -- Entity ID
    | RemoveEntity UUID          -- Entity ID
    | AddAspect UUID             -- Entity ID
    | SetAspectText UUID String  -- Aspect ID
    | RemoveAspect UUID          -- Aspect ID
    | AddDie UUID                -- Aspect ID
    | SetDie UUID Int Bool       -- Aspect ID
    | RemoveDie UUID             -- Aspect ID
    | Roll UUID                  -- Aspect ID
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
    ToggleEntity uuid -> return . toggleEntity uuid
    SetEntityName uuid name -> return . setEntityName name uuid
    MoveEntity uuid index -> return . moveEntity index uuid
    RemoveEntity uuid -> return . removeEntity uuid
    AddAspect uuid -> addAspect uuid
    SetAspectText uuid text -> return . setAspectText text uuid
    RemoveAspect uuid -> return . removeAspect uuid
    AddDie uuid -> return . addDie uuid
    SetDie uuid index selected -> return . setDie index selected uuid
    RemoveDie uuid -> return . removeDie uuid
    Roll aspect -> rollDice aspect
    Undo -> return . undo
    Redo -> return . redo

addEntity :: RandomGen g => World -> Rand g World
addEntity world@World { worldTimeline = timeline } = do
    newId <- getRandom
    let entity = Entity
            { entityId = newId
            , entityName = ""
            , entityAspects = []
            , entityCollapsed = False
            }
    return world { worldTimeline = Timeline.modify timeline (entity :) }

modifyEntity :: (Entity -> Maybe Entity) -> UUID -> World -> World
modifyEntity f uuid world@World { worldTimeline = timeline } =
    world { worldTimeline = Timeline.modify timeline $ mapMaybe modify }
  where
    modify entity@Entity { entityId = uuid' }
        | uuid == uuid' = f entity
        | otherwise     = Just entity

toggleEntity :: UUID -> World -> World
toggleEntity = modifyEntity $ \entity@Entity { entityCollapsed = collapsed } ->
    Just $ entity { entityCollapsed = not collapsed }

setEntityName :: String -> UUID -> World -> World
setEntityName name = modifyEntity $ \entity -> Just $ entity { entityName = name }

moveEntity :: Int -> UUID -> World -> World
moveEntity index uuid world@World { worldTimeline = timeline } =
    case find ((==) uuid . entityId) $ Timeline.value timeline of
        Just entity -> world { worldTimeline = Timeline.modify timeline $ move entity }
        Nothing -> world
  where
    move entity entities = take index removed ++ entity : drop index removed
      where
        removed = filter ((/=) uuid . entityId) entities

removeEntity :: UUID -> World -> World
removeEntity = modifyEntity $ const Nothing

addAspect :: RandomGen g => UUID -> World -> Rand g World
addAspect uuid world = do
    newId <- getRandom
    let aspect = Aspect { aspectId = newId, aspectText = "", aspectDice = [] }
    return $ modifyEntity (add aspect) uuid world
  where
    add aspect entity@Entity { entityAspects = aspects } =
        Just $ entity { entityAspects = aspect : aspects }

modifyAspect :: (Aspect -> Maybe Aspect) -> UUID -> World -> World
modifyAspect f uuid world@World { worldTimeline = timeline } = world
    { worldTimeline = Timeline.modify timeline $ map $
        \entity@Entity { entityAspects = aspects } ->
            entity { entityAspects = mapMaybe modify aspects }
    }
  where
    modify aspect@Aspect { aspectId = uuid' }
        | uuid == uuid' = f aspect
        | otherwise     = Just aspect

setAspectText :: String -> UUID -> World -> World
setAspectText text = modifyAspect $ \aspect -> Just $ aspect { aspectText = text }

removeAspect :: UUID -> World -> World
removeAspect = modifyAspect $ const Nothing

addDie :: UUID -> World -> World
addDie = modifyAspect $ \aspect@Aspect { aspectDice = dice } ->
    Just $ aspect { aspectDice = False : dice }

setDie :: Int -> Bool -> UUID -> World -> World
setDie index selected = modifyAspect $ \aspect@Aspect { aspectDice = dice } ->
    Just $ aspect { aspectDice = zipWith replaceIndex [0..] dice }
  where
    replaceIndex i x
        | i == index = selected
        | otherwise  = x

removeDie :: UUID -> World -> World
removeDie = modifyAspect $ \aspect@Aspect { aspectDice = dice } ->
    let dice' = case dice of
                    [] -> []
                    _ : xs -> xs
    in Just $ aspect { aspectDice = dice' }

rollDice :: RandomGen g => UUID -> World -> Rand g World
rollDice uuid world@World { worldTimeline = timeline } = case aspects of
    [Aspect { aspectDice = dice }] -> do
        let numRolls = length $ filter id dice
        rolls <- take numRolls <$> getRandomRs (1, 6)
        let world' = world { worldLastRoll = sum rolls }
        return $ modifyAspect (setDice $ filter not dice) uuid world'
    _ -> return world
  where
    aspects = concatMap (filter ((==) uuid . aspectId) . entityAspects) $ Timeline.value timeline
    setDice dice aspect = Just $ aspect { aspectDice = dice }

undo :: World -> World
undo world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.undo timeline }

redo :: World -> World
redo world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.redo timeline }

commit :: World -> World
commit world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.commit timeline }

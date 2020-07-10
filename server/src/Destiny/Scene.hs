{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Destiny.Scene
    ( Aspect (..)
    , AspectId
    , Entity
    , EntityId
    , Scene (..)
    , Stat (..)
    , StatGroup
    , StatGroupId
    , StatId
    , addAspect
    , addDie
    , addEntity
    , addStat
    , addStatGroup
    , emptyScene
    , modifyAspect
    , moveAspect
    , moveEntity
    , removeAspect
    , removeDie
    , removeEntity
    , removeStat
    , removeStatGroup
    , setAspectText
    , setEntityName
    , setStatGroupName
    , setStatName
    , setStatScore
    , toggleEntity
    )
where

import Control.Monad.Random
import Data.Aeson.Types hiding (defaultOptions)
import Data.List
import Data.List.Extra
import Data.List.Index
import Data.Map.Lazy (Map)
import Data.UUID
import Elm.Derive

import qualified Data.Map.Lazy as Map

data Scene = Scene
    { sceneBoard :: [EntityId]
    , sceneEntities :: Map EntityId Entity
    , sceneStatGroups :: Map StatGroupId StatGroup
    , sceneStats :: Map StatId Stat
    , sceneAspects :: Map AspectId Aspect
    }

newtype EntityId = EntityId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

newtype StatGroupId = StatGroupId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

newtype StatId = StatId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

newtype AspectId = AspectId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

-- | An entity.
data Entity = Entity
    { -- | The entity ID.
      entityId :: EntityId
      -- | The entity name.
    , entityName :: String
      -- | The entity stat groups.
    , entityStatGroups :: [StatGroupId]
      -- | The aspects that belong to the entity.
    , entityAspects :: [AspectId]
      -- | True if the entity is collapsed.
    , entityCollapsed :: Bool
    }

data StatGroup = StatGroup StatGroupId String [StatId]

data Stat = Stat StatId String Int

-- | An aspect.
data Aspect = Aspect
    { -- | The aspect ID.
      aspectId :: AspectId
      -- | The description of the aspect.
    , aspectText :: String
      -- | The number of free invoke dice for the aspect.
    , aspectDice :: Int
    }

deriveBoth defaultOptions ''AspectId
deriveBoth defaultOptions ''EntityId
deriveBoth defaultOptions ''StatGroupId
deriveBoth defaultOptions ''StatId

deriveBoth (defaultOptionsDropLower 6) ''Aspect
deriveBoth (defaultOptionsDropLower 6) ''Entity
deriveBoth (defaultOptionsDropLower 5) ''Scene
deriveBoth defaultOptions ''Stat
deriveBoth defaultOptions ''StatGroup

emptyScene :: Scene
emptyScene = Scene
    { sceneBoard = []
    , sceneEntities = Map.empty
    , sceneStatGroups = Map.empty
    , sceneStats = Map.empty
    , sceneAspects = Map.empty
    }

addEntity :: RandomGen r => Scene -> Rand r Scene
addEntity scene = do
    newId <- getRandom
    let entity = Entity
            { entityId = newId
            , entityName = ""
            , entityStatGroups = []
            , entityAspects = []
            , entityCollapsed = False
            }
    return scene
        { sceneEntities = Map.insert newId entity $ sceneEntities scene
        , sceneBoard = snoc (sceneBoard scene) newId
        }

modifyEntity :: (Entity -> Entity) -> EntityId -> Scene -> Scene
modifyEntity f eid scene = scene { sceneEntities = Map.adjust f eid $ sceneEntities scene }

toggleEntity :: EntityId -> Scene -> Scene
toggleEntity = modifyEntity $ \entity -> entity { entityCollapsed = not $ entityCollapsed entity }

setEntityName :: String -> EntityId -> Scene -> Scene
setEntityName name = modifyEntity $ \entity -> entity { entityName = name }

moveEntity :: Int -> EntityId -> Scene -> Scene
moveEntity index eid scene = scene { sceneBoard = take index removed ++ eid : drop index removed }
  where
    removed = delete eid $ sceneBoard scene

removeEntity :: EntityId -> Scene -> Scene
removeEntity eid scene = scene
    { sceneBoard = delete eid $ sceneBoard scene
    , sceneEntities = Map.delete eid $ sceneEntities scene
    }

addStatGroup :: RandomGen r => EntityId -> Scene -> Rand r Scene
addStatGroup eid scene = do
    sgid <- getRandom
    let statGroup = StatGroup sgid "" []
    return scene
        { sceneEntities = Map.adjust (addToEntity sgid) eid $ sceneEntities scene
        , sceneStatGroups = Map.insert sgid statGroup $ sceneStatGroups scene
        }
  where
    addToEntity sgid entity@Entity { entityStatGroups = statGroups } =
        entity { entityStatGroups = snoc statGroups sgid }

setStatGroupName :: String -> StatGroupId -> Scene -> Scene
setStatGroupName name sgid scene =
    scene { sceneStatGroups = Map.adjust update sgid $ sceneStatGroups scene }
  where
    update (StatGroup sgid' _ stats) = StatGroup sgid' name stats

removeStatGroup :: StatGroupId -> Scene -> Scene
removeStatGroup sgid scene = scene
    { sceneEntities = Map.map updateEntity $ sceneEntities scene
    , sceneStatGroups = Map.delete sgid $ sceneStatGroups scene
    }
  where
    updateEntity entity = entity { entityStatGroups = delete sgid $ entityStatGroups entity }

addStat :: RandomGen r => StatGroupId -> Scene -> Rand r Scene
addStat sgid scene = do
    sid <- getRandom
    let stat = Stat sid "" 0
    return scene
        { sceneStatGroups = Map.adjust (addToGroup sid) sgid $ sceneStatGroups scene
        , sceneStats = Map.insert sid stat $ sceneStats scene
        }
  where
    addToGroup sid (StatGroup sgid' name stats) = StatGroup sgid' name $ snoc stats sid

modifyStat :: (Stat -> Stat) -> StatId -> Scene -> Scene
modifyStat f sid scene = scene { sceneStats = Map.adjust f sid $ sceneStats scene }

setStatName :: String -> StatId -> Scene -> Scene
setStatName name = modifyStat $ \(Stat sid _ score) -> Stat sid name score

setStatScore :: Int -> StatId -> Scene -> Scene
setStatScore score = modifyStat $ \(Stat sid name _) -> Stat sid name score

removeStat :: StatId -> Scene -> Scene
removeStat sid scene = scene
    { sceneStatGroups = Map.map updateGroup $ sceneStatGroups scene
    , sceneStats = Map.delete sid $ sceneStats scene
    }
  where
    updateGroup (StatGroup sgid name stats) = StatGroup sgid name $ delete sid stats

addAspect :: RandomGen r => EntityId -> Scene -> Rand r Scene
addAspect eid scene = do
    aid <- getRandom
    let aspect = Aspect { aspectId = aid, aspectText = "", aspectDice = 0 }
    return scene
        { sceneEntities = Map.adjust (append aid) eid $ sceneEntities scene
        , sceneAspects = Map.insert aid aspect $ sceneAspects scene
        }
  where
    append aid entity = entity { entityAspects = snoc (entityAspects entity) aid }

modifyAspect :: (Aspect -> Aspect) -> AspectId -> Scene -> Scene
modifyAspect f aid scene = scene { sceneAspects = Map.adjust f aid $ sceneAspects scene }

setAspectText :: String -> AspectId -> Scene -> Scene
setAspectText text = modifyAspect $ \aspect -> aspect { aspectText = text }

moveAspect :: AspectId -> EntityId -> Int -> Scene -> Scene
moveAspect aid eid index scene = scene
    { sceneEntities = Map.adjust add eid $ Map.map remove $ sceneEntities scene
    }
  where
    remove entity = entity { entityAspects = delete aid $ entityAspects entity }
    add entity = entity { entityAspects = insertAt index aid $ entityAspects entity }

removeAspect :: AspectId -> Scene -> Scene
removeAspect aid scene = scene
    { sceneEntities = Map.map updateEntity $ sceneEntities scene
    , sceneAspects = Map.delete aid $ sceneAspects scene
    }
  where
    updateEntity entity = entity { entityAspects = delete aid $ entityAspects entity }

addDie :: AspectId -> Scene -> Scene
addDie = modifyAspect $ \aspect -> aspect { aspectDice = aspectDice aspect + 1 }

removeDie :: AspectId -> Scene -> Scene
removeDie = modifyAspect $ \aspect@Aspect { aspectDice = dice } ->
    if dice >= 1
        then aspect { aspectDice = dice - 1 }
        else aspect

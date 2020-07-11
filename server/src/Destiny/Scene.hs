{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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
    { board :: [EntityId]
    , entities :: Map EntityId Entity
    , statGroups :: Map StatGroupId StatGroup
    , stats :: Map StatId Stat
    , aspects :: Map AspectId Aspect
    }

newtype EntityId = EntityId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

newtype StatGroupId = StatGroupId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

newtype StatId = StatId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

newtype AspectId = AspectId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

-- | An entity.
data Entity = Entity
    { -- | The entity ID.
      id :: EntityId
      -- | The entity name.
    , name :: String
      -- | The entity stat groups.
    , statGroups :: [StatGroupId]
      -- | The aspects that belong to the entity.
    , aspects :: [AspectId]
      -- | True if the entity is collapsed.
    , collapsed :: Bool
    }

data StatGroup = StatGroup StatGroupId String [StatId]

data Stat = Stat StatId String Int

-- | An aspect.
data Aspect = Aspect
    { -- | The aspect ID.
      id :: AspectId
      -- | The description of the aspect.
    , text :: String
      -- | The number of free invoke dice for the aspect.
    , dice :: Int
    }

deriveBoth defaultOptions ''AspectId
deriveBoth defaultOptions ''EntityId
deriveBoth defaultOptions ''StatGroupId
deriveBoth defaultOptions ''StatId

deriveBoth defaultOptions ''Aspect
deriveBoth defaultOptions ''Entity
deriveBoth defaultOptions ''Scene
deriveBoth defaultOptions ''Stat
deriveBoth defaultOptions ''StatGroup

emptyScene :: Scene
emptyScene = Scene
    { board = []
    , entities = Map.empty
    , statGroups = Map.empty
    , stats = Map.empty
    , aspects = Map.empty
    }

addEntity :: RandomGen r => Scene -> Rand r Scene
addEntity scene = do
    newId <- getRandom
    let entity = Entity
            { id = newId
            , name = ""
            , statGroups = []
            , aspects = []
            , collapsed = False
            }
    return scene
        { entities = Map.insert newId entity $ entities scene
        , board = snoc (board scene) newId
        }

modifyEntity :: (Entity -> Entity) -> EntityId -> Scene -> Scene
modifyEntity f eid scene = scene { entities = Map.adjust f eid $ entities scene }

toggleEntity :: EntityId -> Scene -> Scene
toggleEntity = modifyEntity $ \entity -> entity { collapsed = not $ collapsed entity }

setEntityName :: String -> EntityId -> Scene -> Scene
setEntityName name' = modifyEntity $ \entity -> entity { name = name' }

moveEntity :: Int -> EntityId -> Scene -> Scene
moveEntity index eid scene = scene { board = take index removed ++ eid : drop index removed }
  where
    removed = delete eid $ board scene

removeEntity :: EntityId -> Scene -> Scene
removeEntity eid scene = scene
    { board = delete eid $ board scene
    , entities = Map.delete eid $ entities scene
    }

addStatGroup :: RandomGen r => EntityId -> Scene -> Rand r Scene
addStatGroup eid scene = do
    sgid <- getRandom
    let sg = StatGroup sgid "" []
    return scene
        { entities = Map.adjust (addToEntity sgid) eid $ entities scene
        , statGroups = Map.insert sgid sg $ statGroups (scene :: Scene)
        }
  where
    addToEntity :: StatGroupId -> Entity -> Entity
    addToEntity sgid entity = entity { statGroups = snoc (statGroups (entity :: Entity)) sgid }

setStatGroupName :: String -> StatGroupId -> Scene -> Scene
setStatGroupName name' sgid scene =
    scene { statGroups = Map.adjust update sgid $ statGroups (scene :: Scene) }
  where
    update (StatGroup sgid' _ stats') = StatGroup sgid' name' stats'

removeStatGroup :: StatGroupId -> Scene -> Scene
removeStatGroup sgid scene = scene
    { entities = Map.map updateEntity $ entities scene
    , statGroups = Map.delete sgid $ statGroups (scene :: Scene)
    }
  where
    updateEntity :: Entity -> Entity
    updateEntity entity = entity { statGroups = delete sgid $ statGroups (entity :: Entity) }

addStat :: RandomGen r => StatGroupId -> Scene -> Rand r Scene
addStat sgid scene = do
    sid <- getRandom
    let stat = Stat sid "" 0
    return scene
        { statGroups = Map.adjust (addToGroup sid) sgid $ statGroups (scene :: Scene)
        , stats = Map.insert sid stat $ stats scene
        }
  where
    addToGroup sid (StatGroup sgid' name' stats') = StatGroup sgid' name' $ snoc stats' sid

modifyStat :: (Stat -> Stat) -> StatId -> Scene -> Scene
modifyStat f sid scene = scene { stats = Map.adjust f sid $ stats scene }

setStatName :: String -> StatId -> Scene -> Scene
setStatName name' = modifyStat $ \(Stat sid _ score) -> Stat sid name' score

setStatScore :: Int -> StatId -> Scene -> Scene
setStatScore score = modifyStat $ \(Stat sid name' _) -> Stat sid name' score

removeStat :: StatId -> Scene -> Scene
removeStat sid scene = scene
    { statGroups = Map.map updateGroup $ statGroups (scene :: Scene)
    , stats = Map.delete sid $ stats scene
    }
  where
    updateGroup (StatGroup sgid name' stats') = StatGroup sgid name' $ delete sid stats'

addAspect :: RandomGen r => EntityId -> Scene -> Rand r Scene
addAspect eid scene = do
    aid <- getRandom
    let aspect = Aspect { id = aid, text = "", dice = 0 }
    return scene
        { entities = Map.adjust (append aid) eid $ entities scene
        , aspects = Map.insert aid aspect $ aspects (scene :: Scene)
        }
  where
    append :: AspectId -> Entity -> Entity
    append aid entity = entity { aspects = snoc (aspects (entity :: Entity)) aid }

modifyAspect :: (Aspect -> Aspect) -> AspectId -> Scene -> Scene
modifyAspect f aid scene = scene { aspects = Map.adjust f aid $ aspects (scene :: Scene) }

setAspectText :: String -> AspectId -> Scene -> Scene
setAspectText text' = modifyAspect $ \aspect -> aspect { text = text' }

moveAspect :: AspectId -> EntityId -> Int -> Scene -> Scene
moveAspect aid eid index scene = scene
    { entities = Map.adjust add eid $ Map.map remove $ entities scene
    }
  where
    remove :: Entity -> Entity
    remove entity = entity { aspects = delete aid $ aspects (entity :: Entity) }
    add :: Entity -> Entity
    add entity = entity { aspects = insertAt index aid $ aspects (entity :: Entity) }

removeAspect :: AspectId -> Scene -> Scene
removeAspect aid scene = scene
    { entities = Map.map updateEntity $ entities scene
    , aspects = Map.delete aid $ aspects (scene :: Scene)
    }
  where
    updateEntity :: Entity -> Entity
    updateEntity entity = entity { aspects = delete aid $ aspects (entity :: Entity) }

addDie :: AspectId -> Scene -> Scene
addDie = modifyAspect $ \aspect -> aspect { dice = dice aspect + 1 }

removeDie :: AspectId -> Scene -> Scene
removeDie = modifyAspect $ \aspect ->
    if dice aspect >= 1
        then aspect { dice = dice aspect - 1 }
        else aspect

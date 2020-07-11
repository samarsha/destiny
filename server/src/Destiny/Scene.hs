{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Destiny.Scene
    ( Aspect
    , AspectId
    , Destiny.Scene.id
    , Entity
    , EntityId
    , Scene
    , Stat (..)
    , StatGroup
    , StatGroupId
    , StatId
    , addAspect
    , addDie
    , addEntity
    , addStat
    , addStatGroup
    , aspects
    , dice
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
    , stats
    , text
    , toggleEntity
    )
where

import Control.Lens
import Control.Monad.Random
import Data.Aeson.Types hiding (defaultOptions)
import Data.List
import Data.List.Index
import Data.Map.Lazy (Map)
import Data.UUID
import Elm.Derive

import qualified Data.Map.Lazy as Map

newtype StatId = StatId UUID
    deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)
deriveBoth defaultOptions ''StatId

data Stat = Stat StatId String Int
deriveBoth defaultOptions ''Stat

newtype StatGroupId = StatGroupId UUID
    deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)
deriveBoth defaultOptions ''StatGroupId

data StatGroup = StatGroup StatGroupId String [StatId]
deriveBoth defaultOptions ''StatGroup

newtype AspectId = AspectId UUID
    deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)
deriveBoth defaultOptions ''AspectId

-- | An aspect.
data Aspect = Aspect
    { -- | The aspect ID.
      _id :: AspectId
      -- | The description of the aspect.
    , _text :: String
      -- | The number of free invoke dice for the aspect.
    , _dice :: Int
    }
deriveBoth (defaultOptionsDropLower 1) ''Aspect
makeFieldsNoPrefix ''Aspect

newtype EntityId = EntityId UUID
    deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)
deriveBoth defaultOptions ''EntityId

-- | An entity.
data Entity = Entity
    { -- | The entity ID.
      _id :: EntityId
      -- | The entity name.
    , _name :: String
      -- | The entity stat groups.
    , _statGroups :: [StatGroupId]
      -- | The aspects that belong to the entity.
    , _aspects :: [AspectId]
      -- | True if the entity is collapsed.
    , _collapsed :: Bool
    }
deriveBoth (defaultOptionsDropLower 1) ''Entity
makeFieldsNoPrefix ''Entity

data Scene = Scene
    { _board :: [EntityId]
    , _entities :: Map EntityId Entity
    , _statGroups :: Map StatGroupId StatGroup
    , _stats :: Map StatId Stat
    , _aspects :: Map AspectId Aspect
    }
deriveBoth (defaultOptionsDropLower 1) ''Scene
makeFieldsNoPrefix ''Scene

emptyScene :: Scene
emptyScene = Scene
    { _board = []
    , _entities = Map.empty
    , _statGroups = Map.empty
    , _stats = Map.empty
    , _aspects = Map.empty
    }

addEntity :: RandomGen r => Scene -> Rand r Scene
addEntity scene = do
    newId <- getRandom
    let entity = Entity
            { _id = newId
            , _name = ""
            , _statGroups = []
            , _aspects = []
            , _collapsed = False
            }
    return $ scene
        & entities %~ Map.insert newId entity
        & board %~ flip snoc newId

modifyEntity :: (Entity -> Entity) -> EntityId -> Scene -> Scene
modifyEntity f eid = entities %~ Map.adjust f eid

toggleEntity :: EntityId -> Scene -> Scene
toggleEntity = modifyEntity $ collapsed %~ not

setEntityName :: String -> EntityId -> Scene -> Scene
setEntityName = modifyEntity . set name

moveEntity :: Int -> EntityId -> Scene -> Scene
moveEntity i eid scene = scene & board .~ moved
  where
    removed = delete eid $ scene ^. board
    moved = take i removed ++ eid : drop i removed

removeEntity :: EntityId -> Scene -> Scene
removeEntity eid scene = scene
    & board %~ delete eid
    & entities %~ Map.delete eid

addStatGroup :: RandomGen r => EntityId -> Scene -> Rand r Scene
addStatGroup eid scene = do
    sgid <- getRandom
    let group' = StatGroup sgid "" []
    return $ scene
        & entities %~ Map.adjust (statGroups %~ flip snoc sgid) eid
        & statGroups %~ Map.insert sgid group'

setStatGroupName :: String -> StatGroupId -> Scene -> Scene
setStatGroupName name' sgid = statGroups %~ Map.adjust update sgid
  where
    update (StatGroup sgid' _ stats') = StatGroup sgid' name' stats'

removeStatGroup :: StatGroupId -> Scene -> Scene
removeStatGroup sgid scene = scene
    & entities %~ Map.map (statGroups %~ delete sgid)
    & statGroups %~ Map.delete sgid

addStat :: RandomGen r => StatGroupId -> Scene -> Rand r Scene
addStat sgid scene = do
    sid <- getRandom
    let stat = Stat sid "" 0
    return $ scene
        & statGroups %~ Map.adjust (addToGroup sid) sgid
        & stats %~ Map.insert sid stat
  where
    addToGroup sid (StatGroup sgid' name' stats') = StatGroup sgid' name' $ snoc stats' sid

modifyStat :: (Stat -> Stat) -> StatId -> Scene -> Scene
modifyStat f sid = stats %~ Map.adjust f sid

setStatName :: String -> StatId -> Scene -> Scene
setStatName name' = modifyStat $ \(Stat sid _ score) -> Stat sid name' score

setStatScore :: Int -> StatId -> Scene -> Scene
setStatScore score = modifyStat $ \(Stat sid name' _) -> Stat sid name' score

removeStat :: StatId -> Scene -> Scene
removeStat sid scene = scene
    & statGroups %~ Map.map updateGroup
    & stats %~ Map.delete sid
  where
    updateGroup (StatGroup sgid name' stats') = StatGroup sgid name' $ delete sid stats'

addAspect :: RandomGen r => EntityId -> Scene -> Rand r Scene
addAspect eid scene = do
    aid <- getRandom
    let aspect = Aspect { _id = aid, _text = "", _dice = 0 }
    return $ scene
        & entities %~ Map.adjust (aspects %~ flip snoc aid) eid
        & aspects %~ Map.insert aid aspect

modifyAspect :: (Aspect -> Aspect) -> AspectId -> Scene -> Scene
modifyAspect f aid = aspects %~ Map.adjust f aid

setAspectText :: String -> AspectId -> Scene -> Scene
setAspectText = modifyAspect . set text

moveAspect :: AspectId -> EntityId -> Int -> Scene -> Scene
moveAspect aid eid i = entities
    %~ Map.adjust (aspects %~ insertAt i aid) eid
    .  Map.map (aspects %~ delete aid)

removeAspect :: AspectId -> Scene -> Scene
removeAspect aid scene = scene
    & entities %~ Map.map (aspects %~ delete aid)
    & aspects %~ Map.delete aid

addDie :: AspectId -> Scene -> Scene
addDie = modifyAspect $ dice +~ 1

removeDie :: AspectId -> Scene -> Scene
removeDie = modifyAspect $ \aspect ->
    if aspect ^. dice >= 1
        then aspect & dice -~ 1
        else aspect

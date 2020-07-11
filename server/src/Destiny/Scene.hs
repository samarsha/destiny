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
    , name
    , removeAspect
    , removeDie
    , removeEntity
    , removeStat
    , removeStatGroup
    , score
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

data Stat = Stat
    { _id :: StatId
    , _name :: String
    , _score :: Int
    }
deriveBoth (defaultOptionsDropLower 1) ''Stat
makeFieldsNoPrefix ''Stat

newtype StatGroupId = StatGroupId UUID
    deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)
deriveBoth defaultOptions ''StatGroupId

data StatGroup = StatGroup
    { _id :: StatGroupId
    , _name :: String
    , _stats :: [StatId]
    }
deriveBoth (defaultOptionsDropLower 1) ''StatGroup
makeFieldsNoPrefix ''StatGroup

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
    entityId <- getRandom
    let entity = Entity
            { _id = entityId
            , _name = ""
            , _statGroups = []
            , _aspects = []
            , _collapsed = False
            }
    return $ scene
        & entities %~ Map.insert entityId entity
        & board %~ flip snoc entityId

modifyEntity :: (Entity -> Entity) -> EntityId -> Scene -> Scene
modifyEntity f entityId = entities %~ Map.adjust f entityId

toggleEntity :: EntityId -> Scene -> Scene
toggleEntity = modifyEntity $ collapsed %~ not

setEntityName :: String -> EntityId -> Scene -> Scene
setEntityName = modifyEntity . set name

moveEntity :: Int -> EntityId -> Scene -> Scene
moveEntity i entityId scene = scene & board .~ moved
  where
    removed = delete entityId $ scene^.board
    moved = take i removed ++ entityId : drop i removed

removeEntity :: EntityId -> Scene -> Scene
removeEntity entityId scene = scene
    & board %~ delete entityId
    & entities %~ Map.delete entityId

addStatGroup :: RandomGen r => EntityId -> Scene -> Rand r Scene
addStatGroup entityId scene = do
    groupId <- getRandom
    let group' = StatGroup groupId "" []
    return $ scene
        & entities %~ Map.adjust (statGroups %~ flip snoc groupId) entityId
        & statGroups %~ Map.insert groupId group'

setStatGroupName :: String -> StatGroupId -> Scene -> Scene
setStatGroupName name' groupId = statGroups %~ Map.adjust (name .~ name') groupId

removeStatGroup :: StatGroupId -> Scene -> Scene
removeStatGroup groupId scene = scene
    & entities %~ Map.map (statGroups %~ delete groupId)
    & statGroups %~ Map.delete groupId

addStat :: RandomGen r => StatGroupId -> Scene -> Rand r Scene
addStat groupId scene = do
    statId <- getRandom
    let stat = Stat { _id = statId, _name = "", _score = 0 }
    return $ scene
        & statGroups %~ Map.adjust (stats %~ flip snoc statId) groupId
        & stats %~ Map.insert statId stat

modifyStat :: (Stat -> Stat) -> StatId -> Scene -> Scene
modifyStat f statId = stats %~ Map.adjust f statId

setStatName :: String -> StatId -> Scene -> Scene
setStatName = modifyStat . set name

setStatScore :: Int -> StatId -> Scene -> Scene
setStatScore = modifyStat . set score

removeStat :: StatId -> Scene -> Scene
removeStat statId scene = scene
    & statGroups %~ Map.map (stats %~ delete statId)
    & stats %~ Map.delete statId

addAspect :: RandomGen r => EntityId -> Scene -> Rand r Scene
addAspect entityId scene = do
    aspectId <- getRandom
    let aspect = Aspect { _id = aspectId, _text = "", _dice = 0 }
    return $ scene
        & entities %~ Map.adjust (aspects %~ flip snoc aspectId) entityId
        & aspects %~ Map.insert aspectId aspect

modifyAspect :: (Aspect -> Aspect) -> AspectId -> Scene -> Scene
modifyAspect f aspectId = aspects %~ Map.adjust f aspectId

setAspectText :: String -> AspectId -> Scene -> Scene
setAspectText = modifyAspect . set text

moveAspect :: AspectId -> EntityId -> Int -> Scene -> Scene
moveAspect aspectId entityId i = entities
    %~ Map.adjust (aspects %~ insertAt i aspectId) entityId
    .  Map.map (aspects %~ delete aspectId)

removeAspect :: AspectId -> Scene -> Scene
removeAspect aspectId scene = scene
    & entities %~ Map.map (aspects %~ delete aspectId)
    & aspects %~ Map.delete aspectId

addDie :: AspectId -> Scene -> Scene
addDie = modifyAspect $ dice +~ 1

removeDie :: AspectId -> Scene -> Scene
removeDie = modifyAspect $ \aspect ->
    if aspect^.dice >= 1
        then aspect & dice -~ 1
        else aspect

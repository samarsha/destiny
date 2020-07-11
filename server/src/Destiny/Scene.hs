{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.Scene
    ( Aspect
    , AspectId
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

import Control.Lens
import Control.Monad.Random
import Data.Aeson.Types hiding (defaultOptions)
import Data.Generics.Labels ()
import Data.List
import Data.List.Index
import Data.Map.Lazy (Map)
import Data.UUID
import Elm.Derive
import GHC.Generics

import qualified Data.Map.Lazy as Map

newtype StatId = StatId UUID
    deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)
deriveBoth defaultOptions ''StatId

data Stat = Stat
    { id :: StatId
    , name :: String
    , score :: Int
    }
    deriving Generic
deriveBoth defaultOptions ''Stat

newtype StatGroupId = StatGroupId UUID
    deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)
deriveBoth defaultOptions ''StatGroupId

data StatGroup = StatGroup
    { id :: StatGroupId
    , name :: String
    , stats :: [StatId]
    }
    deriving Generic
deriveBoth defaultOptions ''StatGroup

newtype AspectId = AspectId UUID
    deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)
deriveBoth defaultOptions ''AspectId

-- | An aspect.
data Aspect = Aspect
    { -- | The aspect ID.
      id :: AspectId
      -- | The description of the aspect.
    , text :: String
      -- | The number of free invoke dice for the aspect.
    , dice :: Int
    }
    deriving Generic
deriveBoth defaultOptions ''Aspect

newtype EntityId = EntityId UUID
    deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)
deriveBoth defaultOptions ''EntityId

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
    deriving Generic
deriveBoth defaultOptions ''Entity

data Scene = Scene
    { board :: [EntityId]
    , entities :: Map EntityId Entity
    , statGroups :: Map StatGroupId StatGroup
    , stats :: Map StatId Stat
    , aspects :: Map AspectId Aspect
    }
    deriving Generic
deriveBoth defaultOptions ''Scene

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
    entityId <- getRandom
    let entity = Entity
            { id = entityId
            , name = ""
            , statGroups = []
            , aspects = []
            , collapsed = False
            }
    return $ scene
        & over #entities (Map.insert entityId entity)
        & over #board (flip snoc entityId)

modifyEntity :: (Entity -> Entity) -> EntityId -> Scene -> Scene
modifyEntity f entityId = over #entities $ Map.adjust f entityId

toggleEntity :: EntityId -> Scene -> Scene
toggleEntity = modifyEntity $ over #collapsed not

setEntityName :: String -> EntityId -> Scene -> Scene
setEntityName = modifyEntity . set #name

moveEntity :: Int -> EntityId -> Scene -> Scene
moveEntity i entityId scene = scene & #board .~ moved
  where
    removed = delete entityId $ scene ^. #board
    moved = take i removed ++ entityId : drop i removed

removeEntity :: EntityId -> Scene -> Scene
removeEntity entityId scene = scene
    & over #board (delete entityId)
    & over #entities (Map.delete entityId)

addStatGroup :: RandomGen r => EntityId -> Scene -> Rand r Scene
addStatGroup entityId scene = do
    groupId <- getRandom
    let group' = StatGroup groupId "" []
    return $ scene
        & over #entities (Map.adjust (over #statGroups $ flip snoc groupId) entityId)
        & over #statGroups (Map.insert groupId group')

setStatGroupName :: String -> StatGroupId -> Scene -> Scene
setStatGroupName name' groupId = over #statGroups $ Map.adjust (#name .~ name') groupId

removeStatGroup :: StatGroupId -> Scene -> Scene
removeStatGroup groupId scene = scene
    & over #entities (Map.map $ over #statGroups $ delete groupId)
    & over #statGroups (Map.delete groupId)

addStat :: RandomGen r => StatGroupId -> Scene -> Rand r Scene
addStat groupId scene = do
    statId <- getRandom
    let stat = Stat { id = statId, name = "", score = 0 }
    return $ scene
        & over #statGroups (Map.adjust (over #stats $ flip snoc statId) groupId)
        & over #stats (Map.insert statId stat)

modifyStat :: (Stat -> Stat) -> StatId -> Scene -> Scene
modifyStat f statId = over #stats $ Map.adjust f statId

setStatName :: String -> StatId -> Scene -> Scene
setStatName = modifyStat . set #name

setStatScore :: Int -> StatId -> Scene -> Scene
setStatScore = modifyStat . set #score

removeStat :: StatId -> Scene -> Scene
removeStat statId scene = scene
    & over #statGroups (Map.map $ over #stats $ delete statId)
    & over #stats (Map.delete statId)

addAspect :: RandomGen r => EntityId -> Scene -> Rand r Scene
addAspect entityId scene = do
    aspectId <- getRandom
    let aspect = Aspect { id = aspectId, text = "", dice = 0 }
    return $ scene
        & over #entities (Map.adjust (over #aspects $ flip snoc aspectId) entityId)
        & over #aspects (Map.insert aspectId aspect)

modifyAspect :: (Aspect -> Aspect) -> AspectId -> Scene -> Scene
modifyAspect f aspectId = over #aspects $ Map.adjust f aspectId

setAspectText :: String -> AspectId -> Scene -> Scene
setAspectText = modifyAspect . set #text

moveAspect :: AspectId -> EntityId -> Int -> Scene -> Scene
moveAspect aspectId entityId i = over #entities
    $ Map.adjust (over #aspects $ insertAt i aspectId) entityId
    . Map.map (over #aspects $ delete aspectId)

removeAspect :: AspectId -> Scene -> Scene
removeAspect aspectId scene = scene
    & over #entities (Map.map $ over #aspects $ delete aspectId)
    & over #aspects (Map.delete aspectId)

addDie :: AspectId -> Scene -> Scene
addDie = modifyAspect $ #dice +~ 1

removeDie :: AspectId -> Scene -> Scene
removeDie = modifyAspect $ \aspect ->
    if aspect ^. #dice >= 1
        then aspect & #dice -~ 1
        else aspect

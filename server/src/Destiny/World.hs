{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.World
    ( Event (..)
    , InvokeRoll
    , Roll
    , RollId
    , World (..)
    , WorldSnapshot
    , commit
    , emptyWorld
    , undo
    , redo
    , rollAspect
    , rollStat
    , worldSnapshot
    )
where

import Control.Monad.Random
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types hiding (defaultOptions)
import Data.List.Extra
import Data.Map.Lazy (Map)
import Data.UUID
import Destiny.Scene
import Destiny.Timeline (Timeline)
import Elm.Derive

import qualified Data.Map.Lazy as Map
import qualified Destiny.Timeline as Timeline

-- | The world.
data World = World
    { -- | The timeline of scenes.
      worldTimeline :: Timeline Scene
      -- | The events that have occurred.
    , worldEvents :: [Event]
      -- | The rolls that hath been rolled.
    , worldRolls :: Map RollId Roll
    }

-- | A snapshot of the world.
data WorldSnapshot = WorldSnapshot
    { -- | The scene.
      snapshotScene :: Scene
      -- | The events that have occurred.
    , snapshotEvents :: [Event]
      -- | The rolls that hath been rolled.
    , snapshotRolls :: Map RollId Roll
    }

data Event = RollEvent RollId

data Roll = Roll {
    rollId :: RollId,
    rollStatName :: String,
    rollStatRollValue :: Int,
    rollStatModifier :: Int,
    rollInvokes :: [InvokeRoll]
}

data InvokeRoll = InvokeRoll {
    invokeSource :: String,
    invokeValue :: Int
}

newtype RollId = RollId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

deriveBoth defaultOptions ''RollId

deriveBoth (defaultOptionsDropLower 6) ''InvokeRoll
deriveBoth (defaultOptionsDropLower 4) ''Roll
deriveBoth defaultOptions ''Event
deriveJSON (defaultOptionsDropLower 5) ''World
deriveBoth (defaultOptionsDropLower 8) ''WorldSnapshot

emptyWorld :: World
emptyWorld = World
    { worldTimeline = Timeline.singleton emptyScene
    , worldEvents = []
    , worldRolls = Map.empty
    }

worldSnapshot :: World -> WorldSnapshot
worldSnapshot world = WorldSnapshot {
    snapshotScene = Timeline.value (worldTimeline world),
    snapshotEvents = worldEvents world,
    snapshotRolls = worldRolls world
}

undo :: World -> World
undo world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.undo timeline }

redo :: World -> World
redo world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.redo timeline }

commit :: World -> World
commit world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.commit timeline }

rollStat :: RandomGen r => RollId -> StatId -> World -> Rand r World
rollStat rid sid world@(World timeline events rolls) = case Map.lookup sid stats of
    Just (Stat _ name score) -> do
        -- TODO: Generate roll ID on the server and send it to just the client that initiated the
        -- roll.
        rollValue <- getRandomR (1, 6)
        let roll = Roll {
            rollId = rid,
            rollStatName = name,
            rollStatRollValue = rollValue,
            rollStatModifier = score,
            rollInvokes = []
        }
        return $ world {
            worldEvents = snoc events $ RollEvent rid,
            worldRolls = Map.insert rid roll rolls
        }
    Nothing -> return world
  where
    stats = sceneStats $ Timeline.value timeline

rollAspect :: RandomGen r => RollId -> AspectId -> World -> Rand r World
rollAspect rid aid world@(World timeline events rolls) =
    case Map.lookup aid aspects of
        Just (Aspect { aspectDice = dice, aspectText = name }) | dice >= 1 -> do
            roll <- getRandomR (1, 6)
            let invokeRoll = InvokeRoll {
                invokeValue = roll,
                invokeSource = name
            }
            return world {
                worldTimeline = Timeline.modify timeline (removeDie aid),
                worldRolls = Map.adjust (appendInvoke invokeRoll) rid rolls
            }
        _ -> return world
  where
    aspects = sceneAspects $ Timeline.value timeline
    appendInvoke :: InvokeRoll -> Roll -> Roll
    appendInvoke invokeRoll roll = roll {
        rollInvokes = snoc (rollInvokes roll) invokeRoll
    }

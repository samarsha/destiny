{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.World
    ( Event (..)
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
    }

-- | A snapshot of the world.
data WorldSnapshot = WorldSnapshot
    { -- | The scene.
      snapshotScene :: Scene
      -- | The events that have occurred.
    , snapshotEvents :: [Event]
    }

data Event = RollResult RollId [Int]

newtype RollId = RollId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

deriveBoth defaultOptions ''RollId

deriveBoth defaultOptions ''Event
deriveJSON (defaultOptionsDropLower 5) ''World
deriveBoth (defaultOptionsDropLower 8) ''WorldSnapshot

emptyWorld :: World
emptyWorld = World
    { worldTimeline = Timeline.singleton emptyScene
    , worldEvents = []
    }

worldSnapshot :: World -> WorldSnapshot
worldSnapshot World { worldTimeline = timeline, worldEvents = events } = WorldSnapshot
    { snapshotScene = Timeline.value timeline
    , snapshotEvents = events
    }

undo :: World -> World
undo world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.undo timeline }

redo :: World -> World
redo world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.redo timeline }

commit :: World -> World
commit world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.commit timeline }

rollStat :: RandomGen r => RollId -> StatId -> World -> Rand r World
rollStat rid sid world@(World timeline events) = case Map.lookup sid stats of
    Just (Stat _ _ score) -> do
        -- TODO: Generate roll ID on the server and send it to just the client that initiated the
        -- roll.
        roll <- getRandomR (1, 6)
        return $ world { worldEvents = snoc events $ RollResult rid [roll, score] }
    Nothing -> return world
  where
    stats = sceneStats $ Timeline.value timeline

rollAspect :: RandomGen r => RollId -> AspectId -> World -> Rand r World
rollAspect rid aid world@(World timeline events) = case Map.lookup aid aspects of
    Just (Aspect { aspectDice = dice }) | dice >= 1 -> do
        roll <- getRandomR (1, 6)
        return world
            { worldTimeline = Timeline.modify timeline $ removeDie aid
            , worldEvents = updateEvents roll
            }
    _ -> return world
  where
    aspects = sceneAspects $ Timeline.value timeline
    updateEvents roll = case find requestedRoll events of
        Just _ -> map (amendRoll roll) events
        Nothing -> events
    amendRoll roll result@(RollResult rid' rolls)
        | rid == rid' = RollResult rid $ snoc rolls roll
        | otherwise   = result
    requestedRoll (RollResult rid' _)
        | rid == rid' = True
        | otherwise   = False

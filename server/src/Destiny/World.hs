{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.World
    ( World (..)
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
import Destiny.Message
import Destiny.Scene
import Destiny.Timeline (Timeline)
import Elm.Derive

import qualified Data.Map.Lazy as Map
import qualified Destiny.Timeline as Timeline

data World = World
    { worldTimeline :: Timeline Scene
    , worldMessages :: MessageList
    }

data WorldSnapshot = WorldSnapshot
    { snapshotScene :: Scene
    , snapshotMessages :: MessageList
    }

deriveJSON (defaultOptionsDropLower 5) ''World
deriveBoth (defaultOptionsDropLower 8) ''WorldSnapshot

emptyWorld :: World
emptyWorld = World
    { worldTimeline = Timeline.singleton emptyScene
    , worldMessages = emptyMessages
    }

worldSnapshot :: World -> WorldSnapshot
worldSnapshot world = WorldSnapshot
    { snapshotScene = Timeline.value (worldTimeline world)
    , snapshotMessages = worldMessages world
    }

undo :: World -> World
undo world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.undo timeline }

redo :: World -> World
redo world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.redo timeline }

commit :: World -> World
commit world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.commit timeline }

rollStat :: RandomGen r => StatId -> MessageId -> World -> Rand r World
rollStat sid mid world@(World timeline messages) = case Map.lookup sid stats of
    Just (Stat _ name score) -> do
        -- TODO: Generate roll ID on the server and send it to just the client that initiated the
        -- roll.
        result <- getRandomR (1, 6)
        let roll = Roll
                { rollId = mid
                , rollStatName = name
                , rollStatResult = result
                , rollStatModifier = score
                , rollInvokes = []
                }
        let messages' = messages
                { messageList = snoc (messageList messages) mid
                , messageMap = Map.insert mid (RollMessage roll) $ messageMap messages
                }
        return world { worldMessages = messages' }
    Nothing -> return world
  where
    stats = sceneStats $ Timeline.value timeline

rollAspect :: RandomGen r => AspectId -> MessageId -> World -> Rand r World
rollAspect aid mid world@(World timeline messages) = case Map.lookup aid aspects of
    Just Aspect { aspectText = text, aspectDice = dice } | dice >= 1 -> do
        result <- getRandomR (1, 6)
        let invoke = Invoke text result
        let messages' = messages
                { messageMap = Map.adjust (append invoke) mid $ messageMap messages
                }
        return world
            { worldTimeline = Timeline.modify timeline $ removeDie aid
            , worldMessages = messages'
            }
    _ -> return world
  where
    aspects = sceneAspects $ Timeline.value timeline
    append invoke (RollMessage roll) = RollMessage roll
        { rollInvokes = snoc (rollInvokes roll) invoke
        }

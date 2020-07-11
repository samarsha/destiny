{-# LANGUAGE DuplicateRecordFields #-}
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
import Data.List.Extra
import Destiny.Message
import Destiny.Scene
import Destiny.Timeline (Timeline)
import Elm.Derive

import qualified Data.Map.Lazy as Map
import qualified Destiny.Timeline as Timeline

data World = World
    { timeline :: Timeline Scene
    , messages :: MessageList
    }

data WorldSnapshot = WorldSnapshot
    { scene :: Scene
    , messages :: MessageList
    }

deriveJSON defaultOptions ''World
deriveBoth defaultOptions ''WorldSnapshot

emptyWorld :: World
emptyWorld = World
    { timeline = Timeline.singleton emptyScene
    , messages = emptyMessages
    }

worldSnapshot :: World -> WorldSnapshot
worldSnapshot world = WorldSnapshot
    { scene = Timeline.value $ timeline world
    , messages = messages (world :: World)
    }

undo :: World -> World
undo world = world { timeline = Timeline.undo $ timeline world }

redo :: World -> World
redo world = world { timeline = Timeline.redo $ timeline world }

commit :: World -> World
commit world = world { timeline = Timeline.commit $ timeline world }

rollStat :: RandomGen r => StatId -> MessageId -> World -> Rand r World
rollStat sid mid world = case Map.lookup sid stats' of
    Just (Stat _ name score) -> do
        -- TODO: Generate roll ID on the server and send it to just the client that initiated the
        -- roll.
        result' <- getRandomR (1, 6)
        let roll = Roll
                { id = mid
                , statName = name
                , statResult = result'
                , statModifier = score
                , invokes = []
                }
        let messages' = (messages (world :: World))
                { ids = snoc (ids $ messages (world :: World)) mid
                , messages = Map.insert mid (RollMessage roll) $
                    messages (messages (world :: World) :: MessageList)
                }
        return (world :: World) { messages = messages' }
    Nothing -> return world
  where
    stats' = stats $ Timeline.value $ timeline world

rollAspect :: RandomGen r => AspectId -> MessageId -> World -> Rand r World
rollAspect aid mid world = case Map.lookup aid aspects' of
    Just aspect | dice aspect >= 1 -> do
        result' <- getRandomR (1, 6)
        let invoke = Invoke (text aspect) result'
        let messages' = (messages (world :: World) :: MessageList)
                { messages = Map.adjust (append invoke) mid $
                    messages (messages (world :: World) :: MessageList)
                }
        return world
            { timeline = Timeline.modify (timeline world) $ removeDie aid
            , messages = messages'
            }
    _ -> return world
  where
    aspects' = aspects $ Timeline.value $ timeline world
    append invoke (RollMessage roll) = RollMessage roll
        { invokes = snoc (invokes roll) invoke
        }

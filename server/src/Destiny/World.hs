{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.World
    ( World (..)
    , WorldSnapshot
    , commit
    , emptyWorld
    , redo
    , rollAspect
    , rollStat
    , undo
    , worldSnapshot
    )
where

import Control.Lens
import Control.Monad.Random
import Data.Aeson.TH (deriveJSON)
import Data.Generics.Labels ()
import Destiny.Message
import Destiny.Scene
import Destiny.Timeline (Timeline)
import Elm.Derive
import GHC.Generics

import qualified Data.Map.Lazy as Map
import qualified Destiny.Timeline as Timeline

data World = World
    { timeline :: Timeline Scene
    , messages :: MessageList
    }
    deriving Generic
deriveJSON defaultOptions ''World

data WorldSnapshot = WorldSnapshot
    { scene :: Scene
    , messages :: MessageList
    }
    deriving Generic
deriveBoth defaultOptions ''WorldSnapshot

emptyWorld :: World
emptyWorld = World
    { timeline = Timeline.singleton emptyScene
    , messages = emptyMessages
    }

worldSnapshot :: World -> WorldSnapshot
worldSnapshot world = WorldSnapshot
    { scene = Timeline.value $ world ^. #timeline
    , messages = world ^. #messages
    }

undo :: World -> World
undo = over #timeline Timeline.undo

redo :: World -> World
redo = over #timeline Timeline.redo

commit :: World -> World
commit = over #timeline Timeline.commit

rollStat :: RandomGen r => StatId -> MessageId -> World -> Rand r World
rollStat statId messageId world = case Map.lookup statId stats' of
    Just stat -> do
        -- TODO: Generate roll ID on the server and send it to just the client that initiated the
        -- roll.
        result <- getRandomR (1, 6)
        let roll = Roll
                { id = messageId
                , statName = stat ^. #name
                , statResult = result
                , statModifier = stat ^. #score
                , invokes = []
                }
        return $ world & over #messages (\(MessageList ids msgs) -> MessageList
            (snoc ids messageId)
            (Map.insert messageId (RollMessage roll) msgs))
    Nothing -> return world
  where
    stats' = Timeline.value (world ^. #timeline) ^. #stats

rollAspect :: RandomGen r => AspectId -> MessageId -> World -> Rand r World
rollAspect aspectId messageId world = case Map.lookup aspectId aspects' of
    Just aspect | aspect ^. #dice >= 1 -> do
        result <- getRandomR (1, 6)
        let invoke = Invoke (aspect ^. #text) result
        return $ world
            & over #timeline (Timeline.modify $ removeDie aspectId)
            & over #messages (\(MessageList ids msgs) -> MessageList
                ids
                (Map.adjust (append invoke) messageId msgs))
    _ -> return world
  where
    aspects' = Timeline.value (world ^. #timeline) ^. #aspects
    append invoke (RollMessage roll) = RollMessage $ roll & over #invokes (flip snoc invoke)

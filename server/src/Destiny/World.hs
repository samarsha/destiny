{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.World
    ( World (..)
    , WorldSnapshot
    , commit
    , emptyWorld
    , redo
    , rollAspect
    , rollStat
    , scene
    , timeline
    , undo
    , worldSnapshot
    )
where

import Control.Lens
import Control.Monad.Random
import Data.Aeson.TH (deriveJSON)
import Destiny.Message
import Destiny.Scene
import Destiny.Timeline (Timeline)
import Elm.Derive

import qualified Data.Map.Lazy as Map
import qualified Destiny.Timeline as Timeline

data World = World
    { _timeline :: Timeline Scene
    , _messages :: MessageList
    }
deriveJSON (defaultOptionsDropLower 1) ''World
makeFieldsNoPrefix ''World

data WorldSnapshot = WorldSnapshot
    { _scene :: Scene
    , _messages :: MessageList
    }
deriveBoth (defaultOptionsDropLower 1) ''WorldSnapshot
makeFieldsNoPrefix ''WorldSnapshot

emptyWorld :: World
emptyWorld = World
    { _timeline = Timeline.singleton emptyScene
    , _messages = emptyMessages
    }

worldSnapshot :: World -> WorldSnapshot
worldSnapshot world = WorldSnapshot
    { _scene = Timeline.value $ world ^. timeline
    , _messages = world ^. messages
    }

undo :: World -> World
undo = timeline %~ Timeline.undo

redo :: World -> World
redo = timeline %~ Timeline.redo

commit :: World -> World
commit = timeline %~ Timeline.commit

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
        return $ world & messages %~ \case
            MessageList ids msgs -> MessageList
                (snoc ids mid)
                (Map.insert mid (RollMessage roll) msgs)
    Nothing -> return world
  where
    stats' = Timeline.value (world ^. timeline) ^. stats

rollAspect :: RandomGen r => AspectId -> MessageId -> World -> Rand r World
rollAspect aid mid world = case Map.lookup aid aspects' of
    Just aspect | aspect ^. dice >= 1 -> do
        result' <- getRandomR (1, 6)
        let invoke = Invoke (aspect ^. text) result'
        return $ world
            & timeline %~ Timeline.modify (removeDie aid)
            & messages %~ \case
                MessageList ids msgs -> MessageList ids $ Map.adjust (append invoke) mid msgs
    _ -> return world
  where
    aspects' = Timeline.value (world ^. timeline) ^. aspects
    append invoke (RollMessage roll) = RollMessage roll
        { invokes = snoc (invokes roll) invoke
        }

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    , scene
    , timeline
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
rollStat statId messageId world = case Map.lookup statId stats' of
    Just stat -> do
        -- TODO: Generate roll ID on the server and send it to just the client that initiated the
        -- roll.
        result <- getRandomR (1, 6)
        let roll = Roll
                { _id = messageId
                , _statName = stat ^. #name
                , _statResult = result
                , _statModifier = stat ^. #score
                , _invokes = []
                }
        return $ world & messages %~ \(MessageList ids msgs) -> MessageList
            (snoc ids messageId)
            (Map.insert messageId (RollMessage roll) msgs)
    Nothing -> return world
  where
    stats' = Timeline.value (world ^. timeline) ^. #stats

rollAspect :: RandomGen r => AspectId -> MessageId -> World -> Rand r World
rollAspect aspectId messageId world = case Map.lookup aspectId aspects' of
    Just aspect | aspect ^. #dice >= 1 -> do
        result <- getRandomR (1, 6)
        let invoke = Invoke (aspect ^. #text) result
        return $ world
            & timeline %~ Timeline.modify (removeDie aspectId)
            & messages %~ \(MessageList ids msgs) -> MessageList
                ids
                (Map.adjust (append invoke) messageId msgs)
    _ -> return world
  where
    aspects' = Timeline.value (world ^. timeline) ^. #aspects
    append invoke (RollMessage roll) = RollMessage $ roll & invokes %~ flip snoc invoke

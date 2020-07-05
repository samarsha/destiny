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
    , worldSnapshot
    )
where

import Control.Monad.Random
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types hiding (defaultOptions)
import Data.UUID
import Destiny.Scene
import Destiny.Timeline (Timeline)
import Elm.Derive

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

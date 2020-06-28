{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.Timeline (Timeline, commit, modify, redo, singleton, undo, update, value) where

import Data.Aeson.TH
import Destiny.Utils

data Timeline a = Timeline
    { timelinePast :: [a]
    , timelinePresent :: a
    , timelineFuture :: [a]
    , timelineLength :: Int
    , timelineCommitted :: Bool
    }

deriveJSON (stripFieldPrefixOptions "timeline") ''Timeline

singleton :: a -> Timeline a
singleton x = Timeline
    { timelinePast = []
    , timelinePresent = x
    , timelineFuture = []
    , timelineLength = 1
    , timelineCommitted = True
    }

value :: Timeline a -> a
value = timelinePresent

undo :: Timeline a -> Timeline a
undo timeline@Timeline { timelinePast = present' : past'
                       , timelinePresent = present
                       , timelineFuture = future
                       } = timeline
    { timelinePast = past'
    , timelinePresent = present'
    , timelineFuture = present : future
    , timelineCommitted = True
    }
undo timeline = timeline

redo :: Timeline a -> Timeline a
redo timeline@Timeline { timelinePast = past
                       , timelinePresent = present
                       , timelineFuture = present' : future'
                       } = timeline
    { timelinePast = present : past
    , timelinePresent = present'
    , timelineFuture = future'
    , timelineCommitted = True
    }
redo timeline = timeline

commit :: Timeline a -> Timeline a
commit timeline = timeline { timelineCommitted = True }

update :: a -> Timeline a -> Timeline a
update x timeline@Timeline { timelinePast = past
                           , timelinePresent = present
                           , timelineFuture = future
                           , timelineLength = len
                           , timelineCommitted = committed
                           }
    | committed && len == maxLen = timeline' { timelinePast = present : init past }
    | committed = timeline'
        { timelinePast = present : past
        , timelineLength = timelineLength timeline' + 1
        }
    | otherwise = timeline'
  where
    timeline' = timeline
        { timelinePresent = x
        , timelineFuture = []
        , timelineLength = len - length future
        , timelineCommitted = False
        }
    maxLen = 100

modify :: Timeline a -> (a -> a) -> Timeline a
modify timeline@Timeline { timelinePresent = present } f = update (f $ present) timeline
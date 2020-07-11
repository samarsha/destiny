{-# LANGUAGE TemplateHaskell #-}

module Destiny.Timeline (Timeline (present), commit, modify, redo, singleton, undo, update) where

import Data.Aeson.TH (deriveJSON)
import Elm.Derive

data Timeline a = Timeline
    { past :: [a]
    , present :: a
    , future :: [a]
    , committed :: Bool
    }
deriveJSON defaultOptions ''Timeline

instance Foldable Timeline where
    foldr f z timeline = foldr f z $
        reverse (future timeline) ++ present timeline : past timeline

singleton :: a -> Timeline a
singleton x = Timeline
    { past = []
    , present = x
    , future = []
    , committed = True
    }

undo :: Timeline a -> Timeline a
undo timeline@Timeline { past = present' : past' } = timeline
    { past = past'
    , present = present'
    , future = present timeline : future timeline
    , committed = True
    }
undo timeline = timeline

redo :: Timeline a -> Timeline a
redo timeline@Timeline { future = present' : future' } = timeline
    { past = present timeline : past timeline
    , present = present'
    , future = future'
    , committed = True
    }
redo timeline = timeline

commit :: Timeline a -> Timeline a
commit timeline = timeline { committed = True }

update :: a -> Timeline a -> Timeline a
update x timeline
    | committed timeline && length timeline == maxLength =
        timeline' { past = present timeline : init (past timeline) }
    | committed timeline = timeline' { past = present timeline : past timeline }
    | otherwise = timeline'
  where
    timeline' = timeline
        { present = x
        , future = []
        , committed = False
        }
    maxLength = 50

modify :: (a -> a) -> Timeline a -> Timeline a
modify f timeline = update (f $ present timeline) timeline

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.Message
    ( Invoke (..)
    , Message (..)
    , MessageId
    , MessageList (..)
    , Roll (..)
    , emptyMessages
    )
where

import Control.Monad.Random
import Data.Aeson.Types hiding (defaultOptions)
import Data.Map.Lazy (Map)
import Data.UUID
import Elm.Derive

import qualified Data.Map.Lazy as Map

data MessageList = MessageList
    { messageList :: [MessageId]
    , messageMap :: Map MessageId Message
    }

newtype MessageId = MessageId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

data Message = RollMessage Roll

data Roll = Roll
    { rollId :: MessageId
    , rollStatName :: String
    , rollStatResult :: Int
    , rollStatModifier :: Int
    , rollInvokes :: [Invoke]
    }

data Invoke = Invoke
    { invokeSource :: String
    , invokeResult :: Int
    }

deriveBoth defaultOptions ''MessageId

deriveBoth (defaultOptionsDropLower 6) ''Invoke
deriveBoth (defaultOptionsDropLower 4) ''Roll
deriveBoth defaultOptions ''Message
deriveBoth (defaultOptionsDropLower 7) ''MessageList

emptyMessages :: MessageList
emptyMessages = MessageList
    { messageList = []
    , messageMap = Map.empty
    }

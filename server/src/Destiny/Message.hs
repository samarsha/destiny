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
    { ids :: [MessageId]
    , messages :: Map MessageId Message
    }

newtype MessageId = MessageId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

data Message = RollMessage Roll

data Roll = Roll
    { id :: MessageId
    , statName :: String
    , statResult :: Int
    , statModifier :: Int
    , invokes :: [Invoke]
    }

data Invoke = Invoke
    { source :: String
    , result :: Int
    }

deriveBoth defaultOptions ''MessageId

deriveBoth defaultOptions ''Invoke
deriveBoth defaultOptions ''Roll
deriveBoth defaultOptions ''Message
deriveBoth defaultOptions ''MessageList

emptyMessages :: MessageList
emptyMessages = MessageList { ids = [], messages = Map.empty }

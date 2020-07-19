{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Destiny.Scene
import Elm.Derive
import GHC.Generics

import qualified Data.Map.Lazy as Map

newtype MessageId = MessageId UUID
    deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)
deriveBoth defaultOptions ''MessageId

data Invoke = Invoke
    { source :: String
    , role :: Role
    , result :: Int
    }
deriveBoth defaultOptions ''Invoke

data Roll = Roll
    { id :: MessageId
    , role :: Role
    , statName :: String
    , statResult :: Int
    , statModifier :: Int
    , invokes :: [Invoke]
    }
    deriving Generic
deriveBoth defaultOptions ''Roll

data Message = RollMessage Roll
deriveBoth defaultOptions ''Message

data MessageList = MessageList [MessageId] (Map MessageId Message)
deriveBoth defaultOptions ''MessageList

emptyMessages :: MessageList
emptyMessages = MessageList [] Map.empty

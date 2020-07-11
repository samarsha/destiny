{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.Message
    ( Destiny.Message.id
    , Invoke (..)
    , Message (..)
    , MessageId
    , MessageList (..)
    , Roll (..)
    , emptyMessages
    , invokes
    , statModifier
    , statName
    , statResult
    )
where

import Control.Lens
import Control.Monad.Random
import Data.Aeson.Types hiding (defaultOptions)
import Data.Map.Lazy (Map)
import Data.UUID
import Elm.Derive

import qualified Data.Map.Lazy as Map

newtype MessageId = MessageId UUID
    deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)
deriveBoth defaultOptions ''MessageId

data Invoke = Invoke String Int
deriveBoth defaultOptions ''Invoke

data Roll = Roll
    { _id :: MessageId
    , _statName :: String
    , _statResult :: Int
    , _statModifier :: Int
    , _invokes :: [Invoke]
    }
deriveBoth (defaultOptionsDropLower 1) ''Roll
makeFieldsNoPrefix ''Roll

data Message = RollMessage Roll
deriveBoth defaultOptions ''Message

data MessageList = MessageList [MessageId] (Map MessageId Message)
deriveBoth defaultOptions ''MessageList

emptyMessages :: MessageList
emptyMessages = MessageList [] Map.empty

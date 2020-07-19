{-# OPTIONS_GHC -fno-warn-orphans #-}

module Destiny.MultiSet () where

import Data.Aeson
import Data.MultiSet (MultiSet)

import qualified Data.MultiSet as MultiSet

instance ToJSONKey a => ToJSON (MultiSet a) where
    toJSON = toJSON . MultiSet.toMap

instance (Ord a, FromJSONKey a) => FromJSON (MultiSet a) where
    parseJSON value = MultiSet.fromMap <$> parseJSON value

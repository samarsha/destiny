module Destiny.Utils where

import Data.Aeson.Types
import Data.Char
import Data.List

stripFieldPrefixOptions :: String -> Options
stripFieldPrefixOptions prefix = defaultOptions { fieldLabelModifier = strip }
  where
    strip field = maybe field lowerFirst (stripPrefix prefix field)
    lowerFirst "" = ""
    lowerFirst (x : xs) = toLower x : xs

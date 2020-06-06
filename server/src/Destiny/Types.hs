{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.Types
    ( Aspect (..)
    , ClientRequest (..)
    , Entity (..)
    , World (..)
    , Id
    )
where

import Elm.Derive

-- | The world.
data World = World
    { -- | The entities.
      worldEntities :: [Entity]
      -- | The result of the last dice roll.
    , worldLastRoll :: Int
    }

-- | An entity.
data Entity = Entity
    { -- | The entity ID.
      entityId :: Id
      -- | The aspects that belong to the entity.
    , entityAspects :: [Aspect]
      -- | True if the entity is collapsed.
    , entityCollapsed :: Bool
    }

-- | An aspect.
data Aspect = Aspect
    { -- | The aspect ID.
      aspectId :: Id
      -- | The description of the aspect.
    , aspectText :: String
      -- | A list of the selected status for each free invoke die.
    , aspectDice :: [Bool]
    }

-- | An opaque identifier.
newtype Id = Id Int
    deriving (Bounded, Enum, Eq, Ord)

data ClientRequest
    = AddEntity
    | ToggleEntity Entity
    | RemoveEntity Entity
    | AddAspect Entity
    | EditAspect Aspect
    | RemoveAspect Aspect
    | AddDie Aspect
    | ToggleDie Aspect Int Bool
    | RemoveDie Aspect
    | Roll Aspect

deriveBoth defaultOptions ''Aspect
deriveBoth defaultOptions ''ClientRequest
deriveBoth defaultOptions ''Entity
deriveBoth defaultOptions ''Id
deriveBoth defaultOptions ''World

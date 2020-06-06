{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.Types
    ( Id
    , World (..)
    , Entity (..)
    , Aspect (..)
    , Request (..)
    )
where

import Elm.Derive

-- | An opaque identifier.
newtype Id = Id Int
    deriving (Bounded, Enum, Eq, Ord)

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

data Request
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

deriveBoth defaultOptions ''World
deriveBoth defaultOptions ''Entity
deriveBoth defaultOptions ''Aspect
deriveBoth defaultOptions ''Id
deriveBoth defaultOptions ''Request

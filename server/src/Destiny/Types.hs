{-# LANGUAGE TemplateHaskell #-}

module Destiny.Types
    ( World (..)
    , Entity (..)
    , Aspect (..)
    , Request (..)
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
      entityId :: EntityId
      -- | The aspects that belong to the entity.
    , entityAspects :: [Aspect]
      -- | True if the entity is collapsed.
    , entityCollapsed :: Bool
    }

-- | An entity ID.
type EntityId = Int

-- | An aspect.
data Aspect = Aspect
    { -- | The aspect ID.
      aspectId :: AspectId
      -- | The description of the aspect.
    , aspectText :: String
      -- | A list of the selected status for each free invoke die.
    , aspectDice :: [Bool]
    }

-- | An aspect ID.
type AspectId = Int

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
deriveBoth defaultOptions ''Request

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.Model
    ( Aspect (..)
    , ClientRequest (..)
    , Entity (..)
    , World (..)
    , Id
    , updateWorld
    )
where

import Destiny.Utils
import Elm.Derive
import System.Random

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
    deriving (Bounded, Enum, Eq, Ord, Random)

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

updateWorld :: World -> ClientRequest -> IO World
updateWorld world = \case
    AddEntity -> do
        eid <- randomRIO (Id 0, Id 1000000)
        let entity = Entity { entityId = eid, entityAspects = [], entityCollapsed = False }
        return world { worldEntities = entity : worldEntities world }
    ToggleEntity _ -> putStrLn "ToggleEntity" >> return world
    RemoveEntity _ -> putStrLn "RemoveEntity" >> return world
    AddAspect _ -> putStrLn "AddAspect" >> return world
    EditAspect _ -> putStrLn "EditAspect" >> return world
    RemoveAspect _ -> putStrLn "RemoveAspect" >> return world
    AddDie _ -> putStrLn "AddDie" >> return world
    ToggleDie _ _ _ -> putStrLn "ToggleDie" >> return world
    RemoveDie _ -> putStrLn "RemoveDie" >> return world
    Roll _ -> putStrLn "Roll" >> return world

deriveBoth (stripFieldPrefixOptions "aspect") ''Aspect
deriveBoth (stripFieldPrefixOptions "entity") ''Entity
deriveBoth (stripFieldPrefixOptions "world") ''World
deriveBoth defaultOptions ''ClientRequest
deriveBoth defaultOptions ''Id

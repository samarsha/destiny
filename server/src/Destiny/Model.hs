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
    AddEntity -> addEntity world
    ToggleEntity entity -> return $ toggleEntity world entity
    RemoveEntity entity -> return $ removeEntity world entity
    AddAspect entity -> addAspect world entity
    EditAspect aspect -> return $ updateAspect world aspect
    RemoveAspect _ -> putStrLn "RemoveAspect" >> return world
    AddDie _ -> putStrLn "AddDie" >> return world
    ToggleDie _ _ _ -> putStrLn "ToggleDie" >> return world
    RemoveDie _ -> putStrLn "RemoveDie" >> return world
    Roll _ -> putStrLn "Roll" >> return world

addEntity :: World -> IO World
addEntity world = do
    eid <- randomRIO (Id 0, Id 1000000)
    let entity = Entity { entityId = eid, entityAspects = [], entityCollapsed = False }
    return world { worldEntities = entity : worldEntities world }

updateEntity :: World -> Entity -> World
updateEntity world entity = world { worldEntities = map replaceEntity $ worldEntities world }
  where
    replaceEntity entity'
        | entityId entity' == entityId entity = entity
        | otherwise = entity'

toggleEntity :: World -> Entity -> World
toggleEntity world entity =
    updateEntity world entity { entityCollapsed = not (entityCollapsed entity) }

removeEntity :: World -> Entity -> World
removeEntity world entity =
    world { worldEntities = filter ((/=) (entityId entity) . entityId) $ worldEntities world }

addAspect :: World -> Entity -> IO World
addAspect world entity = do
    aid <- randomRIO (Id 0, Id 1000000)
    let aspect = Aspect { aspectId = aid, aspectText = "", aspectDice = [] }
    return $ updateEntity world entity { entityAspects = aspect : entityAspects entity }

updateAspect :: World -> Aspect -> World
updateAspect world aspect = world { worldEntities = map updateEntityAspects $ worldEntities world }
  where
    updateEntityAspects entity = entity { entityAspects = map replaceAspect $ entityAspects entity }
    replaceAspect aspect'
        | aspectId aspect' == aspectId aspect = aspect
        | otherwise = aspect'

deriveBoth (stripFieldPrefixOptions "aspect") ''Aspect
deriveBoth (stripFieldPrefixOptions "entity") ''Entity
deriveBoth (stripFieldPrefixOptions "world") ''World
deriveBoth defaultOptions ''ClientRequest
deriveBoth defaultOptions ''Id

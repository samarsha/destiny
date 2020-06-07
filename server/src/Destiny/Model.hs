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

updateWorld :: ClientRequest -> World -> IO World
updateWorld = \case
    AddEntity -> addEntity
    ToggleEntity entity -> return . toggleEntity entity
    RemoveEntity entity -> return . removeEntity entity
    AddAspect entity -> addAspect entity
    EditAspect aspect -> return . updateAspect aspect
    RemoveAspect _ -> \world -> putStrLn "RemoveAspect" >> return world
    AddDie _ -> \world -> putStrLn "AddDie" >> return world
    ToggleDie _ _ _ -> \world -> putStrLn "ToggleDie" >> return world
    RemoveDie _ -> \world -> putStrLn "RemoveDie" >> return world
    Roll _ -> \world -> putStrLn "Roll" >> return world

addEntity :: World -> IO World
addEntity world = do
    eid <- randomRIO (Id 0, Id 1000000)
    let entity = Entity { entityId = eid, entityAspects = [], entityCollapsed = False }
    return world { worldEntities = entity : worldEntities world }

updateEntity :: Entity -> World -> World
updateEntity entity world = world { worldEntities = map replaceEntity $ worldEntities world }
  where
    replaceEntity entity'
        | entityId entity' == entityId entity = entity
        | otherwise = entity'

toggleEntity :: Entity -> World -> World
toggleEntity entity world =
    updateEntity entity { entityCollapsed = not (entityCollapsed entity) } world

removeEntity :: Entity -> World -> World
removeEntity entity world =
    world { worldEntities = filter ((/=) (entityId entity) . entityId) $ worldEntities world }

addAspect :: Entity -> World -> IO World
addAspect entity world = do
    aid <- randomRIO (Id 0, Id 1000000)
    let aspect = Aspect { aspectId = aid, aspectText = "", aspectDice = [] }
    return $ updateEntity entity { entityAspects = aspect : entityAspects entity } world

updateAspect :: Aspect -> World -> World
updateAspect aspect world = world { worldEntities = map updateEntityAspects $ worldEntities world }
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

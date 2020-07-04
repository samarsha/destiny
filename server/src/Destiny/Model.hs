{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Destiny.Model
    ( Aspect
    , AspectId
    , ClientRequest
    , ClientResponse (..)
    , Entity
    , EntityId
    , Event
    , RollId
    , Scene
    , Stat
    , StatGroup
    , StatGroupId
    , StatId
    , World
    , WorldSnapshot
    , commit
    , emptyWorld
    , updateWorld
    , worldSnapshot
    )
where

import Control.Monad.Random
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types hiding (defaultOptions)
import Data.Functor
import Data.List
import Data.List.Extra
import Data.Map.Lazy (Map)
import Data.UUID
import Destiny.Timeline (Timeline)
import Elm.Derive

import qualified Data.Map.Lazy as Map
import qualified Destiny.Timeline as Timeline

-- | The world.
data World = World
    { -- | The timeline of scenes.
      worldTimeline :: Timeline Scene
      -- | The events that have occurred.
    , worldEvents :: [Event]
    }

-- | A snapshot of the world.
data WorldSnapshot = WorldSnapshot
    { -- | The scene.
      snapshotScene :: Scene
      -- | The events that have occurred.
    , snapshotEvents :: [Event]
    }

data Scene = Scene
    { sceneBoard :: [EntityId]
    , sceneEntities :: Map EntityId Entity
    , sceneStatGroups :: Map StatGroupId StatGroup
    , sceneStats :: Map StatId Stat
    , sceneAspects :: Map AspectId Aspect
    }

newtype EntityId = EntityId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

newtype StatGroupId = StatGroupId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

newtype StatId = StatId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

newtype AspectId = AspectId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

newtype RollId = RollId UUID deriving (Eq, Ord, Random, FromJSONKey, ToJSONKey)

-- | An entity.
data Entity = Entity
    { -- | The entity ID.
      entityId :: EntityId
      -- | The entity name.
    , entityName :: String
      -- | The entity stat groups.
    , entityStatGroups :: [StatGroupId]
      -- | The aspects that belong to the entity.
    , entityAspects :: [AspectId]
      -- | True if the entity is collapsed.
    , entityCollapsed :: Bool
    }

data StatGroup = StatGroup StatGroupId String [StatId]

data Stat = Stat StatId String Int

-- | An aspect.
data Aspect = Aspect
    { -- | The aspect ID.
      aspectId :: AspectId
      -- | The description of the aspect.
    , aspectText :: String
      -- | The number of free invoke dice for the aspect.
    , aspectDice :: Int
    }

data Event = RollResult RollId [Int]

data ClientRequest
    = AddEntity
    | ToggleEntity EntityId
    | SetEntityName EntityId String
    | MoveEntity EntityId Int
    | RemoveEntity EntityId
    | AddStatGroup EntityId
    | SetStatGroupName StatGroupId String
    | RemoveStatGroup StatGroupId
    | AddStat StatGroupId
    | SetStatName StatId String
    | SetStatScore StatId Int
    | RemoveStat StatId
    | AddAspect EntityId
    | SetAspectText AspectId String
    | RemoveAspect AspectId
    | AddDie AspectId
    | RemoveDie AspectId
    | RollStat StatId RollId
    | RollAspect AspectId RollId
    | Undo
    | Redo

data ClientResponse
    = UpdateWorld
    | NoResponse

deriveBoth defaultOptions ''AspectId
deriveBoth defaultOptions ''EntityId
deriveBoth defaultOptions ''RollId
deriveBoth defaultOptions ''StatGroupId
deriveBoth defaultOptions ''StatId
deriveBoth (defaultOptionsDropLower 6) ''Aspect
deriveBoth (defaultOptionsDropLower 6) ''Entity
deriveBoth (defaultOptionsDropLower 5) ''Scene
deriveBoth (defaultOptionsDropLower 8) ''WorldSnapshot
deriveBoth defaultOptions ''ClientRequest
deriveBoth defaultOptions ''Event
deriveBoth defaultOptions ''Stat
deriveBoth defaultOptions ''StatGroup
deriveJSON (defaultOptionsDropLower 5) ''World

emptyWorld :: World
emptyWorld = World
    { worldTimeline = Timeline.singleton emptyScene
    , worldEvents = []
    }

emptyScene :: Scene
emptyScene = Scene
    { sceneBoard = []
    , sceneEntities = Map.empty
    , sceneStatGroups = Map.empty
    , sceneStats = Map.empty
    , sceneAspects = Map.empty
    }

worldSnapshot :: World -> WorldSnapshot
worldSnapshot World { worldTimeline = timeline, worldEvents = events } = WorldSnapshot
    { snapshotScene = Timeline.value timeline
    , snapshotEvents = events
    }

updateWorld :: RandomGen r => ClientRequest -> World -> Rand r (World, ClientResponse)
updateWorld request world = case request of
    AddEntity ->
        addEntity world <&> (, UpdateWorld)
    ToggleEntity eid ->
        return (toggleEntity eid world, UpdateWorld)
    SetEntityName eid name ->
        return (setEntityName name eid world, UpdateWorld)
    MoveEntity eid index ->
        return (moveEntity index eid world, UpdateWorld)
    RemoveEntity eid ->
        return (removeEntity eid world, UpdateWorld)
    AddStatGroup eid ->
        addStatGroup eid world <&> (, UpdateWorld)
    SetStatGroupName sgid name ->
        return (setStatGroupName name sgid world, UpdateWorld)
    RemoveStatGroup sgid ->
        return (removeStatGroup sgid world, UpdateWorld)
    AddStat sgid ->
        addStat sgid world <&> (, UpdateWorld)
    SetStatName sid name ->
        return (setStatName name sid world, UpdateWorld)
    SetStatScore sid score ->
        return (setStatScore score sid world, UpdateWorld)
    RemoveStat sid ->
        return (removeStat sid world, UpdateWorld)
    AddAspect eid ->
        addAspect eid world <&> (, UpdateWorld)
    SetAspectText aid text ->
        return (setAspectText text aid world, NoResponse)
    RemoveAspect aid ->
        return (removeAspect aid world, UpdateWorld)
    AddDie aid ->
        return (addDie aid world, UpdateWorld)
    RemoveDie aid ->
        return (removeDie aid world, UpdateWorld)
    RollStat sid rid ->
        rollStat rid sid world <&> (, UpdateWorld)
    RollAspect aid rid ->
        rollAspect rid aid world <&> (, UpdateWorld)
    Undo ->
        return (undo world, UpdateWorld)
    Redo ->
        return (redo world, UpdateWorld)

addEntity :: RandomGen r => World -> Rand r World
addEntity world@World { worldTimeline = timeline } = do
    newId <- getRandom
    let entity = Entity
            { entityId = newId
            , entityName = ""
            , entityStatGroups = []
            , entityAspects = []
            , entityCollapsed = False
            }
    return world
        { worldTimeline = Timeline.modify timeline $ \scene -> scene
            { sceneEntities = Map.insert newId entity $ sceneEntities scene
            , sceneBoard = snoc (sceneBoard scene) newId
            }
        }

modifyScene :: (Scene -> Scene) -> World -> World
modifyScene f world = world { worldTimeline = Timeline.modify (worldTimeline world) f }

modifyEntity :: (Entity -> Entity) -> EntityId -> World -> World
modifyEntity f eid = modifyScene $ \scene ->
    scene { sceneEntities = Map.adjust f eid $ sceneEntities scene }

toggleEntity :: EntityId -> World -> World
toggleEntity = modifyEntity $ \entity -> entity { entityCollapsed = not $ entityCollapsed entity }

setEntityName :: String -> EntityId -> World -> World
setEntityName name = modifyEntity $ \entity -> entity { entityName = name }

moveEntity :: Int -> EntityId -> World -> World
moveEntity index eid = modifyScene $ \scene ->
    let removed = delete eid $ sceneBoard scene
    in  scene { sceneBoard = take index removed ++ eid : drop index removed }

removeEntity :: EntityId -> World -> World
removeEntity eid = modifyScene $ \scene -> scene
    { sceneBoard = delete eid $ sceneBoard scene
    , sceneEntities = Map.delete eid $ sceneEntities scene
    }

addStatGroup :: RandomGen r => EntityId -> World -> Rand r World
addStatGroup eid world = do
    newId <- getRandom
    let statGroup = StatGroup newId "" []
    return $ modifyScene (addToScene statGroup) world
  where
    addToScene statGroup@(StatGroup sgid _ _) scene = scene
        { sceneEntities = Map.adjust (addToEntity sgid) eid $ sceneEntities scene
        , sceneStatGroups = Map.insert sgid statGroup $ sceneStatGroups scene
        }
    addToEntity sgid entity@Entity { entityStatGroups = statGroups } =
        entity { entityStatGroups = snoc statGroups sgid }

setStatGroupName :: String -> StatGroupId -> World -> World
setStatGroupName name sgid = modifyScene $ \scene ->
    scene { sceneStatGroups = Map.adjust adjust sgid $ sceneStatGroups scene }
  where
    adjust (StatGroup sgid' _ stats) = StatGroup sgid' name stats

removeStatGroup :: StatGroupId -> World -> World
removeStatGroup sgid = modifyScene $ \scene -> scene
    { sceneEntities = Map.map updateEntity $ sceneEntities scene
    , sceneStatGroups = Map.delete sgid $ sceneStatGroups scene
    }
  where
    updateEntity entity = entity { entityStatGroups = delete sgid $ entityStatGroups entity }

addStat :: RandomGen r => StatGroupId -> World -> Rand r World
addStat sgid world = do
    newId <- getRandom
    let stat = Stat newId "" 0
    return $ modifyScene (addToScene stat) world
  where
    addToScene stat@(Stat sid _ _) scene = scene
        { sceneStatGroups = Map.adjust (addToGroup sid) sgid $ sceneStatGroups scene
        , sceneStats = Map.insert sid stat $ sceneStats scene
        }
    addToGroup sid (StatGroup sgid' name stats) = StatGroup sgid' name $ snoc stats sid

modifyStat :: (Stat -> Stat) -> StatId -> World -> World
modifyStat f sid = modifyScene $ \scene ->
    scene { sceneStats = Map.adjust f sid $ sceneStats scene }

setStatName :: String -> StatId -> World -> World
setStatName name = modifyStat $ \(Stat sid' _ score) -> Stat sid' name score

setStatScore :: Int -> StatId -> World -> World
setStatScore score = modifyStat $ \(Stat sid' name _) -> Stat sid' name score

removeStat :: StatId -> World -> World
removeStat sid = modifyScene $ \scene -> scene
    { sceneStatGroups = Map.map updateGroup $ sceneStatGroups scene
    , sceneStats = Map.delete sid $ sceneStats scene
    }
  where
    updateGroup (StatGroup sgid name stats) = StatGroup sgid name $ delete sid stats

addAspect :: RandomGen r => EntityId -> World -> Rand r World
addAspect eid world = do
    newId <- getRandom
    let aspect = Aspect { aspectId = newId, aspectText = "", aspectDice = 0 }
    return $ modifyScene (addToScene aspect) world
  where
    addToScene aspect@Aspect { aspectId = aid } scene = scene
        { sceneEntities = Map.adjust (addToEntity aid) eid $ sceneEntities scene
        , sceneAspects = Map.insert aid aspect $ sceneAspects scene
        }
    addToEntity aid entity@Entity { entityAspects = aspects } =
        entity { entityAspects = snoc aspects aid }

modifyAspect :: (Aspect -> Aspect) -> AspectId -> World -> World
modifyAspect f aid = modifyScene $ \scene ->
    scene { sceneAspects = Map.adjust f aid $ sceneAspects scene }

setAspectText :: String -> AspectId -> World -> World
setAspectText text = modifyAspect $ \aspect -> aspect { aspectText = text }

removeAspect :: AspectId -> World -> World
removeAspect aid = modifyScene $ \scene -> scene
    { sceneEntities = Map.map updateEntity $ sceneEntities scene
    , sceneAspects = Map.delete aid $ sceneAspects scene
    }
  where
    updateEntity entity = entity { entityAspects = delete aid $ entityAspects entity }

addDie :: AspectId -> World -> World
addDie = modifyAspect $ \aspect -> aspect { aspectDice = aspectDice aspect + 1 }

removeDie :: AspectId -> World -> World
removeDie = modifyAspect $ \aspect@Aspect { aspectDice = dice } ->
    if dice >= 1
        then aspect { aspectDice = dice - 1 }
        else aspect

rollStat :: RandomGen r => RollId -> StatId -> World -> Rand r World
rollStat rid sid world@(World timeline events) = case Map.lookup sid stats of
    Just (Stat _ _ score) -> do
        -- TODO: Generate roll ID on the server and send it to just the client that initiated the
        -- roll.
        roll <- getRandomR (1, 6)
        return $ world { worldEvents = snoc events $ RollResult rid [roll, score] }
    Nothing -> return world
  where
    stats = sceneStats $ Timeline.value timeline

rollAspect :: RandomGen r => RollId -> AspectId -> World -> Rand r World
rollAspect rid aid world@(World timeline events) = case Map.lookup aid aspects of
    Just (Aspect { aspectDice = dice }) | dice >= 1 -> do
        roll <- getRandomR (1, 6)
        let world' = world { worldEvents = updateEvents roll }
        return $ modifyAspect (\aspect -> aspect { aspectDice = dice - 1 }) aid world'
    _ -> return world
  where
    aspects = sceneAspects $ Timeline.value timeline
    updateEvents roll = case find requestedRoll events of
        Just _ -> map (amendRoll roll) events
        Nothing -> events
    amendRoll roll result@(RollResult rid' rolls)
        | rid == rid' = RollResult rid $ snoc rolls roll
        | otherwise   = result
    requestedRoll (RollResult rid' _)
        | rid == rid' = True
        | otherwise   = False

undo :: World -> World
undo world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.undo timeline }

redo :: World -> World
redo world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.redo timeline }

commit :: World -> World
commit world@World { worldTimeline = timeline } = world { worldTimeline = Timeline.commit timeline }

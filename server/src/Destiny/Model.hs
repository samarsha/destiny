{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Destiny.Model
    ( Aspect
    , AspectId
    , ClientRequest
    , Entity
    , EntityId
    , Event
    , RollId
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
import Data.List
import Data.List.Extra
import Data.Maybe
import Data.UUID
import Destiny.Timeline (Timeline)
import Elm.Derive

import qualified Destiny.Timeline as Timeline

-- | The world.
data World = World
    { -- | The timeline of entities.
      worldTimeline :: Timeline [Entity]
      -- | The events that have occurred.
    , worldEvents :: [Event]
    }

-- | A snapshot of the world.
data WorldSnapshot = WorldSnapshot
    { -- | The entities.
      snapshotEntities :: [Entity]
      -- | The events that have occurred.
    , snapshotEvents :: [Event]
    }

-- | An entity.
data Entity = Entity
    { -- | The entity ID.
      entityId :: EntityId
      -- | The entity name.
    , entityName :: String
      -- | The entity stat groups.
    , entityStatGroups :: [StatGroup]
      -- | The aspects that belong to the entity.
    , entityAspects :: [Aspect]
      -- | True if the entity is collapsed.
    , entityCollapsed :: Bool
    }

-- | An entity ID.
newtype EntityId = EntityId UUID
    deriving (Eq, Random)

data StatGroup = StatGroup StatGroupId String [Stat]

newtype StatGroupId = StatGroupId UUID
    deriving (Eq, Random)

data Stat = Stat StatId String Int

newtype StatId = StatId UUID
    deriving (Eq, Random)

-- | An aspect.
data Aspect = Aspect
    { -- | The aspect ID.
      aspectId :: AspectId
      -- | The description of the aspect.
    , aspectText :: String
      -- | The number of free invoke dice for the aspect.
    , aspectDice :: Int
    }

-- | An aspect ID.
newtype AspectId = AspectId UUID
    deriving (Eq, Random)

data Event = RollResult RollId [Int]

newtype RollId = RollId UUID
    deriving (Eq, Random)

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

deriveBoth defaultOptions ''RollId
deriveBoth defaultOptions ''Event
deriveJSON (defaultOptionsDropLower 5) ''World
deriveBoth (defaultOptionsDropLower 8) ''WorldSnapshot
deriveBoth defaultOptions ''EntityId
deriveBoth defaultOptions ''StatId
deriveBoth defaultOptions ''Stat
deriveBoth defaultOptions ''StatGroupId
deriveBoth defaultOptions ''StatGroup
deriveBoth (defaultOptionsDropLower 6) ''Entity
deriveBoth defaultOptions ''AspectId
deriveBoth (defaultOptionsDropLower 6) ''Aspect
deriveBoth defaultOptions ''ClientRequest

emptyWorld :: World
emptyWorld = World
    { worldTimeline = Timeline.singleton []
    , worldEvents = []
    }

worldSnapshot :: World -> WorldSnapshot
worldSnapshot World { worldTimeline = timeline, worldEvents = events } = WorldSnapshot
    { snapshotEntities = Timeline.value timeline
    , snapshotEvents = events
    }

updateWorld :: RandomGen g => ClientRequest -> World -> Rand g World
updateWorld = \case
    AddEntity -> addEntity
    ToggleEntity eid -> return . toggleEntity eid
    SetEntityName eid name -> return . setEntityName name eid
    MoveEntity eid index -> return . moveEntity index eid
    RemoveEntity eid -> return . removeEntity eid
    AddStatGroup eid -> addStatGroup eid
    SetStatGroupName sgid name -> return . setStatGroupName name sgid
    RemoveStatGroup sgid -> return . removeStatGroup sgid
    AddStat sgid -> addStat sgid
    SetStatName sid name -> return . setStatName name sid
    SetStatScore sid score -> return . setStatScore score sid
    RemoveStat sid -> return . removeStat sid
    AddAspect eid -> addAspect eid
    SetAspectText aid text -> return . setAspectText text aid
    RemoveAspect aid -> return . removeAspect aid
    AddDie aid -> return . addDie aid
    RemoveDie aid -> return . removeDie aid
    RollStat sid rid -> rollStat rid sid
    RollAspect aid rid -> rollAspect rid aid
    Undo -> return . undo
    Redo -> return . redo

addEntity :: RandomGen g => World -> Rand g World
addEntity world@World { worldTimeline = timeline } = do
    newId <- getRandom
    let entity = Entity
            { entityId = newId
            , entityName = ""
            , entityStatGroups = []
            , entityAspects = []
            , entityCollapsed = False
            }
    return world { worldTimeline = Timeline.modify timeline $ flip snoc entity }

modifyEntity :: (Entity -> Maybe Entity) -> EntityId -> World -> World
modifyEntity f eid world@World { worldTimeline = timeline } =
    world { worldTimeline = Timeline.modify timeline $ mapMaybe modify }
  where
    modify entity@Entity { entityId = eid' }
        | eid == eid' = f entity
        | otherwise   = Just entity

toggleEntity :: EntityId -> World -> World
toggleEntity = modifyEntity $ \entity@Entity { entityCollapsed = collapsed } ->
    Just $ entity { entityCollapsed = not collapsed }

setEntityName :: String -> EntityId -> World -> World
setEntityName name = modifyEntity $ \entity -> Just $ entity { entityName = name }

moveEntity :: Int -> EntityId -> World -> World
moveEntity index eid world@World { worldTimeline = timeline } =
    case find ((==) eid . entityId) $ Timeline.value timeline of
        Just entity -> world { worldTimeline = Timeline.modify timeline $ move entity }
        Nothing -> world
  where
    move entity entities = take index removed ++ entity : drop index removed
      where
        removed = filter ((/=) eid . entityId) entities

removeEntity :: EntityId -> World -> World
removeEntity = modifyEntity $ const Nothing

addStatGroup :: RandomGen g => EntityId -> World -> Rand g World
addStatGroup eid world = do
    newId <- getRandom
    let statGroup = StatGroup newId "" []
    return $ modifyEntity (add statGroup) eid world
  where
    add statGroup entity@Entity { entityStatGroups = statGroups } =
        Just $ entity { entityStatGroups = snoc statGroups statGroup }

modifyStatGroup :: (StatGroup -> Maybe StatGroup) -> StatGroupId -> World -> World
modifyStatGroup f sgid world@World { worldTimeline = timeline } = world
    { worldTimeline = Timeline.modify timeline $ map $
        \entity@Entity { entityStatGroups = statGroups } ->
            entity { entityStatGroups = mapMaybe modify statGroups }
    }
  where
    modify statGroup@(StatGroup sgid' _ _)
        | sgid == sgid' = f statGroup
        | otherwise     = Just $ statGroup

setStatGroupName :: String -> StatGroupId -> World -> World
setStatGroupName name = modifyStatGroup $ \(StatGroup sgid _ stats) ->
    Just $ StatGroup sgid name stats

removeStatGroup :: StatGroupId -> World -> World
removeStatGroup = modifyStatGroup $ const Nothing

addStat :: RandomGen g => StatGroupId -> World -> Rand g World
addStat sgid world = do
    newId <- getRandom
    let stat = Stat newId "" 0
    return $ modifyStatGroup (add stat) sgid world
  where
    add stat (StatGroup sgid' name stats) = Just $ StatGroup sgid' name $ snoc stats stat

modifyStat :: (Stat -> Maybe Stat) -> StatId -> World -> World
modifyStat f sid world@World { worldTimeline = timeline } = world
    { worldTimeline = Timeline.modify timeline $ map $
        \entity@Entity { entityStatGroups = statGroups } -> entity
            { entityStatGroups = flip map statGroups $ \(StatGroup name sgid stats) ->
                StatGroup name sgid $ mapMaybe modify stats
            }
    }
  where
    modify stat@(Stat sid' _ _)
        | sid == sid' = f stat
        | otherwise   = Just $ stat

setStatName :: String -> StatId -> World -> World
setStatName name = modifyStat $ \(Stat sid _ score) -> Just $ Stat sid name score

setStatScore :: Int -> StatId -> World -> World
setStatScore score = modifyStat $ \(Stat sid name _) -> Just $ Stat sid name score

removeStat :: StatId -> World -> World
removeStat = modifyStat $ const Nothing

addAspect :: RandomGen g => EntityId -> World -> Rand g World
addAspect eid world = do
    newId <- getRandom
    let aspect = Aspect { aspectId = newId, aspectText = "", aspectDice = 0 }
    return $ modifyEntity (add aspect) eid world
  where
    add aspect entity@Entity { entityAspects = aspects } =
        Just $ entity { entityAspects = snoc aspects aspect }

modifyAspect :: (Aspect -> Maybe Aspect) -> AspectId -> World -> World
modifyAspect f aid world@World { worldTimeline = timeline } = world
    { worldTimeline = Timeline.modify timeline $ map $
        \entity@Entity { entityAspects = aspects } ->
            entity { entityAspects = mapMaybe modify aspects }
    }
  where
    modify aspect@Aspect { aspectId = aid' }
        | aid == aid' = f aspect
        | otherwise   = Just aspect

setAspectText :: String -> AspectId -> World -> World
setAspectText text = modifyAspect $ \aspect -> Just $ aspect { aspectText = text }

removeAspect :: AspectId -> World -> World
removeAspect = modifyAspect $ const Nothing

addDie :: AspectId -> World -> World
addDie = modifyAspect $ \aspect@Aspect { aspectDice = dice } ->
    Just $ aspect { aspectDice = dice + 1 }

removeDie :: AspectId -> World -> World
removeDie = modifyAspect $ \aspect@Aspect { aspectDice = dice } ->
    if dice >= 1
        then Just $ aspect { aspectDice = dice - 1 }
        else Just $ aspect

rollStat :: RandomGen g => RollId -> StatId -> World -> Rand g World
rollStat rid sid world@World { worldTimeline = timeline, worldEvents = events } = case stats of
    [Stat _ _ score] -> do
        -- TODO: Generate roll ID on the server and send it to just the client that initiated the
        -- roll.
        roll <- getRandomR (1, 6)
        return $ world { worldEvents = snoc events $ RollResult rid [roll, score] }
    _ -> return world
  where
    stats = concatMap (filter ((==) sid . statId) . concatMap statGroupStats . entityStatGroups) $
        Timeline.value timeline
    statGroupStats (StatGroup _ _ stats') = stats'
    statId (Stat sid' _ _) = sid'

rollAspect :: RandomGen g => RollId -> AspectId -> World -> Rand g World
rollAspect rid aid world@World { worldTimeline = timeline, worldEvents = events } = case aspects of
    [Aspect { aspectDice = dice }] | dice >= 1 -> do
        roll <- getRandomR (1, 6)
        let world' = world { worldEvents = updateEvents roll }
        return $ modifyAspect (setDice $ dice - 1) aid world'
    _ -> return world
  where
    aspects = concatMap (filter ((==) aid . aspectId) . entityAspects) $ Timeline.value timeline
    setDice dice aspect = Just $ aspect { aspectDice = dice }
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

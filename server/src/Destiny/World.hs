{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Destiny.World
    ( Command
    , Reply (..)
    , World (..)
    , Snapshot
    , commit
    , empty
    , redo
    , rollAspect
    , rollStat
    , undo
    , update
    , snapshot
    )
where

import Control.Lens
import Control.Monad.Random
import Data.Aeson.TH (deriveJSON)
import Data.Generics.Labels ()
import Destiny.Message
import Destiny.Scene
import Destiny.Timeline (Timeline)
import Elm.Derive
import GHC.Generics

import qualified Data.Map.Lazy as Map
import qualified Destiny.Timeline as Timeline

data World = World
    { timeline :: Timeline Scene
    , messages :: MessageList
    }
    deriving Generic
deriveJSON defaultOptions ''World

data Snapshot = Snapshot
    { scene :: Scene
    , messages :: MessageList
    }
    deriving Generic
deriveBoth defaultOptions ''Snapshot

data Command
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
    | MoveAspect AspectId EntityId Int
    | RemoveAspect AspectId
    | AddDie AspectId
    | RemoveDie AspectId
    | RollStat StatId MessageId
    | RollAspect AspectId MessageId
    | Undo
    | Redo
deriveBoth defaultOptions ''Command

data Reply
    = SendWorld
    | NoResponse

empty :: World
empty = World
    { timeline = Timeline.singleton emptyScene
    , messages = emptyMessages
    }

snapshot :: World -> Snapshot
snapshot world = Snapshot
    { scene = Timeline.present $ world ^. #timeline
    , messages = world ^. #messages
    }

update :: RandomGen r => Command -> World -> Rand r (World, Reply)
update request world = case request of
    AddEntity ->
        updateScene addEntity world <&> (, SendWorld)
    ToggleEntity entityId ->
        updateScene (return . toggleEntity entityId) world <&> (, SendWorld)
    SetEntityName entityId name' ->
        updateScene (return . setEntityName name' entityId) world <&> (, NoResponse)
    MoveEntity entityId i ->
        updateScene (return . moveEntity i entityId) world <&> (, SendWorld)
    RemoveEntity entityId ->
        updateScene (return . removeEntity entityId) world <&> (, SendWorld)
    AddStatGroup entityId ->
        updateScene (addStatGroup entityId) world <&> (, SendWorld)
    SetStatGroupName groupId name' ->
        updateScene (return . setStatGroupName name' groupId) world <&> (, NoResponse)
    RemoveStatGroup groupId ->
        updateScene (return . removeStatGroup groupId) world <&> (, SendWorld)
    AddStat groupId ->
        updateScene (addStat groupId) world <&> (, SendWorld)
    SetStatName statId name' ->
        updateScene (return . setStatName name' statId) world <&> (, NoResponse)
    SetStatScore statId score' ->
        updateScene (return . setStatScore score' statId) world <&> (, SendWorld)
    RemoveStat statId ->
        updateScene (return . removeStat statId) world <&> (, SendWorld)
    AddAspect entityId ->
        updateScene (addAspect entityId) world <&> (, SendWorld)
    SetAspectText aspectId text' ->
        updateScene (return . setAspectText text' aspectId) world <&> (, NoResponse)
    MoveAspect aspectId entityId i ->
        updateScene (return . moveAspect aspectId entityId i) world <&> (, SendWorld)
    RemoveAspect aspectId ->
        updateScene (return . removeAspect aspectId) world <&> (, SendWorld)
    AddDie aspectId ->
        updateScene (return . addDie aspectId) world <&> (, SendWorld)
    RemoveDie aspectId ->
        updateScene (return . removeDie aspectId) world <&> (, SendWorld)
    RollStat statId messageId ->
        rollStat statId messageId world <&> (, SendWorld)
    RollAspect aspectId messageId ->
        rollAspect aspectId messageId world <&> (, SendWorld)
    Undo ->
        return (undo world, SendWorld)
    Redo ->
        return (redo world, SendWorld)

updateScene :: RandomGen r => (Scene -> Rand r Scene) -> World -> Rand r World
updateScene f world = do
    scene' <- f $ Timeline.present $ world ^. #timeline
    return $ world & over #timeline (Timeline.update scene')

undo :: World -> World
undo = over #timeline Timeline.undo

redo :: World -> World
redo = over #timeline Timeline.redo

commit :: World -> World
commit = over #timeline Timeline.commit

rollStat :: RandomGen r => StatId -> MessageId -> World -> Rand r World
rollStat statId messageId world = case Map.lookup statId stats' of
    Just stat -> do
        -- TODO: Generate roll ID on the server and send it to just the client that initiated the
        -- roll.
        result <- getRandomR (1, 6)
        let roll = Roll
                { id = messageId
                , statName = stat ^. #name
                , statResult = result
                , statModifier = stat ^. #score
                , invokes = []
                }
        return $ world & over #messages (\(MessageList ids msgs) -> MessageList
            (snoc ids messageId)
            (Map.insert messageId (RollMessage roll) msgs))
    Nothing -> return world
  where
    stats' = Timeline.present (world ^. #timeline) ^. #stats

rollAspect :: RandomGen r => AspectId -> MessageId -> World -> Rand r World
rollAspect aspectId messageId world = case Map.lookup aspectId aspects' of
    Just aspect | aspect ^. #dice >= 1 -> do
        result <- getRandomR (1, 6)
        let invoke = Invoke (aspect ^. #text) result
        return $ world
            & over #timeline (Timeline.modify $ removeDie aspectId)
            & over #messages (\(MessageList ids msgs) -> MessageList
                ids
                (Map.adjust (append invoke) messageId msgs))
    _ -> return world
  where
    aspects' = Timeline.present (world ^. #timeline) ^. #aspects
    append invoke (RollMessage roll) = RollMessage $ roll & over #invokes (flip snoc invoke)

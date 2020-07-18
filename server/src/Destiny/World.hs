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
import qualified Data.MultiSet as MultiSet
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
    = All
    | Others

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

update :: RandomGen r => Role -> Command -> World -> Rand r (World, Reply)
update role command world = case command of
    AddEntity ->
        updateScene addEntity world <&> (, All)
    ToggleEntity entityId ->
        updateScene (return . toggleEntity entityId) world <&> (, All)
    SetEntityName entityId name' ->
        updateScene (return . setEntityName name' entityId) world <&> (, Others)
    MoveEntity entityId i ->
        updateScene (return . moveEntity i entityId) world <&> (, All)
    RemoveEntity entityId ->
        updateScene (return . removeEntity entityId) world <&> (, All)
    AddStatGroup entityId ->
        updateScene (addStatGroup entityId) world <&> (, All)
    SetStatGroupName groupId name' ->
        updateScene (return . setStatGroupName name' groupId) world <&> (, Others)
    RemoveStatGroup groupId ->
        updateScene (return . removeStatGroup groupId) world <&> (, All)
    AddStat groupId ->
        updateScene (addStat groupId) world <&> (, All)
    SetStatName statId name' ->
        updateScene (return . setStatName name' statId) world <&> (, Others)
    SetStatScore statId score' ->
        updateScene (return . setStatScore score' statId) world <&> (, All)
    RemoveStat statId ->
        updateScene (return . removeStat statId) world <&> (, All)
    AddAspect entityId ->
        updateScene (addAspect entityId) world <&> (, All)
    SetAspectText aspectId text' ->
        updateScene (return . setAspectText text' aspectId) world <&> (, Others)
    MoveAspect aspectId entityId i ->
        updateScene (return . moveAspect aspectId entityId i) world <&> (, All)
    RemoveAspect aspectId ->
        updateScene (return . removeAspect aspectId) world <&> (, All)
    AddDie aspectId ->
        updateScene (return . addDie (Die role) aspectId) world <&> (, All)
    RemoveDie aspectId ->
        updateScene (return . removeDie (Die role) aspectId) world <&> (, All)
    RollStat statId messageId ->
        rollStat statId messageId world <&> (, All)
    RollAspect aspectId messageId ->
        rollAspect (Die role) aspectId messageId world <&> (, All)
    Undo ->
        return (undo world, All)
    Redo ->
        return (redo world, All)

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

rollAspect :: RandomGen r => Die -> AspectId -> MessageId -> World -> Rand r World
rollAspect die aspectId messageId world = case Map.lookup aspectId aspects' of
    Just aspect | MultiSet.member die $ aspect ^. #dice -> do
        result <- getRandomR (1, 6)
        let invoke = Invoke (aspect ^. #text) result
        return $ world
            & over #timeline (Timeline.modify $ removeDie die aspectId)
            & over #messages (\(MessageList ids msgs) -> MessageList
                ids
                (Map.adjust (append invoke) messageId msgs))
    _ -> return world
  where
    aspects' = Timeline.present (world ^. #timeline) ^. #aspects
    append invoke (RollMessage roll) = RollMessage $ roll & over #invokes (flip snoc invoke)

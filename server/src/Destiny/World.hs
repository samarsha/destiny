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
update role' command world = case command of
    AddEntity ->
        commit world & updateScene addEntity <&> (, All)
    ToggleEntity entityId ->
        world & updateScene (return . toggleEntity entityId) <&> (, All)
    SetEntityName entityId name' ->
        world & updateScene (return . setEntityName name' entityId) <&> (, Others)
    MoveEntity entityId i ->
        world & updateScene (return . moveEntity i entityId) <&> (, All)
    RemoveEntity entityId ->
        commit world & updateScene (return . removeEntity entityId) <&> (, All)
    AddStatGroup entityId ->
        commit world & updateScene (addStatGroup entityId) <&> (, All)
    SetStatGroupName groupId name' ->
        world & updateScene (return . setStatGroupName name' groupId) <&> (, Others)
    RemoveStatGroup groupId ->
        commit world & updateScene (return . removeStatGroup groupId) <&> (, All)
    AddStat groupId ->
        commit world & updateScene (addStat groupId) <&> (, All)
    SetStatName statId name' ->
        world & updateScene (return . setStatName name' statId) <&> (, Others)
    SetStatScore statId score' ->
        world & updateScene (return . setStatScore score' statId) <&> (, All)
    RemoveStat statId ->
        commit world & updateScene (return . removeStat statId) <&> (, All)
    AddAspect entityId ->
        commit world & updateScene (addAspect entityId) <&> (, All)
    SetAspectText aspectId text' ->
        world & updateScene (return . setAspectText text' aspectId) <&> (, Others)
    MoveAspect aspectId entityId i ->
        world & updateScene (return . moveAspect aspectId entityId i) <&> (, All)
    RemoveAspect aspectId ->
        commit world & updateScene (return . removeAspect aspectId) <&> (, All)
    AddDie aspectId ->
        commit world & updateScene (return . addDie (Die role') aspectId) <&> (, All)
    RemoveDie aspectId ->
        commit world & updateScene (return . removeDie (Die role') aspectId) <&> (, All)
    RollStat statId messageId ->
        world & rollStat role' statId messageId <&> (, All)
    RollAspect aspectId messageId ->
        commit world & rollAspect (Die role') aspectId messageId <&> (, All)
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

rollStat :: RandomGen r => Role -> StatId -> MessageId -> World -> Rand r World
rollStat role' statId messageId world = case Map.lookup statId stats' of
    Just stat -> do
        -- TODO: Generate roll ID on the server and send it to just the client that initiated the
        -- roll.
        result' <- getRandomR (1, 6)
        let roll = Roll
                { id = messageId
                , role = role'
                , statName = stat ^. #name
                , statResult = result'
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
rollAspect die@(Die role') aspectId messageId world = case Map.lookup aspectId aspects' of
    Just aspect | MultiSet.member die $ aspect ^. #dice -> do
        result' <- getRandomR (1, 6)
        let invoke = Invoke { source = aspect ^. #text, role = role', result = result' }
        return $ world
            & over #timeline (Timeline.modify $ removeDie die aspectId)
            & over #messages (\(MessageList ids msgs) -> MessageList
                ids
                (Map.adjust (append invoke) messageId msgs))
    _ -> return world
  where
    aspects' = Timeline.present (world ^. #timeline) ^. #aspects
    append invoke (RollMessage roll) = RollMessage $ roll & over #invokes (flip snoc invoke)

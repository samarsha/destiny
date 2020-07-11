{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Destiny.Request (ClientRequest, ClientResponse (..), updateWorld) where

import Control.Lens.Operators
import Control.Monad.Random
import Destiny.Message
import Destiny.Scene
import Destiny.World
import Elm.Derive

import qualified Destiny.Timeline as Timeline

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
    | MoveAspect AspectId EntityId Int
    | RemoveAspect AspectId
    | AddDie AspectId
    | RemoveDie AspectId
    | RollStat StatId MessageId
    | RollAspect AspectId MessageId
    | Undo
    | Redo

data ClientResponse
    = UpdateWorld
    | NoResponse

deriveBoth defaultOptions ''ClientRequest

updateWorld :: RandomGen r => ClientRequest -> World -> Rand r (World, ClientResponse)
updateWorld request world = case request of
    AddEntity ->
        modifyScene addEntity world <&> (, UpdateWorld)
    ToggleEntity entityId ->
        modifyScene (return . toggleEntity entityId) world <&> (, UpdateWorld)
    SetEntityName entityId name ->
        modifyScene (return . setEntityName name entityId) world <&> (, NoResponse)
    MoveEntity entityId index ->
        modifyScene (return . moveEntity index entityId) world <&> (, UpdateWorld)
    RemoveEntity entityId ->
        modifyScene (return . removeEntity entityId) world <&> (, UpdateWorld)
    AddStatGroup entityId ->
        modifyScene (addStatGroup entityId) world <&> (, UpdateWorld)
    SetStatGroupName groupId name ->
        modifyScene (return . setStatGroupName name groupId) world <&> (, NoResponse)
    RemoveStatGroup groupId ->
        modifyScene (return . removeStatGroup groupId) world <&> (, UpdateWorld)
    AddStat groupId ->
        modifyScene (addStat groupId) world <&> (, UpdateWorld)
    SetStatName statId name ->
        modifyScene (return . setStatName name statId) world <&> (, NoResponse)
    SetStatScore statId score ->
        modifyScene (return . setStatScore score statId) world <&> (, UpdateWorld)
    RemoveStat statId ->
        modifyScene (return . removeStat statId) world <&> (, UpdateWorld)
    AddAspect entityId ->
        modifyScene (addAspect entityId) world <&> (, UpdateWorld)
    SetAspectText aspectId text' ->
        modifyScene (return . setAspectText text' aspectId) world <&> (, NoResponse)
    MoveAspect aspectId entityId index ->
        modifyScene (return . moveAspect aspectId entityId index) world <&> (, UpdateWorld)
    RemoveAspect aspectId ->
        modifyScene (return . removeAspect aspectId) world <&> (, UpdateWorld)
    AddDie aspectId ->
        modifyScene (return . addDie aspectId) world <&> (, UpdateWorld)
    RemoveDie aspectId ->
        modifyScene (return . removeDie aspectId) world <&> (, UpdateWorld)
    RollStat statId messageId ->
        rollStat statId messageId world <&> (, UpdateWorld)
    RollAspect aspectId messageId ->
        rollAspect aspectId messageId world <&> (, UpdateWorld)
    Undo ->
        return (undo world, UpdateWorld)
    Redo ->
        return (redo world, UpdateWorld)

modifyScene :: RandomGen r => (Scene -> Rand r Scene) -> World -> Rand r World
modifyScene f world = do
    scene' <- f $ Timeline.value $ world^.timeline
    return $ world & timeline %~ Timeline.update scene'

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
    ToggleEntity eid ->
        modifyScene (return . toggleEntity eid) world <&> (, UpdateWorld)
    SetEntityName eid name ->
        modifyScene (return . setEntityName name eid) world <&> (, NoResponse)
    MoveEntity eid index ->
        modifyScene (return . moveEntity index eid) world <&> (, UpdateWorld)
    RemoveEntity eid ->
        modifyScene (return . removeEntity eid) world <&> (, UpdateWorld)
    AddStatGroup eid ->
        modifyScene (addStatGroup eid) world <&> (, UpdateWorld)
    SetStatGroupName sgid name ->
        modifyScene (return . setStatGroupName name sgid) world <&> (, NoResponse)
    RemoveStatGroup sgid ->
        modifyScene (return . removeStatGroup sgid) world <&> (, UpdateWorld)
    AddStat sgid ->
        modifyScene (addStat sgid) world <&> (, UpdateWorld)
    SetStatName sid name ->
        modifyScene (return . setStatName name sid) world <&> (, NoResponse)
    SetStatScore sid score ->
        modifyScene (return . setStatScore score sid) world <&> (, UpdateWorld)
    RemoveStat sid ->
        modifyScene (return . removeStat sid) world <&> (, UpdateWorld)
    AddAspect eid ->
        modifyScene (addAspect eid) world <&> (, UpdateWorld)
    SetAspectText aid text' ->
        modifyScene (return . setAspectText text' aid) world <&> (, NoResponse)
    MoveAspect aid eid index ->
        modifyScene (return . moveAspect aid eid index) world <&> (, UpdateWorld)
    RemoveAspect aid ->
        modifyScene (return . removeAspect aid) world <&> (, UpdateWorld)
    AddDie aid ->
        modifyScene (return . addDie aid) world <&> (, UpdateWorld)
    RemoveDie aid ->
        modifyScene (return . removeDie aid) world <&> (, UpdateWorld)
    RollStat sid mid ->
        rollStat sid mid world <&> (, UpdateWorld)
    RollAspect aid mid ->
        rollAspect aid mid world <&> (, UpdateWorld)
    Undo ->
        return (undo world, UpdateWorld)
    Redo ->
        return (redo world, UpdateWorld)

modifyScene :: RandomGen r => (Scene -> Rand r Scene) -> World -> Rand r World
modifyScene f world = do
    scene' <- f $ Timeline.value $ world ^. timeline
    return $ world & timeline %~ Timeline.update scene'

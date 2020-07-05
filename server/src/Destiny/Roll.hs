module Destiny.Roll (rollAspect, rollStat) where

import Control.Monad.Random
import Data.List.Extra
import Destiny.Scene
import Destiny.World

import qualified Data.Map.Lazy as Map
import qualified Destiny.Timeline as Timeline

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
        return world
            { worldTimeline = Timeline.modify timeline $ removeDie aid
            , worldEvents = updateEvents roll
            }
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

﻿namespace Destiny.Server

open Destiny.Shared
open Destiny.Shared.Bag
open Destiny.Shared.Collections
open Destiny.Shared.Collections.OptionBuilder
open Destiny.Shared.Roll
open Destiny.Shared.World
open System

type internal Universe =
    { History : World Timeline
      Rolls : RollLog }

module internal Universe =
    let history = { Get = (fun s -> s.History); Set = fun v s -> { s with History = v } }

    let empty =
        { History = Timeline.singleton World.empty
          Rolls = RollLog.empty }

    let private d6 (random : Random) = random.Next (1, 7)

    let private addRoll (roll : Roll) universe =
        // TODO: Verify that the roll ID doesn't exist.
        let rolls =
            { Map = Map.add roll.Id roll universe.Rolls.Map
              Order = List.add roll.Id universe.Rolls.Order }
        { universe with Rolls = rolls }

    let private addInvoke invoke rollId universe =
        // TODO: Verify that the roll ID exists.
        let add roll = { roll with Invokes = List.add invoke roll.Invokes }
        let rolls = { universe.Rolls with Map = Map.change add rollId universe.Rolls.Map }
        { universe with Rolls = rolls }

    let rollStat random (die : Die) statId rollId universe =
        let world = Timeline.present universe.History
        option {
            let! stat = Map.tryFind statId world.Catalog.Stats
            let! entity =
                Map.tryFind stat.Group world.Catalog.StatGroups
                |> Option.bind (fun group -> Map.tryFind group.Entity world.Catalog.Entities)
            let roll =
                { Id = rollId
                  Entity = entity.Name
                  Stat = stat.Name
                  Role = die.Role
                  Result = d6 random
                  Modifier = stat.Score
                  Invokes = [] }
            return addRoll roll universe
        } |> Option.defaultValue universe

    let rollAspect random die aspectId rollId universe =
        let world = Timeline.present universe.History
        option {
            let! aspect = Map.tryFind aspectId world.Catalog.Aspects
            if Bag.contains die aspect.Dice then
                let! entity = Map.tryFind aspect.Entity world.Catalog.Entities
                let invoke =
                    { Entity = entity.Name
                      Aspect = aspect.Description
                      Role = die.Role
                      Result = d6 random }
                let universe' = addInvoke invoke rollId universe
                let history = universe'.History |> Timeline.commit |> Timeline.update (World.removeDie die aspectId)
                return { universe' with History = history }
        } |> Option.defaultValue universe

    let rollSpare random (die : Die) rollId universe =
        if Map.containsKey rollId universe.Rolls.Map
        then
            let invoke =
                { Entity = "Dice bag"
                  Aspect = "Spare die"
                  Role = die.Role
                  Result = d6 random }
            addInvoke invoke rollId universe
        else
            let roll =
                { Id = rollId
                  Entity = "Dice bag"
                  Stat = "Spare die"
                  Role = die.Role
                  Result = d6 random
                  Modifier = 0
                  Invokes = [] }
            addRoll roll universe

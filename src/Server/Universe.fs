namespace Destiny.Server

open Destiny.Shared
open Destiny.Shared.Bag
open Destiny.Shared.Board
open Destiny.Shared.Collections
open Destiny.Shared.Collections.OptionBuilder
open Destiny.Shared.Roll
open System

type internal Universe =
    { Boards : Board Timeline
      Rolls : RollLog }

module internal Universe =
    let boards = { Get = (fun s -> s.Boards); Set = fun v s -> { s with Boards = v } }

    let empty =
        { Boards = Timeline.singleton Board.empty
          Rolls = RollLog.empty }

    let rollStat (random : Random) role statId rollId universe =
        let board = Timeline.present universe.Boards
        option {
            let! stat = Map.tryFind statId board.Stats
            let! entity =
                Map.tryFind stat.Group board.StatGroups
                |> Option.bind (fun group -> Map.tryFind group.Entity board.Entities)
            // TODO: Verify that the roll ID doesn't exist.
            let roll =
                { Id = rollId
                  Entity = entity.Name
                  Stat = stat.Name
                  Role = role
                  Result = random.Next (1, 7)
                  Modifier = stat.Score
                  Invokes = [] }
            let rolls =
                { Map = Map.add rollId roll universe.Rolls.Map
                  Order = List.add rollId universe.Rolls.Order }
            return { universe with Rolls = rolls }
        } |> Option.defaultValue universe

    let rollAspect (random : Random) die aspectId rollId universe =
        let board = Timeline.present universe.Boards
        option {
            let! aspect = Map.tryFind aspectId board.Aspects
            if Bag.contains die aspect.Dice then
                let! entity = Map.tryFind aspect.Entity board.Entities
                let invoke =
                    { Entity = entity.Name
                      Aspect = aspect.Description
                      Role = die.Role
                      Result = random.Next (1, 7) }
                let addInvoke roll = { roll with Invokes = List.add invoke roll.Invokes }
                let boards = universe.Boards |> Timeline.commit |> Timeline.update (Board.removeDie die aspectId)
                let rolls = { universe.Rolls with Map = Map.change addInvoke rollId universe.Rolls.Map }
                return { universe with Boards = boards; Rolls = rolls }
        } |> Option.defaultValue universe

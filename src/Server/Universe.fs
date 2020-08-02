namespace Destiny.Server

open Destiny.Shared
open Destiny.Shared.Bag
open Destiny.Shared.Board
open Destiny.Shared.Collections
open Destiny.Shared.World
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
        match Map.tryFind statId (Timeline.present universe.Boards).Stats with
        | Some stat ->
            // TODO: Verify that the roll ID doesn't exist.
            let roll =
                { Id = rollId
                  Role = role
                  StatName = stat.Name
                  StatResult = random.Next (1, 7)
                  StatBase = stat.Score
                  Invokes = [] }
            let rolls' =
                { Map = Map.add rollId roll universe.Rolls.Map
                  Order = List.add rollId universe.Rolls.Order }
            { universe with Rolls = rolls' }
        | None -> universe

    let rollAspect (random : Random) (Die role as die) aspectId rollId universe =
        match Map.tryFind aspectId (Timeline.present universe.Boards).Aspects with
        | Some aspect when Bag.contains die aspect.Dice ->
            let invoke =
                { Source = aspect.Description
                  Role = role
                  Result = random.Next (1, 7) }
            let addInvoke roll = { roll with Invokes = List.add invoke roll.Invokes }
            let rolls' = { universe.Rolls with Map = Map.change addInvoke rollId universe.Rolls.Map }
            { universe with
                  Boards = universe.Boards |> Timeline.commit |> Timeline.update (Board.removeDie die aspectId)
                  Rolls = rolls' }
        | _ -> universe

namespace Destiny.Server

open Destiny.Shared
open Destiny.Shared.Bag
open Destiny.Shared.Board
open Destiny.Shared.Collections
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
        match Map.tryFind statId board.Stats with
        | Some stat ->
            let entity =
                Map.tryFind stat.Group board.StatGroups
                |> Option.bind (fun group -> Map.tryFind group.Entity board.Entities)
            let name =
                match entity with
                | Some entity' -> entity'.Name + ": " + stat.Name
                | None -> stat.Name
            // TODO: Verify that the roll ID doesn't exist.
            let roll =
                { Id = rollId
                  Role = role
                  Name = name
                  Result = random.Next (1, 7)
                  Modifier = stat.Score
                  Invokes = [] }
            let rolls' =
                { Map = Map.add rollId roll universe.Rolls.Map
                  Order = List.add rollId universe.Rolls.Order }
            { universe with Rolls = rolls' }
        | None -> universe

    let rollAspect (random : Random) die aspectId rollId universe =
        let board = Timeline.present universe.Boards
        match Map.tryFind aspectId board.Aspects with
        | Some aspect when Bag.contains die aspect.Dice ->
            let source =
                match Map.tryFind aspect.Entity board.Entities with
                | Some entity -> entity.Name + ": " + aspect.Description
                | None -> aspect.Description
            let invoke =
                { Source = source
                  Role = die.Role
                  Result = random.Next (1, 7) }
            let addInvoke roll = { roll with Invokes = List.add invoke roll.Invokes }
            let rolls' = { universe.Rolls with Map = Map.change addInvoke rollId universe.Rolls.Map }
            { universe with
                  Boards = universe.Boards |> Timeline.commit |> Timeline.update (Board.removeDie die aspectId)
                  Rolls = rolls' }
        | _ -> universe

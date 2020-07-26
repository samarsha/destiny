module internal Destiny.Server.Roll

open Destiny.Shared
open Destiny.Shared.Bag
open Destiny.Shared.Board
open Destiny.Shared.World
open System

let rollStat (random : Random) role statId rollId world =
    match Map.tryFind statId world.Board.Stats with
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
            { Map = Map.add rollId roll world.Rolls.Map
              Order = List.add rollId world.Rolls.Order }
        { world with Rolls = rolls' }
    | None -> world

let rollAspect (random : Random) (Die role as die) aspectId rollId world =
    match Map.tryFind aspectId world.Board.Aspects with
    | Some aspect when Bag.contains die aspect.Dice ->
        let invoke =
            { Source = aspect.Description
              Role = role
              Result = random.Next (1, 7) }
        let addInvoke roll = { roll with Invokes = List.add invoke roll.Invokes }
        let rolls' = { world.Rolls with Map = Map.change addInvoke rollId world.Rolls.Map }
        { world with
              Board = Board.removeDie die aspectId world.Board
              Rolls = rolls' }
    | _ -> world

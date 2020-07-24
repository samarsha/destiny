namespace Destiny.Shared.Board

open Destiny.Shared.Bag
open Destiny.Shared.Collections
open FSharpPlus.Lens
open FSharpPlus.Operators
open System

type 'a Id = private Id of Guid

type Stat =
    { Id : Stat Id
      Name : string
      Score : int }

module private Stat =
    let inline _name f stat = f stat.Name <&> fun n -> { stat with Name = n }
    let inline _score f stat = f stat.Score <&> fun s -> { stat with Score = s }

type StatGroup =
    { Id : StatGroup Id
      Name : string
      Stats : Stat Id list }

module private StatGroup =
    let inline _name f group = f group.Name <&> fun n -> { group with Name = n }
    let inline _stats f group = f group.Stats <&> fun s -> { group with Stats = s }

type Role =
    | Player
    | DM

type Die = Die of Role

type Aspect =
    { Id : Aspect Id
      Description : string
      Dice : Die Bag }

module private Aspect =
    let inline _description f aspect = f aspect.Description <&> fun d -> { aspect with Description = d }
    let inline _dice f aspect = f aspect.Dice <&> fun d -> { aspect with Dice = d }

type Entity =
    { Id : Entity Id
      Name : string
      StatGroups : StatGroup Id list
      Aspects : Aspect Id list
      Collapsed : bool }

module private Entity =
    let inline _name f entity = f entity.Name <&> fun n -> { entity with Name = n }
    let inline _statGroups f entity = f entity.StatGroups <&> fun s -> { entity with StatGroups = s }
    let inline _aspects f entity = f entity.Aspects <&> fun a -> { entity with Aspects = a }
    let inline _collapsed f entity = f entity.Collapsed <&> fun c -> { entity with Collapsed = c }

type Board =
    { Entities : Map<Entity Id, Entity>
      StatGroups : Map<StatGroup Id, StatGroup>
      Stats : Map<Stat Id, Stat>
      Aspects : Map<Aspect Id, Aspect>
      Sequence : Entity Id list }

module internal Board =
    let inline private _entities f board = f board.Entities <&> fun e -> { board with Entities = e }
    let inline private _statGroups f board = f board.StatGroups <&> fun g -> { board with StatGroups = g }
    let inline private _stats f board = f board.Stats <&> fun s -> { board with Stats = s }
    let inline private _aspects f board = f board.Aspects <&> fun a -> { board with Aspects = a }
    let inline private _sequence f board = f board.Sequence <&> fun s -> { board with Sequence = s }

    let empty =
        { Entities = Map.empty
          Aspects = Map.empty
          Stats = Map.empty
          StatGroups = Map.empty
          Sequence = [] }

    let randomId () = Id <| Guid.NewGuid ()

    // Stats

    let addStat statId groupId =
        let stat =
            { Id = statId
              Name = ""
              Score = 0 }
        over _statGroups (Map.change (List.add statId |> over StatGroup._stats) groupId) >>
        over _stats (Map.add statId stat)

    let setStatName name = Map.change (Stat._name .-> name) >> over _stats

    let setStatScore score = Map.change (Stat._score .-> score) >> over _stats

    let removeStat id =
        over _statGroups (Map.map <| fun _ -> over StatGroup._stats (List.remove id)) >>
        over _stats (Map.remove id)

    // Stat Groups

    let addStatGroup groupId entityId =
        let group =
            { Id = groupId
              Name = ""
              Stats = [] }
        over _entities (Map.change (List.add groupId |> over Entity._statGroups) entityId) >>
        over _statGroups (Map.add groupId group)

    let setStatGroupName name = Map.change (StatGroup._name .-> name) >> over _statGroups

    let removeStatGroup id board =
        let group = Map.tryFind id board.StatGroups
        let stats = group |> Option.map (view StatGroup._stats) |> Option.defaultValue []
        board
        |> over _entities (Map.map <| fun _ -> List.remove id |> over Entity._statGroups)
        |> over _statGroups (Map.remove id)
        |> flip (List.fold <| flip removeStat) stats

    // Aspects

    let addAspect aspectId entityId =
        let aspect =
            { Id = aspectId
              Description = ""
              Dice = Bag.empty }
        over _entities (Map.change (List.add aspectId |> over Entity._aspects) entityId) >>
        over _aspects (Map.add aspectId aspect)

    let setAspectDescription description = Map.change (Aspect._description .-> description) >> over _aspects

    let addDie die = Map.change (Bag.add die |> over Aspect._dice) >> over _aspects

    let removeDie die = Map.change (Bag.remove die |> over Aspect._dice) >> over _aspects

    let moveAspect aspectId entityId index =
        Map.map (fun _ -> List.remove aspectId |> over Entity._aspects)
        >> Map.change (List.insertAt index aspectId |> over Entity._aspects) entityId
        |> over _entities

    let removeAspect id =
        over _entities (Map.map <| fun _ -> List.remove id |> over Entity._aspects) >>
        over _aspects (Map.remove id)

    // Entities

    let addEntity id =
        let entity =
            { Id = id
              Name = ""
              StatGroups = []
              Aspects = []
              Collapsed = false }
        over _entities (Map.add id entity) >>
        over _sequence (fun sequence -> sequence @ [ id ])

    let setEntityName name = Map.change (Entity._name .-> name) >> over _entities

    let collapseEntity = Map.change (over Entity._collapsed not) >> over _entities

    let moveEntity index id = List.remove id >> List.insertAt index id |> over _sequence

    let removeEntity id board =
        match Map.tryFind id board.Entities with
        | Some entity ->
            board
            |> over _sequence (List.remove id)
            |> over _entities (Map.remove id)
            |> flip (List.fold <| flip removeStatGroup) entity.StatGroups
            |> flip (List.fold <| flip removeAspect) entity.Aspects
        | None -> board

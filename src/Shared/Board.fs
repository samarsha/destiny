namespace Destiny.Shared.Board

open Destiny.Shared
open Destiny.Shared.Bag
open Destiny.Shared.Lens
open System

type 'a Id = private Id of Guid

type Stat =
    { Id : Stat Id
      Name : string
      Score : int }

module private Stat =
    let name = { Get = (fun s -> s.Name); Set = fun v s -> { s with Name = v } }

    let score = { Get = (fun s -> s.Score); Set = fun v s -> { s with Score = v } }

type StatGroup =
    { Id : StatGroup Id
      Name : string
      Stats : Stat Id list }

module private StatGroup =
    let name = { Get = (fun s -> s.Name); Set = fun v s -> { s with Name = v } }

    let stats = { Get = (fun s -> s.Stats); Set = fun v s -> { s with Stats = v } }

type Role =
    | Player
    | DM

type Die = Die of Role

type Aspect =
    { Id : Aspect Id
      Description : string
      Dice : Die Bag }

module private Aspect =
    let description = { Get = (fun s -> s.Description); Set = fun v s -> { s with Description = v } }

    let dice = { Get = (fun s -> s.Dice); Set = fun v s -> { s with Dice = v } }

type Entity =
    { Id : Entity Id
      Name : string
      StatGroups : StatGroup Id list
      Aspects : Aspect Id list
      Collapsed : bool }

module private Entity =
    let name = { Get = (fun s -> s.Name); Set = fun v s -> { s with Name = v } }

    let statGroups = { Get = (fun s -> s.StatGroups); Set = fun v s -> { s with StatGroups = v } }

    let aspects = { Get = (fun s -> s.Aspects); Set = fun v s -> { s with Aspects = v } }

    let collapsed = { Get = (fun s -> s.Collapsed); Set = fun v s -> { s with Collapsed = v } }

type Board =
    { Entities : Map<Entity Id, Entity>
      StatGroups : Map<StatGroup Id, StatGroup>
      Stats : Map<Stat Id, Stat>
      Aspects : Map<Aspect Id, Aspect>
      Order : Entity Id list }

module internal Board =
    let private entities = { Get = (fun s -> s.Entities); Set = fun v s -> { s with Entities = v } }

    let private statGroups = { Get = (fun s -> s.StatGroups); Set = fun v s -> { s with StatGroups = v } }

    let private stats = { Get = (fun s -> s.Stats); Set = fun v s -> { s with Stats = v } }

    let private aspects = { Get = (fun s -> s.Aspects); Set = fun v s -> { s with Aspects = v } }

    let private order = { Get = (fun s -> s.Order); Set = fun v s -> { s with Order = v } }

    let private flip f x y = f y x

    let empty =
        { Entities = Map.empty
          Aspects = Map.empty
          Stats = Map.empty
          StatGroups = Map.empty
          Order = [] }

    let randomId () = Id <| Guid.NewGuid ()

    // Stats

    let addStat statId groupId =
        let stat =
            { Id = statId
              Name = ""
              Score = 0 }
        over statGroups (Map.change (List.add statId |> over StatGroup.stats) groupId) >>
        over stats (Map.add statId stat)

    let setStatName name = Map.change (Stat.name .<- name) >> over stats

    let setStatScore score = Map.change (Stat.score .<- score) >> over stats

    let removeStat id =
        over statGroups (Map.map <| fun _ -> over StatGroup.stats (List.remove id)) >>
        over stats (Map.remove id)

    // Stat Groups

    let addStatGroup groupId entityId =
        let group =
            { Id = groupId
              Name = ""
              Stats = [] }
        over entities (Map.change (List.add groupId |> over Entity.statGroups) entityId) >>
        over statGroups (Map.add groupId group)

    let setStatGroupName name = Map.change (StatGroup.name .<- name) >> over statGroups

    let removeStatGroup id board =
        match Map.tryFind id board.StatGroups with
        | Some group ->
            board
            |> over entities (Map.map <| fun _ -> List.remove id |> over Entity.statGroups)
            |> over statGroups (Map.remove id)
            |> flip (List.fold <| flip removeStat) group.Stats
        | None -> board

    // Aspects

    let addAspect aspectId entityId =
        let aspect =
            { Id = aspectId
              Description = ""
              Dice = Bag.empty }
        over entities (Map.change (List.add aspectId |> over Entity.aspects) entityId) >>
        over aspects (Map.add aspectId aspect)

    let setAspectDescription description = Map.change (Aspect.description .<- description) >> over aspects

    let addDie die = Map.change (Bag.add die |> over Aspect.dice) >> over aspects

    let removeDie die = Map.change (Bag.remove die |> over Aspect.dice) >> over aspects

    let moveAspect aspectId entityId index =
        Map.map (fun _ -> List.remove aspectId |> over Entity.aspects)
        >> Map.change (List.insertAt index aspectId |> over Entity.aspects) entityId
        |> over entities

    let removeAspect id =
        over entities (Map.map <| fun _ -> List.remove id |> over Entity.aspects) >>
        over aspects (Map.remove id)

    // Entities

    let addEntity id =
        let entity =
            { Id = id
              Name = ""
              StatGroups = []
              Aspects = []
              Collapsed = false }
        over entities (Map.add id entity) >>
        over order (fun order -> order @ [ id ])

    let setEntityName name = Map.change (Entity.name .<- name) >> over entities

    let collapseEntity = Map.change (over Entity.collapsed not) >> over entities

    let moveEntity index id = List.remove id >> List.insertAt index id |> over order

    let removeEntity id board =
        match Map.tryFind id board.Entities with
        | Some entity ->
            board
            |> over order (List.remove id)
            |> over entities (Map.remove id)
            |> flip (List.fold <| flip removeStatGroup) entity.StatGroups
            |> flip (List.fold <| flip removeAspect) entity.Aspects
        | None -> board

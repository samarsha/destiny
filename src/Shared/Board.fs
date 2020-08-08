namespace Destiny.Shared.Board

open Destiny.Shared
open Destiny.Shared.Bag
open Destiny.Shared.Collections
open Destiny.Shared.Lens

type Role =
    | Player
    | DM

type Die = { Role : Role }

type Stat =
    { Id : Stat Id
      Group : StatGroup Id
      Name : string
      Score : int }

and StatGroup =
    { Id : StatGroup Id
      Entity : Entity Id
      Name : string
      Stats : Stat Id list }

and Aspect =
    { Id : Aspect Id
      Entity : Entity Id
      Description : string
      Dice : Die Bag }

and Entity =
    { Id : Entity Id
      Name : string
      StatGroups : StatGroup Id list
      Aspects : Aspect Id list
      Collapsed : bool }

module private Stat =
    let name = { Get = (fun (s : Stat) -> s.Name); Set = fun v s -> { s with Name = v } }

    let score = { Get = (fun s -> s.Score); Set = fun v s -> { s with Score = v } }

module private StatGroup =
    let name = { Get = (fun (s : StatGroup) -> s.Name); Set = fun v s -> { s with Name = v } }

    let stats = { Get = (fun s -> s.Stats); Set = fun v s -> { s with Stats = v } }

module private Aspect =
    let entity = { Get = (fun s -> s.Entity); Set = fun v s -> { s with Entity = v } }

    let description = { Get = (fun s -> s.Description); Set = fun v s -> { s with Description = v } }

    let dice = { Get = (fun s -> s.Dice); Set = fun v s -> { s with Dice = v } }

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

module Board =
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

    // TODO: In add functions, verify that the ID doesn't already exist.

    // Stats

    let internal addStat statId groupId =
        let stat =
            { Id = statId
              Group = groupId
              Name = ""
              Score = 0 }
        over statGroups (Map.change (List.add statId |> over StatGroup.stats) groupId) >>
        over stats (Map.add statId stat)

    let internal setStatName name = Map.change (Stat.name .<- name) >> over stats

    let internal setStatScore score = Map.change (Stat.score .<- score) >> over stats

    let internal removeStat id =
        over statGroups (Map.map <| fun _ -> over StatGroup.stats (List.remove id)) >>
        over stats (Map.remove id)

    // Stat Groups

    let internal addStatGroup groupId entityId =
        let group =
            { Id = groupId
              Entity = entityId
              Name = ""
              Stats = [] }
        over entities (Map.change (List.add groupId |> over Entity.statGroups) entityId) >>
        over statGroups (Map.add groupId group)

    let internal setStatGroupName name = Map.change (StatGroup.name .<- name) >> over statGroups

    let internal removeStatGroup id board =
        match Map.tryFind id board.StatGroups with
        | Some group ->
            board
            |> over entities (Map.map <| fun _ -> List.remove id |> over Entity.statGroups)
            |> over statGroups (Map.remove id)
            |> flip (List.fold <| flip removeStat) group.Stats
        | None -> board

    // Aspects

    let internal addAspect aspectId entityId =
        let aspect =
            { Id = aspectId
              Entity = entityId
              Description = ""
              Dice = Bag.empty }
        over entities (Map.change (List.add aspectId |> over Entity.aspects) entityId) >>
        over aspects (Map.add aspectId aspect)

    let internal setAspectDescription description = Map.change (Aspect.description .<- description) >> over aspects

    let internal addDie die = Map.change (Bag.add die |> over Aspect.dice) >> over aspects

    let removeDie die = Map.change (Bag.remove die |> over Aspect.dice) >> over aspects

    let internal moveAspect aspectId entityId index =
        let remove = Map.map (fun _ -> List.remove aspectId |> over Entity.aspects)
        let add = Map.change (List.insertAt index aspectId |> over Entity.aspects) entityId
        let setParent = Map.change (Aspect.entity .<- entityId) aspectId
        remove >> add |> over entities >> (setParent |> over aspects)

    let internal removeAspect id =
        over entities (Map.map <| fun _ -> List.remove id |> over Entity.aspects) >>
        over aspects (Map.remove id)

    // Entities

    let internal addEntity id =
        let entity =
            { Id = id
              Name = ""
              StatGroups = []
              Aspects = []
              Collapsed = false }
        over entities (Map.add id entity) >>
        over order (fun order -> order @ [ id ])

    let internal setEntityName name = Map.change (Entity.name .<- name) >> over entities

    let internal setEntityCollapsed collapsed = Map.change (Entity.collapsed .<- collapsed) >> over entities

    let internal moveEntity index id = List.remove id >> List.insertAt index id |> over order

    let internal removeEntity id board =
        match Map.tryFind id board.Entities with
        | Some entity ->
            board
            |> over order (List.remove id)
            |> over entities (Map.remove id)
            |> flip (List.fold <| flip removeStatGroup) entity.StatGroups
            |> flip (List.fold <| flip removeAspect) entity.Aspects
        | None -> board

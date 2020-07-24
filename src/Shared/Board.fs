namespace Destiny.Shared.Board

open Destiny.Shared
open Destiny.Shared.Bag
open Destiny.Shared.Collections
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
      Sequence : Entity Id list }

module internal Board =
    let private entities = { Get = (fun s -> s.Entities); Set = fun v s -> { s with Entities = v } }

    let private statGroups = { Get = (fun s -> s.StatGroups); Set = fun v s -> { s with StatGroups = v } }

    let private stats = { Get = (fun s -> s.Stats); Set = fun v s -> { s with Stats = v } }

    let private aspects = { Get = (fun s -> s.Aspects); Set = fun v s -> { s with Aspects = v } }

    let private sequence = { Get = (fun s -> s.Sequence); Set = fun v s -> { s with Sequence = v } }

    let private flip f x y = f y x

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
        Lens.update statGroups (Map.change (List.add statId |> Lens.update StatGroup.stats) groupId) >>
        Lens.update stats (Map.add statId stat)

    let setStatName name = Map.change (Stat.name.Set name) >> Lens.update stats

    let setStatScore score = Map.change (Stat.score.Set score) >> Lens.update stats

    let removeStat id =
        Lens.update statGroups (Map.map <| fun _ -> Lens.update StatGroup.stats (List.remove id)) >>
        Lens.update stats (Map.remove id)

    // Stat Groups

    let addStatGroup groupId entityId =
        let group =
            { Id = groupId
              Name = ""
              Stats = [] }
        Lens.update entities (Map.change (List.add groupId |> Lens.update Entity.statGroups) entityId) >>
        Lens.update statGroups (Map.add groupId group)

    let setStatGroupName name = Map.change (StatGroup.name.Set name) >> Lens.update statGroups

    let removeStatGroup id board =
        match Map.tryFind id board.StatGroups with
        | Some group ->
            board
            |> Lens.update entities (Map.map <| fun _ -> List.remove id |> Lens.update Entity.statGroups)
            |> Lens.update statGroups (Map.remove id)
            |> flip (List.fold <| flip removeStat) group.Stats
        | None -> board

    // Aspects

    let addAspect aspectId entityId =
        let aspect =
            { Id = aspectId
              Description = ""
              Dice = Bag.empty }
        Lens.update entities (Map.change (List.add aspectId |> Lens.update Entity.aspects) entityId) >>
        Lens.update aspects (Map.add aspectId aspect)

    let setAspectDescription description = Map.change (Aspect.description.Set description) >> Lens.update aspects

    let addDie die = Map.change (Bag.add die |> Lens.update Aspect.dice) >> Lens.update aspects

    let removeDie die = Map.change (Bag.remove die |> Lens.update Aspect.dice) >> Lens.update aspects

    let moveAspect aspectId entityId index =
        Map.map (fun _ -> List.remove aspectId |> Lens.update Entity.aspects)
        >> Map.change (List.insertAt index aspectId |> Lens.update Entity.aspects) entityId
        |> Lens.update entities

    let removeAspect id =
        Lens.update entities (Map.map <| fun _ -> List.remove id |> Lens.update Entity.aspects) >>
        Lens.update aspects (Map.remove id)

    // Entities

    let addEntity id =
        let entity =
            { Id = id
              Name = ""
              StatGroups = []
              Aspects = []
              Collapsed = false }
        Lens.update entities (Map.add id entity) >>
        Lens.update sequence (fun sequence -> sequence @ [ id ])

    let setEntityName name = Map.change (Entity.name.Set name) >> Lens.update entities

    let collapseEntity = Map.change (Lens.update Entity.collapsed not) >> Lens.update entities

    let moveEntity index id = List.remove id >> List.insertAt index id |> Lens.update sequence

    let removeEntity id board =
        match Map.tryFind id board.Entities with
        | Some entity ->
            board
            |> Lens.update sequence (List.remove id)
            |> Lens.update entities (Map.remove id)
            |> flip (List.fold <| flip removeStatGroup) entity.StatGroups
            |> flip (List.fold <| flip removeAspect) entity.Aspects
        | None -> board

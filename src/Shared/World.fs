namespace Destiny.Shared.World

open Destiny.Shared
open Destiny.Shared.Bag
open Destiny.Shared.Collections
open Destiny.Shared.Functions
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
      Collapsed : bool
      Saved : bool }

type Catalog =
    { Entities : Map<Entity Id, Entity>
      StatGroups : Map<StatGroup Id, StatGroup>
      Stats : Map<Stat Id, Stat>
      Aspects : Map<Aspect Id, Aspect> }

type Board =
    { Id : Board Id
      Name : string
      Entities : Entity Id list }

type World =
    { Catalog : Catalog
      Boards : Map<Board Id, Board>
      BoardList : Board Id list }

module private Stat =
    let name = { Get = (fun (s : Stat) -> s.Name); Set = fun v s -> { s with Name = v } }

    let score = { Get = (fun s -> s.Score); Set = fun v s -> { s with Score = v } }

module private StatGroup =
    let name = { Get = (fun (s : StatGroup) -> s.Name); Set = fun v s -> { s with Name = v } }

    let stats = { Get = (fun (s : StatGroup) -> s.Stats); Set = fun v s -> { s with Stats = v } }

module private Aspect =
    let entity = { Get = (fun s -> s.Entity); Set = fun v s -> { s with Entity = v } }

    let description = { Get = (fun s -> s.Description); Set = fun v s -> { s with Description = v } }

    let dice = { Get = (fun s -> s.Dice); Set = fun v s -> { s with Dice = v } }

module private Entity =
    let name = { Get = (fun (s : Entity) -> s.Name); Set = fun v s -> { s with Name = v } }

    let statGroups = { Get = (fun (s : Entity) -> s.StatGroups); Set = fun v s -> { s with StatGroups = v } }

    let aspects = { Get = (fun (s : Entity) -> s.Aspects); Set = fun v s -> { s with Aspects = v } }

    let collapsed = { Get = (fun s -> s.Collapsed); Set = fun v s -> { s with Collapsed = v } }

    let saved = { Get = (fun s -> s.Saved); Set = fun v s -> { s with Saved = v } }

module private Catalog =
    let entities = { Get = (fun (s : Catalog) -> s.Entities); Set = fun v s -> { s with Entities = v } }

    let statGroups = { Get = (fun s -> s.StatGroups); Set = fun v s -> { s with StatGroups = v } }

    let stats = { Get = (fun s -> s.Stats); Set = fun v s -> { s with Stats = v } }

    let aspects = { Get = (fun s -> s.Aspects); Set = fun v s -> { s with Aspects = v } }

    let empty =
        { Entities = Map.empty
          Aspects = Map.empty
          Stats = Map.empty
          StatGroups = Map.empty }

module private Board =
    let internal name = { Get = (fun s -> s.Name); Set = fun v s -> { s with Name = v } }

    let internal entities = { Get = (fun s -> s.Entities); Set = fun v s -> { s with Entities = v } }

module World =
    let private catalog = { Get = (fun s -> s.Catalog); Set = fun v s -> { s with Catalog = v } }

    let private boards = { Get = (fun s -> s.Boards); Set = fun v s -> { s with Boards = v } }

    let private boardList = { Get = (fun s -> s.BoardList); Set = fun v s -> { s with BoardList = v } }

    let private entities = catalog .>> Catalog.entities

    let private statGroups = catalog .>> Catalog.statGroups

    let private stats = catalog .>> Catalog.stats

    let private aspects = catalog .>> Catalog.aspects

    let empty =
        let home =
            { Id = Id.parse "00000000-0000-0000-0000-000000000000"
              Name = "Home"
              Entities = [] }
        { Catalog = Catalog.empty
          Boards = Map.ofList [ home.Id, home ]
          BoardList = [ home.Id ] }

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

    let internal removeStatGroup id world =
        match Map.tryFind id world.Catalog.StatGroups with
        | Some group ->
            world
            |> over entities (Map.map <| fun _ -> List.remove id |> over Entity.statGroups)
            |> over statGroups (Map.remove id)
            |> flip (List.fold <| flip removeStat) group.Stats
        | None -> world

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

    let internal addEntity entityId boardId =
        let entity =
            { Id = entityId
              Name = ""
              StatGroups = []
              Aspects = []
              Collapsed = false
              Saved = false }
        over entities (Map.addIfNew entityId entity) >>
        over boards (Map.change (List.addIfNew entityId |> over Board.entities) boardId)

    let internal setEntityName name = Map.change (Entity.name .<- name) >> over entities

    let internal setEntityCollapsed collapsed = Map.change (Entity.collapsed .<- collapsed) >> over entities

    let internal setEntitySaved saved = Map.change (Entity.saved .<- saved) >> over entities

    let internal moveEntity index entityId boardId =
        Map.change (List.remove entityId >> List.insertAt index entityId |> over Board.entities) boardId
        |> over boards

    let internal removeEntity entityId boardId world =
        match Map.tryFind entityId world.Catalog.Entities with
        | Some entity ->
            if entity.Saved
            then world |> over boards (Map.change (List.remove entityId |> over Board.entities) boardId)
            else
                world
                |> over boards (Map.map (fun _ -> List.remove entityId |> over Board.entities))
                |> over entities (Map.remove entityId)
                |> flip (flip removeStatGroup |> List.fold) entity.StatGroups
                |> flip (flip removeAspect |> List.fold) entity.Aspects
        | None -> world

    // Boards

    let internal addBoard id =
        let board =
            { Id = id
              Name = ""
              Entities = [] }
        over boards (Map.add board.Id board)
        >> over boardList (List.add board.Id)

    let internal setBoardName name = Map.change (Board.name .<- name) >> over boards

    let internal removeBoard id world =
        match Map.tryFind id world.Boards with
        | Some board ->
            world
            |> over boards (Map.remove id)
            |> over boardList (List.remove id)
            |> flip (flip removeEntity id |> flip |> List.fold) board.Entities
        | None -> world

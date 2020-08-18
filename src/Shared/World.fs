namespace Destiny.Shared.World

open Destiny.Shared
open Destiny.Shared.Bag
open Destiny.Shared.Collections
open Destiny.Shared.Functions
open Destiny.Shared.Lens
open Destiny.Shared.Profile

type Die = { Role : Role }

type Stat =
    { Id : Stat Id
      Hidden : bool
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
      Hidden : bool
      Description : string
      Dice : Die Bag }

and Entity =
    { Id : Entity Id
      Name : string
      User : Username
      Hidden : bool
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

module Stat =
    let name = lens (fun (s : Stat) -> s.Name) (fun v s -> { s with Name = v })

    let hidden = lens (fun (s : Stat) -> s.Hidden) (fun v s -> { s with Hidden = v })

    let score = lens (fun s -> s.Score) (fun v s -> { s with Score = v })

module StatGroup =
    let name = lens (fun (s : StatGroup) -> s.Name) (fun v s -> { s with Name = v })

    let stats = lens (fun (s : StatGroup) -> s.Stats) (fun v s -> { s with Stats = v })

module Aspect =
    let entity = lens (fun s -> s.Entity) (fun v s -> { s with Entity = v })

    let hidden = lens (fun (s : Aspect) -> s.Hidden) (fun v s -> { s with Hidden = v })

    let description = lens (fun s -> s.Description) (fun v s -> { s with Description = v })

    let dice = lens (fun s -> s.Dice) (fun v s -> { s with Dice = v })

module Entity =
    let name = lens (fun (s : Entity) -> s.Name) (fun v s -> { s with Name = v })

    let hidden = lens (fun s -> s.Hidden) (fun v s -> { s with Hidden = v })

    let statGroups = lens (fun (s : Entity) -> s.StatGroups) (fun v s -> { s with StatGroups = v })

    let aspects = lens (fun (s : Entity) -> s.Aspects) (fun v s -> { s with Aspects = v })

    let collapsed = lens (fun s -> s.Collapsed) (fun v s -> { s with Collapsed = v })

    let saved = lens (fun s -> s.Saved) (fun v s -> { s with Saved = v })

module Catalog =
    let entities = lens (fun (s : Catalog) -> s.Entities) (fun v s -> { s with Entities = v })

    let statGroups = lens (fun s -> s.StatGroups) (fun v s -> { s with StatGroups = v })

    let stats = lens (fun s -> s.Stats) (fun v s -> { s with Stats = v })

    let aspects = lens (fun s -> s.Aspects) (fun v s -> { s with Aspects = v })

    let internal empty =
        { Entities = Map.empty
          Aspects = Map.empty
          Stats = Map.empty
          StatGroups = Map.empty }

    let statGroupEntity catalog groupId =
        Map.tryFind groupId catalog.StatGroups
        |> Option.bind (fun group -> Map.tryFind group.Entity catalog.Entities)

    let statEntity catalog statId =
        Map.tryFind statId catalog.Stats
        |> Option.bind (fun stat -> statGroupEntity catalog stat.Group)

    let aspectEntity catalog aspectId =
        Map.tryFind aspectId catalog.Aspects
        |> Option.bind (fun aspect -> Map.tryFind aspect.Entity catalog.Entities)

    let isEntityOwner (catalog : Catalog) username entityId =
        Map.tryFind entityId catalog.Entities
        |> Option.exists (fun entity -> entity.User = username)

    let isAspectOwner catalog username aspectId =
        aspectEntity catalog aspectId
        |> Option.exists (fun entity -> entity.User = username)

    let isStatGroupOwner catalog username groupId =
        statGroupEntity catalog groupId
        |> Option.exists (fun entity -> entity.User = username)

    let isStatOwner catalog username statId =
        statEntity catalog statId
        |> Option.exists (fun entity -> entity.User = username)

module Board =
    let name = lens (fun s -> s.Name) (fun v s -> { s with Name = v })

    let entities = lens (fun s -> s.Entities) (fun v s -> { s with Entities = v })

module World =
    let catalog = lens (fun s -> s.Catalog) (fun v s -> { s with Catalog = v })

    let boards = lens (fun s -> s.Boards) (fun v s -> { s with Boards = v })

    let boardList = lens (fun s -> s.BoardList) (fun v s -> { s with BoardList = v })

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

    let internal revealStat statId stat = Map.add statId stat |> over stats

    let internal obscureStat = Map.remove >> over stats

    let internal addStatPlaceholder statId groupId =
        Map.change (List.add statId |> over StatGroup.stats) groupId |> over statGroups

    let internal addStat statId groupId hidden =
        let stat =
            { Id = statId
              Hidden = hidden
              Group = groupId
              Name = ""
              Score = 0 }
        addStatPlaceholder statId groupId >> revealStat statId stat

    let internal setStatHidden hidden = Map.change (Stat.hidden .<- hidden) >> over stats

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

    let internal revealAspect aspectId aspect = Map.add aspectId aspect |> over aspects

    let internal obscureAspect = Map.remove >> over aspects

    let internal addAspectPlaceholder aspectId entityId =
        Map.change (List.add aspectId |> over Entity.aspects) entityId |> over entities

    let internal addAspect aspectId entityId hidden =
        let aspect =
            { Id = aspectId
              Entity = entityId
              Hidden = hidden
              Description = ""
              Dice = Bag.empty }
        addAspectPlaceholder aspectId entityId >> revealAspect aspectId aspect

    let internal setAspectHidden hidden = Map.change (Aspect.hidden .<- hidden) >> over aspects

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

    let internal linkEntity entityId boardId =
        over boards (Map.change (List.addIfNew entityId |> over Board.entities) boardId)

    let internal addEntity entityId boardId user =
        let entity =
            { Id = entityId
              Name = ""
              User = user
              Hidden = false
              StatGroups = []
              Aspects = []
              Collapsed = false
              Saved = false }
        over entities (Map.addIfNew entityId entity)
        >> linkEntity entityId boardId

    let internal setEntityHidden hidden =
        Map.change (Entity.hidden .<- hidden) >> over entities

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

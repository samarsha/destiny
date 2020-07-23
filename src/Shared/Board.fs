namespace Destiny.Shared.Board

open System

type 'a Id = private Id of Guid

type Stat =
    { Id : Stat Id
      Name : string
      Score : int }

type StatGroup =
    { Id : StatGroup Id
      Name : string
      Stats : Stat Id list }

type Role =
    | Player
    | DM

type Die = Die of Role

type Aspect =
    { Id : Aspect Id
      Description : string
      Dice : Die list }

type Entity =
    { Id : Entity Id
      Name : string
      StatGroups : StatGroup Id list
      Aspects : Aspect Id list
      Collapsed : bool }

type Board =
    { Entities : Map<Entity Id, Entity>
      StatGroups : Map<StatGroup Id, Stat>
      Stats : Map<Stat Id, Stat>
      Aspects : Map<Aspect Id, Aspect>
      Sequence : Entity Id list }

module internal Board =
    let empty =
        { Entities = Map.empty
          Aspects = Map.empty
          Stats = Map.empty
          StatGroups = Map.empty
          Sequence = [] }

    let randomId () = Id <| Guid.NewGuid ()

    let addEntity id board =
        let entity =
            { Id = id
              Name = ""
              StatGroups = []
              Aspects = []
              Collapsed = false }
        { board with Entities = Map.add id entity board.Entities
                     Sequence = board.Sequence @ [ id ] }

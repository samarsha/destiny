namespace Destiny.Shared.Scene

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

type Scene =
    { Entities : Map<Entity Id, Entity>
      StatGroups : Map<StatGroup Id, Stat>
      Stats : Map<Stat Id, Stat>
      Aspects : Map<Aspect Id, Aspect>
      View : Entity Id list }

module internal Scene =
    let empty =
        { Entities = Map.empty
          Aspects = Map.empty
          Stats = Map.empty
          StatGroups = Map.empty
          View = [] }

    let randomId () = Id <| Guid.NewGuid ()

    let addEntity id scene =
        let entity =
            { Id = id
              Name = ""
              StatGroups = []
              Aspects = []
              Collapsed = false }
        { scene with Entities = Map.add id entity scene.Entities
                     View = scene.View @ [ id ] }

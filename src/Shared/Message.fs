namespace Destiny.Shared.Message

open Destiny.Shared
open Destiny.Shared.Board
open Destiny.Shared.World

type BoardCommand =
    | AddEntity of Entity Id
    | CollapseEntity of Entity Id
    | SetEntityName of Entity Id * string
    | MoveEntity of Entity Id * int
    | RemoveEntity of Entity Id
    | AddStatGroup of StatGroup Id * Entity Id
    | SetStatGroupName of StatGroup Id * string
    | RemoveStatGroup of StatGroup Id
    | AddStat of Stat Id * StatGroup Id
    | SetStatName of Stat Id * string
    | SetStatScore of Stat Id * int
    | RemoveStat of Stat Id
    | AddAspect of Aspect Id * Entity Id
    | SetAspectDescription of Aspect Id * string
    | MoveAspect of Aspect Id * Entity Id * int
    | RemoveAspect of Aspect Id
    | AddDie of Aspect Id * Die
    | RemoveDie of Aspect Id * Die

module internal BoardCommand =
    let update = function
        | AddEntity id -> Board.addEntity id
        | CollapseEntity id -> Board.collapseEntity id
        | SetEntityName (id, name) -> Board.setEntityName name id
        | MoveEntity (id, index) -> Board.moveEntity index id
        | RemoveEntity id -> Board.removeEntity id
        | AddStatGroup (groupId, entityId) -> Board.addStatGroup groupId entityId
        | SetStatGroupName (id, name) -> Board.setStatGroupName name id
        | RemoveStatGroup id -> Board.removeStatGroup id
        | AddStat (statId, groupId) -> Board.addStat statId groupId
        | SetStatName (id, name) -> Board.setStatName name id
        | SetStatScore (id, score) -> Board.setStatScore score id
        | RemoveStat id -> Board.removeStat id
        | AddAspect (aspectId, entityId) -> Board.addAspect aspectId entityId
        | SetAspectDescription (id, description) -> Board.setAspectDescription description id
        | MoveAspect (aspectId, entityId, index) -> Board.moveAspect aspectId entityId index
        | RemoveAspect id -> Board.removeAspect id
        | AddDie (id, die) -> Board.addDie die id
        | RemoveDie (id, die) -> Board.removeDie die id

type BoardMessage =
    { Id : BoardMessage Id
      Command : BoardCommand }

module internal BoardMessage =
    let create command =
        { Id = Id.random ()
          Command = command }

type ServerMessage =
    | ClientConnected of Board * RollLog
    | BoardUpdated of BoardMessage
    | BoardReplaced of Board
    | RollLogUpdated of RollLog
    | RoleChanged of Role

type ClientMessage =
    | UpdateBoard of BoardMessage
    | RollStat of Stat Id * Roll Id
    | RollAspect of Aspect Id * Roll Id
    | Undo
    | Redo
    | SetRole of Role

module internal Message =
    let socket = "/socket"
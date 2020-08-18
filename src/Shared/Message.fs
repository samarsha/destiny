namespace Destiny.Shared.Message

open Destiny.Shared
open Destiny.Shared.Profile
open Destiny.Shared.Roll
open Destiny.Shared.World

type WorldCommand =
    | AddBoard of Board Id
    | SetBoardName of Board Id * string
    | RemoveBoard of Board Id
    | AddEntity of Entity Id * Board Id * Username
    | LinkEntity of Entity Id * Board Id
    | SetEntityHidden of Entity Id * bool
    | SetEntityName of Entity Id * string
    | SetEntityCollapsed of Entity Id * bool
    | SetEntitySaved of Entity Id * bool
    | MoveEntity of Entity Id * Board Id * int
    | RemoveEntity of Entity Id * Board Id
    | AddStatGroup of StatGroup Id * Entity Id
    | SetStatGroupName of StatGroup Id * string
    | RemoveStatGroup of StatGroup Id
    | AddStat of Stat Id * StatGroup Id * bool
    | AddStatPlaceholder of Stat Id * StatGroup Id
    | SetStatHidden of Stat Id * bool
    | SetStatName of Stat Id * string
    | SetStatScore of Stat Id * int
    | RevealStat of Stat Id * Stat
    | ObscureStat of Stat Id
    | RemoveStat of Stat Id
    | AddAspect of Aspect Id * Entity Id * bool
    | AddAspectPlaceholder of Aspect Id * Entity Id
    | SetAspectHidden of Aspect Id * bool
    | SetAspectDescription of Aspect Id * string
    | MoveAspect of Aspect Id * Entity Id * int
    | RevealAspect of Aspect Id * Aspect
    | ObscureAspect of Aspect Id
    | RemoveAspect of Aspect Id
    | AddDie of Aspect Id * Die
    | RemoveDie of Aspect Id * Die

module WorldCommand =
    let update = function
        | AddBoard id -> World.addBoard id
        | SetBoardName (id, name) -> World.setBoardName name id
        | RemoveBoard id -> World.removeBoard id
        | AddEntity (entityId, boardId, user) -> World.addEntity entityId boardId user
        | LinkEntity (entityId, boardId) -> World.linkEntity entityId boardId
        | SetEntityHidden (entityId, hidden) -> World.setEntityHidden hidden entityId
        | SetEntityName (id, name) -> World.setEntityName name id
        | SetEntityCollapsed (id, collapsed) -> World.setEntityCollapsed collapsed id
        | SetEntitySaved (id, saved) -> World.setEntitySaved saved id
        | MoveEntity (entityId, boardId, index) -> World.moveEntity index entityId boardId
        | RemoveEntity (entityId, boardId) -> World.removeEntity entityId boardId
        | AddStatGroup (groupId, entityId) -> World.addStatGroup groupId entityId
        | SetStatGroupName (id, name) -> World.setStatGroupName name id
        | RemoveStatGroup id -> World.removeStatGroup id
        | AddStat (statId, groupId, hidden) -> World.addStat statId groupId hidden
        | AddStatPlaceholder (statId, groupId) -> World.addStatPlaceholder statId groupId
        | SetStatHidden (statId, hidden) -> World.setStatHidden hidden statId
        | SetStatName (id, name) -> World.setStatName name id
        | SetStatScore (id, score) -> World.setStatScore score id
        | RevealStat (statId, stat) -> World.revealStat statId stat
        | ObscureStat statId -> World.obscureStat statId
        | RemoveStat id -> World.removeStat id
        | AddAspect (aspectId, entityId, hidden) -> World.addAspect aspectId entityId hidden
        | AddAspectPlaceholder (aspectId, entityId) -> World.addAspectPlaceholder aspectId entityId
        | SetAspectHidden (aspectId, hidden) -> World.setAspectHidden hidden aspectId
        | RevealAspect (aspectId, aspect) -> World.revealAspect aspectId aspect
        | ObscureAspect aspectId -> World.obscureAspect aspectId
        | SetAspectDescription (id, description) -> World.setAspectDescription description id
        | MoveAspect (aspectId, entityId, index) -> World.moveAspect aspectId entityId index
        | RemoveAspect id -> World.removeAspect id
        | AddDie (id, die) -> World.addDie die id
        | RemoveDie (id, die) -> World.removeDie die id

type WorldMessage =
    { Id : WorldMessage Id
      Command : WorldCommand }

module WorldMessage =
    let create command =
        { Id = Id.random ()
          Command = command }

type ServerMessage =
    | ClientConnected of World * RollLog
    | LoginResult of Result<Profile, string>
    | WorldUpdated of WorldMessage
    | WorldReplaced of World
    | RollLogUpdated of RollLog

type ClientMessage =
    | SignUp of Username * Password
    | LogIn of Username * Password
    | UpdateWorld of WorldMessage
    | RollStat of Stat Id * Roll Id * Die
    | RollAspect of Aspect Id * Roll Id * Die
    | RollSpare of Roll Id * Die
    | Undo
    | Redo
    | ClientNoOp

module Message =
    let socket = "/socket"

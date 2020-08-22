namespace Destiny.Shared

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
    | WorldIdentity

module WorldCommand =
    let update = function
        | AddBoard boardId -> World.addBoard boardId
        | SetBoardName (boardId, name) -> World.setBoardName name boardId
        | RemoveBoard boardId -> World.removeBoard boardId
        | AddEntity (entityId, boardId, user) -> World.addEntity entityId boardId user
        | LinkEntity (entityId, boardId) -> World.linkEntity entityId boardId
        | SetEntityHidden (entityId, hidden) -> World.setEntityHidden hidden entityId
        | SetEntityName (entityId, name) -> World.setEntityName name entityId
        | SetEntityCollapsed (entityId, collapsed) -> World.setEntityCollapsed collapsed entityId
        | SetEntitySaved (entityId, saved) -> World.setEntitySaved saved entityId
        | MoveEntity (entityId, boardId, index) -> World.moveEntity index entityId boardId
        | RemoveEntity (entityId, boardId) -> World.removeEntity entityId boardId
        | AddStatGroup (groupId, entityId) -> World.addStatGroup groupId entityId
        | SetStatGroupName (groupId, name) -> World.setStatGroupName name groupId
        | RemoveStatGroup groupId -> World.removeStatGroup groupId
        | AddStat (statId, groupId, hidden) -> World.addStat statId groupId hidden
        | AddStatPlaceholder (statId, groupId) -> World.addStatPlaceholder statId groupId
        | SetStatHidden (statId, hidden) -> World.setStatHidden hidden statId
        | SetStatName (statId, name) -> World.setStatName name statId
        | SetStatScore (statId, score) -> World.setStatScore score statId
        | RevealStat (statId, stat) -> World.revealStat statId stat
        | ObscureStat statId -> World.obscureStat statId
        | RemoveStat statId -> World.removeStat statId
        | AddAspect (aspectId, entityId, hidden) -> World.addAspect aspectId entityId hidden
        | AddAspectPlaceholder (aspectId, entityId) -> World.addAspectPlaceholder aspectId entityId
        | SetAspectHidden (aspectId, hidden) -> World.setAspectHidden hidden aspectId
        | RevealAspect (aspectId, aspect) -> World.revealAspect aspectId aspect
        | ObscureAspect aspectId -> World.obscureAspect aspectId
        | SetAspectDescription (id, description) -> World.setAspectDescription description id
        | MoveAspect (aspectId, entityId, index) -> World.moveAspect aspectId entityId index
        | RemoveAspect aspectId -> World.removeAspect aspectId
        | AddDie (aspectId, die) -> World.addDie die aspectId
        | RemoveDie (aspectId, die) -> World.removeDie die aspectId
        | WorldIdentity -> id

type WorldMessage =
    { Id : WorldMessage Id
      Command : WorldCommand }

module WorldMessage =
    let create command =
        { Id = Id.random ()
          Command = command }

type ServerMessage =
    | ClientConnected of World * RollLog
    | LoginResult of Result<Session, string>
    | WorldUpdated of WorldMessage
    | WorldReplaced of World
    | RollLogUpdated of RollLog

type ClientMessage =
    | SignUp of Username * Password
    | LogIn of Username * Password
    | RestoreSession of Session Id
    | UpdateWorld of WorldMessage
    | RollStat of Stat Id * Roll Id * Die
    | RollAspect of Aspect Id * Roll Id * Die
    | RollSpare of Roll Id * Die
    | Undo
    | Redo
    | ClientIdentity

module Message =
    let socket = "/socket"

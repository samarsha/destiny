module internal Destiny.Server.Auth

open Destiny.Shared.Message
open Destiny.Shared.World

type AuthorizedMessage = private AuthorizedMessage of ClientMessage

let private authorizeDie role die = { Role = min role die.Role }

let private authorizeWorldCommand role = function
    | AddDie (id, die) -> AddDie (id, authorizeDie role die)
    | RemoveDie (id, die) -> RemoveDie (id, authorizeDie role die)
    | AddBoard _
    | SetBoardName _
    | RemoveBoard _
    | AddEntity _
    | SetEntityName _
    | SetEntityCollapsed _
    | SetEntitySaved _
    | MoveEntity _
    | RemoveEntity _
    | AddStatGroup _
    | SetStatGroupName _
    | RemoveStatGroup _
    | AddStat _
    | SetStatName _
    | SetStatScore _
    | RemoveStat _
    | AddAspect _
    | SetAspectDescription _
    | MoveAspect _
    | RemoveAspect _ as message -> message

let authorize role = function
    | UpdateWorld message ->
        { message with Command = authorizeWorldCommand role message.Command }
        |> UpdateWorld
        |> AuthorizedMessage
    | RollStat (statId, rollId, die) ->
        RollStat (statId, rollId, authorizeDie role die) |> AuthorizedMessage
    | RollAspect (aspectId, rollId, die) ->
        RollAspect (aspectId, rollId, authorizeDie role die) |> AuthorizedMessage
    | RollSpare (rollId, die) ->
        RollSpare (rollId, authorizeDie role die) |> AuthorizedMessage
    | SignUp _
    | LogIn _
    | Undo
    | Redo as message -> AuthorizedMessage message

let peek (AuthorizedMessage message) = message

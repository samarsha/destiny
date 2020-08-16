namespace Destiny.Server.Auth

open Destiny.Server.User
open Destiny.Shared.Message
open Destiny.Shared.Profile
open Destiny.Shared.World

type internal Client =
    | Guest
    | Profile of Profile Token

type internal AuthorizedClientMessage = private AuthorizedClientMessage of ClientMessage

module internal Auth =
    let private authorizeDie (token : Profile Token) die =
        { Role = min (Token.profile token).Role die.Role }

    let private authorizeWorldCommand token = function
        | AddDie (id, die) -> AddDie (id, authorizeDie token die)
        | RemoveDie (id, die) -> RemoveDie (id, authorizeDie token die)
        | AddEntity (entityId, boardId, _) -> AddEntity (entityId, boardId, (Token.profile token).Username)
        | AddBoard _
        | SetBoardName _
        | RemoveBoard _
        | LinkEntity _
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

    let authorizeClient client message =
        match client, message with
        | Profile token, UpdateWorld message ->
            { message with Command = authorizeWorldCommand token message.Command }
            |> UpdateWorld
            |> AuthorizedClientMessage
        | Profile token, RollStat (statId, rollId, die) ->
            RollStat (statId, rollId, authorizeDie token die) |> AuthorizedClientMessage
        | Profile token, RollAspect (aspectId, rollId, die) ->
            RollAspect (aspectId, rollId, authorizeDie token die) |> AuthorizedClientMessage
        | Profile token, RollSpare (rollId, die) ->
            RollSpare (rollId, authorizeDie token die) |> AuthorizedClientMessage
        | Profile _, Undo
        | Profile _, Redo
        | Guest, SignUp _
        | Guest, LogIn _ as request -> snd request |> AuthorizedClientMessage
        | Profile _, SignUp _
        | Profile _, LogIn _
        | Profile _, ClientNoOp
        | Guest, _ -> AuthorizedClientMessage ClientNoOp

    let clientMessage (AuthorizedClientMessage message) = message

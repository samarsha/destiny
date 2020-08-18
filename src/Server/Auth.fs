namespace Destiny.Server.Auth

open Destiny.Server.User
open Destiny.Shared.Lens
open Destiny.Shared.Logic
open Destiny.Shared.Message
open Destiny.Shared.Profile
open Destiny.Shared.World

type internal Client =
    | Guest
    | Profile of Profile Token

type internal AuthorizedClientMessage =
    private AuthorizedClientMessage of ClientMessage

module internal Auth =
    let private isDieAuthorized (profile : Profile) die = profile.Role >= die.Role

    let private isAspectVisible (catalog : Catalog) profile aspectId =
        Map.tryFind aspectId catalog.Aspects |> Option.exists (view Aspect.hidden >> not) ||
        Catalog.aspectEntity catalog aspectId |> Option.exists (fun entity -> entity.User = profile.Username)

    let private isStatVisible (catalog : Catalog) profile statId =
        Map.tryFind statId catalog.Stats |> Option.exists (view Stat.hidden >> not) ||
        Catalog.statEntity catalog statId |> Option.exists (fun entity -> entity.User = profile.Username)

    let private authorizedClientCommand catalog profile = function
        | AddDie (aspectId, die)
        | RemoveDie (aspectId, die) ->
            isAspectVisible catalog profile aspectId && isDieAuthorized profile die
        | AddEntity (_, _, username) ->
            profile.Username = username
        | SetEntityHidden (entityId, _) ->
            Catalog.isEntityOwner catalog profile.Username entityId
        | AddStat (_, groupId, hidden) ->
            hidden => lazy Catalog.isStatGroupOwner catalog profile.Username groupId
        | SetStatHidden (statId, _) ->
            Catalog.isStatOwner catalog profile.Username statId
        | SetStatName (statId, _)
        | SetStatScore (statId, _)
        | RemoveStat statId ->
            isStatVisible catalog profile statId
        | AddAspect (_, entityId, hidden) ->
            hidden => lazy Catalog.isEntityOwner catalog profile.Username entityId
        | SetAspectHidden (aspectId, _) ->
            Catalog.isAspectOwner catalog profile.Username aspectId
        | SetAspectDescription (aspectId, _)
        | RemoveAspect aspectId ->
            isAspectVisible catalog profile aspectId
        | ObscureStat _
        | ObscureAspect _ ->
            false
        | _ ->
            true

    let private authorizeClientIf condition message =
        if condition
        then AuthorizedClientMessage message
        else AuthorizedClientMessage ClientNoOp

    let authorizeClient catalog client message =
        match client, message with
        | Profile token, UpdateWorld worldMessage ->
            message |> authorizeClientIf
                (worldMessage.Command |> authorizedClientCommand catalog (Token.profile token))
        | Profile token, RollStat (statId, _, die) ->
            message |> authorizeClientIf
                (isDieAuthorized (Token.profile token) die &&
                 isStatVisible catalog (Token.profile token) statId)
        | Profile token, RollAspect (aspectId, _, die) ->
            message |> authorizeClientIf
                (isDieAuthorized (Token.profile token) die &&
                 isAspectVisible catalog (Token.profile token) aspectId)
        | Profile token, RollSpare (_, die) ->
            message |> authorizeClientIf (isDieAuthorized (Token.profile token) die)
        | Profile _, Undo
        | Profile _, Redo
        | Guest, SignUp _
        | Guest, LogIn _ as request -> snd request |> AuthorizedClientMessage
        | Profile _, SignUp _
        | Profile _, LogIn _
        | Profile _, ClientNoOp
        | Guest, _ -> AuthorizedClientMessage ClientNoOp

    let clientMessage (AuthorizedClientMessage message) = message

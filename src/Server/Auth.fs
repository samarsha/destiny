namespace Destiny.Server.Auth

open Destiny.Server.User
open Destiny.Shared.Collections
open Destiny.Shared.Lens
open Destiny.Shared.Logic
open Destiny.Shared.Message
open Destiny.Shared.Profile
open Destiny.Shared.World

type internal Client =
    | Guest
    | Profile of Profile Token

type internal AuthorizedClientMessage = private AuthorizedClientMessage of ClientMessage

type internal AuthorizedServerMessage = private AuthorizedServerMessage of ServerMessage

module internal Auth =
    let private isDieAuthorized (profile : Profile) die = profile.Role >= die.Role

    let private isAspectVisible (catalog : Catalog) profile aspectId =
        Map.tryFind aspectId catalog.Aspects |> Option.exists (view Aspect.hidden >> not) ||
        Catalog.aspectEntity catalog aspectId |> Option.exists (fun entity -> entity.User = profile.Username)

    let private isStatVisible (catalog : Catalog) profile statId =
        Map.tryFind statId catalog.Stats |> Option.exists (view Stat.hidden >> not) ||
        Catalog.statEntity catalog statId |> Option.exists (fun entity -> entity.User = profile.Username)

    let private profileExists predicate = function
        | Guest -> false
        | Profile token -> Token.profile token |> predicate

    let private isAuthorizedClientCommand catalog profile = function
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
        | AddStatPlaceholder _
        | AddAspectPlaceholder _
        | RevealStat _
        | RevealAspect _
        | ObscureStat _
        | ObscureAspect _ ->
            false
        | _ ->
            true

    let private authorizeServerCommand catalog client command =
        match command with
        | AddStat (statId, groupId, hidden) ->
            if hidden &&
               client |> profileExists (fun profile -> Catalog.isStatGroupOwner catalog profile.Username groupId |> not)
            then AddStatPlaceholder (statId, groupId)
            else command
        | SetStatHidden (statId, hidden) ->
            if client |> profileExists (fun profile -> Catalog.isStatOwner catalog profile.Username statId)
            then command
            elif hidden
            then ObscureStat statId
            else Map.tryFind statId catalog.Stats |> Option.unwrap WorldIdentity (fun stat -> RevealStat (statId, stat))
        | SetStatName (statId, _)
        | SetStatScore (statId, _) ->
            if client |> profileExists (fun profile -> isStatVisible catalog profile statId)
            then command
            else WorldIdentity
        | AddAspect (aspectId, entityId, hidden) ->
            if hidden &&
               client |> profileExists (fun profile -> Catalog.isEntityOwner catalog profile.Username entityId |> not)
            then AddAspectPlaceholder (aspectId, entityId)
            else command
        | SetAspectHidden (aspectId, hidden) ->
            if client |> profileExists (fun profile -> Catalog.isAspectOwner catalog profile.Username aspectId)
            then command
            elif hidden
            then ObscureAspect aspectId
            else Map.tryFind aspectId catalog.Aspects
                 |> Option.unwrap WorldIdentity (fun aspect -> RevealAspect (aspectId, aspect))
        | SetAspectDescription (aspectId, _)
        | AddDie (aspectId, _)
        | RemoveDie (aspectId, _) ->
            if client |> profileExists (fun profile -> isAspectVisible catalog profile aspectId)
            then command
            else WorldIdentity
        | RevealStat _
        | ObscureStat _
        | AddStatPlaceholder _
        | RevealAspect _
        | ObscureAspect _
        | AddAspectPlaceholder _ -> WorldIdentity
        | _ -> command

    let authorizeCatalog client catalog =
        catalog
        |> over Catalog.stats (Map.filter (fun statId _ ->
               client |> profileExists (fun profile -> Catalog.isStatOwner catalog profile.Username statId)))
        |> over Catalog.aspects (Map.filter (fun aspectId _ ->
               client |> profileExists (fun profile -> Catalog.isAspectOwner catalog profile.Username aspectId)))

    let private authorizeClientIf condition message =
        if condition
        then AuthorizedClientMessage message
        else AuthorizedClientMessage ClientIdentity

    let authorizeClient catalog client message =
        match client, message with
        | Profile token, UpdateWorld worldMessage ->
            message |> authorizeClientIf
                (worldMessage.Command |> isAuthorizedClientCommand catalog (Token.profile token))
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
        | Profile _, ClientIdentity
        | Guest, _ -> AuthorizedClientMessage ClientIdentity

    let authorizeServer catalog client = function
        | ClientConnected (world, rolls) ->
            ClientConnected (world |> over World.catalog (authorizeCatalog client), rolls) |> AuthorizedServerMessage
        | WorldReplaced world ->
            world |> over World.catalog (authorizeCatalog client) |> WorldReplaced |> AuthorizedServerMessage
        | WorldUpdated worldMessage ->
            let command = authorizeServerCommand catalog client worldMessage.Command
            WorldUpdated { worldMessage with Command = command } |> AuthorizedServerMessage
        | LoginResult _
        | RollLogUpdated _ as message -> AuthorizedServerMessage message

    let clientMessage (AuthorizedClientMessage message) = message

    let serverMessage (AuthorizedServerMessage message) = message

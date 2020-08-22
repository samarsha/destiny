namespace Destiny.Server

open Destiny.Shared
open Destiny.Shared.Lens
open Destiny.Shared.Logic

type internal Client =
    | Guest
    | Member of Session

type internal AuthorizedClientMessage = private AuthorizedClientMessage of ClientMessage

type internal AuthorizedServerMessage = private AuthorizedServerMessage of ServerMessage

module internal Auth =
    let private profileExists predicate = function
        | Guest -> false
        | Member session -> predicate session.Profile

    let private isDieAuthorized (die : Die) = profileExists (fun profile -> profile.Team >= die.Team)

    let private isAspectVisible catalog client aspectId =
        Map.tryFind aspectId catalog.Aspects |> Option.exists (view Aspect.hidden >> not) ||
        client |> profileExists (fun profile -> Catalog.isAspectOwner catalog profile aspectId)

    let private isStatVisible catalog client statId =
        Map.tryFind statId catalog.Stats |> Option.exists (view Stat.hidden >> not) ||
        client |> profileExists (fun profile -> Catalog.isStatOwner catalog profile statId)

    let private isAuthorizedClientCommand catalog client = function
        | AddDie (aspectId, die)
        | RemoveDie (aspectId, die) ->
            isAspectVisible catalog client aspectId && isDieAuthorized die client
        | AddEntity (_, _, username) ->
            client |> profileExists (fun profile -> profile.Username = username)
        | SetEntityHidden (entityId, _) ->
            client |> profileExists (fun profile -> Catalog.isEntityOwner catalog profile entityId)
        | AddStat (_, groupId, hidden) ->
            hidden =>
            lazy (client |> profileExists (fun profile -> Catalog.isStatGroupOwner catalog profile groupId))
        | SetStatHidden (statId, _) ->
            client |> profileExists (fun profile -> Catalog.isStatOwner catalog profile statId)
        | SetStatName (statId, _)
        | SetStatScore (statId, _)
        | RemoveStat statId ->
            isStatVisible catalog client statId
        | AddAspect (_, entityId, hidden) ->
            hidden =>
            lazy (client |> profileExists (fun profile -> Catalog.isEntityOwner catalog profile entityId))
        | SetAspectHidden (aspectId, _) ->
            client |> profileExists (fun profile -> Catalog.isAspectOwner catalog profile aspectId)
        | SetAspectDescription (aspectId, _)
        | RemoveAspect aspectId ->
            isAspectVisible catalog client aspectId
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
               client |> profileExists (fun profile -> Catalog.isStatGroupOwner catalog profile groupId) |> not
            then AddStatPlaceholder (statId, groupId)
            else command
        | SetStatHidden (statId, hidden) ->
            if client |> profileExists (fun profile -> Catalog.isStatOwner catalog profile statId)
            then command
            elif hidden
            then ObscureStat statId
            else
                Map.tryFind statId catalog.Stats
                |> Option.unwrap WorldIdentity (fun stat -> RevealStat (statId, stat))
        | SetStatName (statId, _)
        | SetStatScore (statId, _) ->
            if isStatVisible catalog client statId then command else WorldIdentity
        | AddAspect (aspectId, entityId, hidden) ->
            if hidden &&
               client |> profileExists (fun profile -> Catalog.isEntityOwner catalog profile entityId) |> not
            then AddAspectPlaceholder (aspectId, entityId)
            else command
        | SetAspectHidden (aspectId, hidden) ->
            if client |> profileExists (fun profile -> Catalog.isAspectOwner catalog profile aspectId)
            then command
            elif hidden
            then ObscureAspect aspectId
            else
                Map.tryFind aspectId catalog.Aspects
                |> Option.unwrap WorldIdentity (fun aspect -> RevealAspect (aspectId, aspect))
        | SetAspectDescription (aspectId, _)
        | AddDie (aspectId, _)
        | RemoveDie (aspectId, _) ->
            if isAspectVisible catalog client aspectId then command else WorldIdentity
        | RevealStat _
        | ObscureStat _
        | AddStatPlaceholder _
        | RevealAspect _
        | ObscureAspect _
        | AddAspectPlaceholder _ ->
            WorldIdentity
        | _ ->
            command

    let authorizeCatalog client catalog =
        catalog
        |> over Catalog.stats (Map.filter (fun statId _ -> isStatVisible catalog client statId))
        |> over Catalog.aspects (Map.filter (fun aspectId _ -> isAspectVisible catalog client aspectId))

    let private authorizeClientIf condition message =
        if condition
        then AuthorizedClientMessage message
        else AuthorizedClientMessage ClientIdentity

    let authorizeClient catalog client message =
        match client, message with
        | Member _, UpdateWorld worldMessage ->
            message |> authorizeClientIf
                (worldMessage.Command |> isAuthorizedClientCommand catalog client)
        | Member _, RollStat (statId, _, die) ->
            message |> authorizeClientIf (isDieAuthorized die client && isStatVisible catalog client statId)
        | Member _, RollAspect (aspectId, _, die) ->
            message |> authorizeClientIf (isDieAuthorized die client && isAspectVisible catalog client aspectId)
        | Member _, RollSpare (_, die) ->
            message |> authorizeClientIf (isDieAuthorized die client)
        | Member _, Undo
        | Member _, Redo
        | Guest, SignUp _
        | Guest, LogIn _
        | Guest, RestoreSession _ as request ->
            snd request |> AuthorizedClientMessage
        | Member _, SignUp _
        | Member _, LogIn _
        | Member _, RestoreSession _
        | Member _, ClientIdentity
        | Guest, _ ->
            AuthorizedClientMessage ClientIdentity

    let authorizeServer catalog client = function
        | ClientConnected (world, rolls) ->
            ClientConnected (world |> over World.catalog (authorizeCatalog client), rolls) |> AuthorizedServerMessage
        | WorldReplaced world ->
            world |> over World.catalog (authorizeCatalog client) |> WorldReplaced |> AuthorizedServerMessage
        | WorldUpdated worldMessage ->
            let command = authorizeServerCommand catalog client worldMessage.Command
            WorldUpdated { worldMessage with Command = command } |> AuthorizedServerMessage
        | LoginResult _
        | RollLogUpdated _ as message ->
            AuthorizedServerMessage message

    let clientMessage (AuthorizedClientMessage message) = message

    let serverMessage (AuthorizedServerMessage message) = message

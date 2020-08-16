open Destiny.Server
open Destiny.Server.User
open Destiny.Shared.Lens
open Destiny.Shared.Message
open Destiny.Shared.Profile
open Elmish
open Elmish.Bridge
open Giraffe.Serialization.Json
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Saturn
open System
open System.IO
open System.Security.Cryptography
open System.Threading
open Thoth.Json.Giraffe

type private Client =
    | Guest
    | Profile of Profile Token

type private Context =
    { Universe : Universe MVar
      Hub : ServerHub<Client, ClientMessage, ServerMessage>
      Random : Random
      Crypto : RandomNumberGenerator }

let private saveInterval = TimeSpan.FromSeconds 30.0

let private savePath = "universe.json"

let private serializer = ThothSerializer () :> IJsonSerializer

let private commitBefore = function
    | AddBoard _
    | RemoveBoard _
    | AddEntity _
    | RemoveEntity _
    | AddStatGroup _
    | RemoveStatGroup _
    | AddStat _
    | RemoveStat _
    | AddAspect _
    | RemoveAspect _
    | AddDie _
    | RemoveDie _ -> true
    | _ -> false

let private role = function
    | Guest -> Player
    | Profile token -> (Token.profile token).Role

let private init universeVar dispatch () =
    let universe = MVar.read universeVar
    ClientConnected (Timeline.present universe.History, universe.Rolls) |> dispatch
    Guest, Cmd.none

// TODO: Verify that role in messages matches the client role.
let private update context dispatch message client =
    match message with
    | SignUp (username, password) ->
        MVar.update context.Universe (fun universe ->
            if Map.containsKey username universe.Users then
                Error "That username already exists." |> LoginResult |> dispatch
                universe
            else
                let role = if Map.isEmpty universe.Users then DM else Player
                let profile = { Username = username; Role = role }
                let user = User.create context.Crypto profile password
                Ok user.Profile |> LoginResult |> dispatch
                universe |> over Universe.users (Map.add username user))
        |> ignore
        client, Cmd.none
    | LogIn (username, password) ->
        let universe = MVar.read context.Universe
        let token = Map.tryFind username universe.Users |> Option.bind (User.authenticate password)
        match token with
        | Some token' ->
            Token.profile token' |> Ok |> LoginResult |> dispatch
            Profile token', Cmd.none
        | None ->
            Error "That username doesn't exist." |> LoginResult |> dispatch
            client, Cmd.none
    | UpdateWorld message ->
        if commitBefore message.Command then Timeline.commit else id
        >> Timeline.update (WorldCommand.update message.Command)
        |> over Universe.history
        |> MVar.update context.Universe
        |> ignore
        WorldUpdated message |> context.Hub.BroadcastClient
        client, Cmd.none
    | RollStat (statId, rollId, die) ->
        let universe =
            (statId, rollId)
            ||> Universe.rollStat context.Random die
            |> MVar.update context.Universe
        RollLogUpdated universe.Rolls |> context.Hub.BroadcastClient
        client, Cmd.none
    | RollAspect (aspectId, rollId, die) ->
        let universe = Universe.rollAspect context.Random die aspectId rollId |> MVar.update context.Universe
        RollLogUpdated universe.Rolls |> context.Hub.BroadcastClient
        RemoveDie (aspectId, die) |> WorldMessage.create |> WorldUpdated |> context.Hub.BroadcastClient
        client, Cmd.none
    | RollSpare (rollId, die) ->
        let universe =
            rollId
            |> Universe.rollSpare context.Random die
            |> MVar.update context.Universe
        RollLogUpdated universe.Rolls |> context.Hub.BroadcastClient
        client, Cmd.none
    | Undo ->
        let universe = over Universe.history Timeline.undo |> MVar.update context.Universe
        Timeline.present universe.History |> WorldReplaced |> context.Hub.BroadcastClient
        client, Cmd.none
    | Redo ->
        let universe = over Universe.history Timeline.redo |> MVar.update context.Universe
        Timeline.present universe.History |> WorldReplaced |> context.Hub.BroadcastClient
        client, Cmd.none

let private load () =
    try
        File.ReadAllText savePath
        |> serializer.Deserialize<Universe>
        |> Some
    with
    | :? FileNotFoundException -> None

let private save universeVar =
    let universe = over Universe.history Timeline.commit |> MVar.update universeVar
    File.WriteAllText (savePath, serializer.SerializeToString universe)

let private router context =
    Bridge.mkServer Message.socket (init context.Universe) (update context)
    |> Bridge.withServerHub context.Hub
    |> Bridge.run Giraffe.server

let private app context =
    application {
        app_config Giraffe.useWebSockets
        memory_cache
        url "http://0.0.0.0:8085/"
        use_gzip
        use_json_serializer serializer
        use_router (router context)
        use_static (Path.GetFullPath "../Client/assets")
    }

[<EntryPoint>]
let main _ =
    let context =
        { Universe = load () |> Option.defaultValue Universe.empty |> MVar.create
          Hub = ServerHub ()
          Random = Random ()
          Crypto = RandomNumberGenerator.Create () }
    let host = (app context).Build ()
    let lifetime = host.Services.GetService<IHostApplicationLifetime> ()
    lifetime.ApplicationStopping.Register (fun () -> save context.Universe) |> ignore
    new Timer ((fun _ -> save context.Universe), (), saveInterval, saveInterval) |> ignore
    host.Run ()
    0

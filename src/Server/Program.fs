open Destiny.Server
open Destiny.Server.User
open Destiny.Shared.Collections
open Destiny.Shared.Collections.ResultBuilder
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

let private signUp context dispatch username password universe =
    if Map.containsKey username universe.Users then
        "The username '" + Username.toString username + "' is already taken."
        |> Error
        |> LoginResult
        |> dispatch
        universe
    else
        let role = if Map.isEmpty universe.Users then DM else Player
        let profile = { Username = username; Role = role }
        let user = User.create context.Crypto profile password
        Ok user.Profile |> LoginResult |> dispatch
        universe |> over Universe.users (Map.add username user)

let private logIn context dispatch username password =
    let universe = MVar.read context.Universe
    let token =
        result {
            let! user =
                Map.tryFind username universe.Users
                |> Result.ofOption ("The user '" + Username.toString username + "' was not found.")
            return!
                User.authenticate user password
                |> Result.ofOption "The password is incorrect."
        }
    token |> Result.map Token.profile |> LoginResult |> dispatch
    Result.toOption token

let private updateWorld context message =
    if commitBefore message.Command then Timeline.commit else id
    >> Timeline.update (WorldCommand.update message.Command)
    |> over Universe.history
    |> MVar.update context.Universe
    |> ignore
    WorldUpdated message |> context.Hub.BroadcastClient

let private rollStat context statId rollId die =
    let universe =
        (statId, rollId)
        ||> Universe.rollStat context.Random die
        |> MVar.update context.Universe
    RollLogUpdated universe.Rolls |> context.Hub.BroadcastClient

let private rollAspect context aspectId rollId die =
    let universe = Universe.rollAspect context.Random die aspectId rollId |> MVar.update context.Universe
    RollLogUpdated universe.Rolls |> context.Hub.BroadcastClient
    RemoveDie (aspectId, die) |> WorldMessage.create |> WorldUpdated |> context.Hub.BroadcastClient

let private rollSpare context rollId die =
    let universe =
        rollId
        |> Universe.rollSpare context.Random die
        |> MVar.update context.Universe
    RollLogUpdated universe.Rolls |> context.Hub.BroadcastClient

let private update context dispatch message client =
    let authorized = Auth.authorize (role client) message
    let client' =
        match Auth.peek authorized with
        | SignUp (username, password) ->
            MVar.update context.Universe (signUp context dispatch username password) |> ignore
            client
        | LogIn (username, password) ->
            logIn context dispatch username password |> Option.unwrap client Profile
        | UpdateWorld message ->
            updateWorld context message
            client
        | RollStat (statId, rollId, die) ->
            rollStat context statId rollId die
            client
        | RollAspect (aspectId, rollId, die) ->
            rollAspect context aspectId rollId die
            client
        | RollSpare (rollId, die) ->
            rollSpare context rollId die
            client
        | Undo ->
            let universe = over Universe.history Timeline.undo |> MVar.update context.Universe
            Timeline.present universe.History |> WorldReplaced |> context.Hub.BroadcastClient
            client
        | Redo ->
            let universe = over Universe.history Timeline.redo |> MVar.update context.Universe
            Timeline.present universe.History |> WorldReplaced |> context.Hub.BroadcastClient
            client
    client', Cmd.none

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

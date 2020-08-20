open Destiny.Server
open Destiny.Server.Auth
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

let private nontrivialServerMessage = function
    | WorldUpdated { Command = WorldIdentity } -> None
    | message -> Some message

let private dispatchAuthorized dispatch context client =
    let catalog = (Timeline.present (MVar.read context.Universe).History).Catalog
    Auth.authorizeServer catalog client
    >> Auth.serverMessage
    >> nontrivialServerMessage
    >> Option.iter dispatch

let private broadcastAuthorized context message =
    let catalog = (Timeline.present (MVar.read context.Universe).History).Catalog
    for client in context.Hub.GetModels () |> Set.ofList do
        Auth.authorizeServer catalog client message
        |> Auth.serverMessage
        |> nontrivialServerMessage
        |> Option.iter (context.Hub.SendClientIf ((=) client))

let private init context dispatch () =
    let universe = MVar.read context.Universe
    ClientConnected (Timeline.present universe.History, universe.Rolls) |> dispatch Guest
    Guest, Cmd.none

let private signUp (random : RandomNumberGenerator) username password universe =
    if Map.containsKey username universe.Users then
        universe, "The username '" + Username.toString username + "' is already taken." |> Error
    else
        let role = if Map.isEmpty universe.Users then DM else Player
        let profile = { Username = username; Role = role }
        let user, token = User.create random profile password
        universe |> over Universe.users (Map.add username user), Ok token

let private logIn context dispatch client username password =
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
    token |> Result.map Token.profile |> LoginResult |> dispatch client
    Result.toOption token

let private updateWorld context message =
    if commitBefore message.Command then Timeline.commit else id
    >> Timeline.update (WorldCommand.update message.Command)
    |> over Universe.history
    |> MVar.update context.Universe
    |> ignore
    WorldUpdated message |> broadcastAuthorized context

let private rollStat context statId rollId die =
    SetStatHidden (statId, false) |> WorldMessage.create |> WorldUpdated |> broadcastAuthorized context
    let universe =
        (statId, rollId)
        ||> Universe.rollStat context.Random die
        |> MVar.update context.Universe
    RollLogUpdated universe.Rolls |> broadcastAuthorized context

let private rollAspect context aspectId rollId die =
    SetAspectHidden (aspectId, false) |> WorldMessage.create |> WorldUpdated |> broadcastAuthorized context
    let universe = Universe.rollAspect context.Random die aspectId rollId |> MVar.update context.Universe
    RemoveDie (aspectId, die) |> WorldMessage.create |> WorldUpdated |> broadcastAuthorized context
    RollLogUpdated universe.Rolls |> broadcastAuthorized context

let private rollSpare context rollId die =
    let universe =
        rollId
        |> Universe.rollSpare context.Random die
        |> MVar.update context.Universe
    RollLogUpdated universe.Rolls |> broadcastAuthorized context

let private update context dispatch message client =
    let universe = MVar.read context.Universe
    let world = Timeline.present universe.History
    let authorized = Auth.authorizeClient world.Catalog client message
    let client' =
        match Auth.clientMessage authorized with
        | SignUp (username, password) ->
            let result = MVar.updateResult context.Universe (signUp context.Crypto username password)
            result |> Result.map Token.profile |> LoginResult |> dispatch client
            let client' = result |> Result.unwrap client Profile
            WorldReplaced world |> dispatch client'
            client'
        | LogIn (username, password) ->
            let client' = logIn context dispatch client username password |> Option.unwrap client Profile
            WorldReplaced world |> dispatch client'
            client'
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
            Timeline.present universe.History |> WorldReplaced |> broadcastAuthorized context
            client
        | Redo ->
            let universe = over Universe.history Timeline.redo |> MVar.update context.Universe
            Timeline.present universe.History |> WorldReplaced |> broadcastAuthorized context
            client
        | ClientIdentity -> client
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
    Bridge.mkServer Message.socket
        (fun dispatch -> dispatchAuthorized dispatch context |> init context)
        (fun dispatch -> dispatchAuthorized dispatch context |> update context)
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

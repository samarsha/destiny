open Destiny.Server
open Destiny.Shared.Board
open Destiny.Shared.Lens
open Destiny.Shared.Message
open Elmish
open Elmish.Bridge
open Giraffe.Serialization.Json
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Saturn
open System
open System.IO
open System.Threading
open Thoth.Json.Giraffe

type private Client = { Role : Role }

type private Context =
    { Universe : Universe MVar
      Hub : ServerHub<Client, ClientMessage, ServerMessage>
      Random : Random }

let private saveInterval = TimeSpan.FromSeconds 30.0

let private savePath = "universe.json"

let private serializer = ThothSerializer () :> IJsonSerializer

let private commitBefore = function
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

let private init universeVar dispatch () =
    let universe = MVar.read universeVar
    ClientConnected (Timeline.present universe.Boards, universe.Rolls) |> dispatch
    { Role = Player }, Cmd.none

let private update context dispatch message client =
    match message with
    | UpdateBoard message ->
        if commitBefore message.Command then Timeline.commit else id
        >> Timeline.update (BoardCommand.update message.Command)
        |> over Universe.boards
        |> MVar.update context.Universe
        |> ignore
        BoardUpdated message |> context.Hub.BroadcastClient
        client, Cmd.none
    | RollStat (statId, rollId) ->
        let universe =
            (statId, rollId)
            ||> Universe.rollStat context.Random { Role = client.Role }
            |> MVar.update context.Universe
        RollLogUpdated universe.Rolls |> context.Hub.BroadcastClient
        client, Cmd.none
    | RollAspect (aspectId, rollId) ->
        let die = { Die.Role = client.Role }
        let universe = Universe.rollAspect context.Random die aspectId rollId |> MVar.update context.Universe
        RollLogUpdated universe.Rolls |> context.Hub.BroadcastClient
        RemoveDie (aspectId, die) |> BoardMessage.create |> BoardUpdated |> context.Hub.BroadcastClient
        client, Cmd.none
    | RollSpare rollId ->
        let universe =
            rollId
            |> Universe.rollSpare context.Random { Role = client.Role }
            |> MVar.update context.Universe
        RollLogUpdated universe.Rolls |> context.Hub.BroadcastClient
        client, Cmd.none
    | Undo ->
        let universe = over Universe.boards Timeline.undo |> MVar.update context.Universe
        Timeline.present universe.Boards |> BoardReplaced |> context.Hub.BroadcastClient
        client, Cmd.none
    | Redo ->
        let universe = over Universe.boards Timeline.redo |> MVar.update context.Universe
        Timeline.present universe.Boards |> BoardReplaced |> context.Hub.BroadcastClient
        client, Cmd.none
    | SetRole role ->
        RoleChanged role |> dispatch
        { Role = role }, Cmd.none

let private load () =
    try
        File.ReadAllText savePath
        |> serializer.Deserialize<Universe>
        |> Some
    with
    | :? FileNotFoundException -> None

let private save universeVar =
    let universe = over Universe.boards Timeline.commit |> MVar.update universeVar
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
          Random = Random () }
    let host = (app context).Build ()
    let lifetime = host.Services.GetService<IHostApplicationLifetime> ()
    lifetime.ApplicationStopping.Register (fun () -> save context.Universe) |> ignore
    new Timer ((fun _ -> save context.Universe), (), saveInterval, saveInterval) |> ignore
    host.Run ()
    0

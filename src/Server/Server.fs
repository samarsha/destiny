open Destiny.Server
open Destiny.Shared.Board
open Destiny.Shared.Lens
open Destiny.Shared.Message
open Elmish
open Elmish.Bridge
open Saturn
open System
open System.IO
open Thoth.Json.Giraffe

type private Client = Client of Role

// TODO: Commit the timeline periodically.
let private universeVar = MVar.create Universe.empty

let private hub = ServerHub ()

let private random = Random ()

let private init dispatch () =
    MVar.read universeVar |> Universe.toWorld |> ClientConnected |> dispatch
    Client Player, Cmd.none

let private update dispatch message (Client role as client) =
    let client' =
        match message with
        | UpdateBoard command ->
            // TODO: Commit the timeline for some commands.
            over Universe.boards (BoardCommand.update command.Command |> Timeline.update)
            |> MVar.update universeVar
            |> ignore
            BoardUpdated command |> hub.BroadcastClient
            client
        | RollStat (statId, rollId) ->
            let universe = Universe.rollStat random role statId rollId |> MVar.update universeVar
            RollLogUpdated universe.Rolls |> hub.BroadcastClient
            client
        | RollAspect (aspectId, rollId) ->
            let die = Die role
            let universe = Universe.rollAspect random die aspectId rollId |> MVar.update universeVar
            RollLogUpdated universe.Rolls |> hub.BroadcastClient
            RemoveDie (aspectId, die) |> BoardMessage.create |> BoardUpdated |> hub.BroadcastClient
            client
        | Undo ->
            let universe = over Universe.boards Timeline.undo |> MVar.update universeVar
            Timeline.present universe.Boards |> BoardReplaced |> hub.BroadcastClient
            client
        | Redo ->
            let universe = over Universe.boards Timeline.redo |> MVar.update universeVar
            Timeline.present universe.Boards |> BoardReplaced |> hub.BroadcastClient
            client
        | SetRole role ->
            RoleChanged role |> dispatch
            Client role
    client', Cmd.none

let private app =
    Bridge.mkServer Message.socket init update
    |> Bridge.withServerHub hub
    |> Bridge.run Giraffe.server

run <| application {
    url "http://0.0.0.0:8085/"
    app_config Giraffe.useWebSockets
    use_router app
    use_static (Path.GetFullPath "../Client/assets")
    use_json_serializer (ThothSerializer ())
    use_gzip
    memory_cache
}

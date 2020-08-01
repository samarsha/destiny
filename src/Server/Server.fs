open System
open Destiny.Server
open Destiny.Server.Roll
open Destiny.Shared.Board
open Destiny.Shared.Lens
open Destiny.Shared.Message
open Destiny.Shared.World
open Elmish
open Elmish.Bridge
open Saturn
open System.IO
open Thoth.Json.Giraffe

type private Client = { Role : Role }

let private worldVar = MVar.create World.empty

let private hub = ServerHub ()

let private random = Random ()

let private init dispatch () =
    MVar.read worldVar |> ClientConnected |> dispatch
    { Role = Player }, Cmd.none

let private update dispatch message client =
    let client' =
        match message with
        | UpdateBoard command ->
            over World.board (BoardCommand.update command.Command) |> MVar.update worldVar |> ignore
            BoardUpdated command |> hub.BroadcastClient
            client
        | RollStat (statId, rollId) ->
            let world' = rollStat random client.Role statId rollId |> MVar.update worldVar
            RollLogUpdated world'.Rolls |> hub.BroadcastClient
            client
        | RollAspect (aspectId, rollId) ->
            let die = Die client.Role
            let world' = rollAspect random die aspectId rollId |> MVar.update worldVar
            RollLogUpdated world'.Rolls |> hub.BroadcastClient
            RemoveDie (aspectId, die) |> BoardMessage.create |> BoardUpdated |> hub.BroadcastClient
            client
        | SetRole role ->
            RoleChanged role |> dispatch
            { Role = role }
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

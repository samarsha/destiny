open System
open Destiny.Server
open Destiny.Shared.Board
open Destiny.Shared.Command
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
    dispatch (SetWorld <| MVar.read worldVar)
    { Role = Player }, Cmd.none

let private update _ message client =
    World.update random client.Role message
    |> MVar.update worldVar
    |> SetWorld
    // TODO: If the command was a board command, broadcast to every other client but not the original client.
    |> hub.BroadcastClient
    client, Cmd.none

let private app =
    Bridge.mkServer Command.socket init update
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

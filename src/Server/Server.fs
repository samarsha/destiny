open Destiny.Server
open Destiny.Shared
open Destiny.Shared.Board
open Elmish
open Elmish.Bridge
open Saturn
open System.IO
open Thoth.Json.Giraffe

type private Client = { Role : Role }

let private boardVar = MVar.create Board.empty

let private hub = ServerHub ()

let private init send () =
    send (SetBoard <| MVar.read boardVar)
    { Role = Player }, Cmd.none

let private update _ message client =
    Message.update client.Role message
    |> MVar.update boardVar
    |> SetBoard
    |> hub.BroadcastClient
    client, Cmd.none

let private app =
    Bridge.mkServer Message.socket init update
    |> Bridge.withServerHub hub
    |> Bridge.run Giraffe.server

run <| application {
    url "http://0.0.0.0:8085/"
    app_config Giraffe.useWebSockets
    use_router app
    use_static (Path.GetFullPath "../Client/assets")
    use_json_serializer (ThothSerializer())
    use_gzip
    memory_cache
}

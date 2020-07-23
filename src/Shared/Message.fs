namespace Destiny.Shared

open Destiny.Shared.Scene

type Message =
    | SetScene of Scene
    | AddEntity

module internal Message =
    let socket = "/socket"

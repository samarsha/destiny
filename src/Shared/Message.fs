namespace Destiny.Shared

open Destiny.Shared.Board

type Message =
    | SetBoard of Board
    | AddEntity

module internal Message =
    let socket = "/socket"

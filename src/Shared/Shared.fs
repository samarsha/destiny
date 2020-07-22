module internal Destiny.Shared

type Counter = { Value : int }

type Message =
    | Increment
    | Decrement
    | Set of Counter

let socket = "/socket"

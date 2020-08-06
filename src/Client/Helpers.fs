module internal Destiny.Client.Helpers

open Fable.React.Props

let ref' callback = Ref <| fun element -> if not <| isNull element then callback element

module Destiny.Shared.Logic

let (=>) a (b : _ Lazy) = not a || b.Value

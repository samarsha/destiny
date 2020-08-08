namespace Destiny.Shared

type Lens<'a, 'b> =
    { Get : 'a -> 'b
      Set : 'b -> 'a -> 'a }

module Lens =
    let internal (.<-) lens value = lens.Set value

    let over lens f source = (lens.Get source |> f, source) ||> lens.Set

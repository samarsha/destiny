namespace Destiny.Shared

type internal Lens<'a, 'b> =
    { Get : 'a -> 'b
      Set : 'b -> 'a -> 'a }

module internal Lens =
    let (.<-) lens v = lens.Set v

    let over lens f v = (lens.Get v |> f, v) ||> lens.Set

namespace Destiny.Shared

type internal Lens<'a, 'b> =
    { Get : 'a -> 'b
      Set : 'b -> 'a -> 'a }

module internal Lens =
    let update lens f x = x |> lens.Set (lens.Get x |> f)

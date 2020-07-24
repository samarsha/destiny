namespace Destiny.Shared

type internal Lens<'a, 'b> =
    { Get : 'a -> 'b
      Set : 'b -> 'a -> 'a }

module internal Lens =
    let (.<-) lens value = lens.Set value

    let over lens f source = (lens.Get source |> f, source) ||> lens.Set

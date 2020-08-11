namespace Destiny.Shared

type Lens<'a, 'b> =
    { Get : 'a -> 'b
      Set : 'b -> 'a -> 'a }

module Lens =
    let (.>>) lens1 lens2 =
        { Get = lens1.Get >> lens2.Get
          Set = fun value source -> source |> lens1.Set (lens1.Get source |> lens2.Set value) }

    let internal (.<-) lens value = lens.Set value

    let over lens f source = source |> lens.Set (lens.Get source |> f)

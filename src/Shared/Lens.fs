module Destiny.Shared.Lens

type Lens<'a, 'b> =
    private
        { Get : 'a -> 'b
          Set : 'b -> 'a -> 'a }

let lens getter setter = { Get = getter; Set = setter }

let (.>>) lens1 lens2 =
    { Get = lens1.Get >> lens2.Get
      Set = fun value source -> source |> lens1.Set (lens1.Get source |> lens2.Set value) }

let (.<-) lens value = lens.Set value

let view lens = lens.Get

let over lens f source = source |> lens.Set (lens.Get source |> f)

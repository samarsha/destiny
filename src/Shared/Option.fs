module internal Destiny.Shared.Option

type OptionBuilder () =
    member _.Bind (option, binder) = Option.bind binder option

    member _.Return value = Some value

    member _.Zero () = None

let option = OptionBuilder ()

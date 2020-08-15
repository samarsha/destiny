module internal Destiny.Client.Login

open Destiny.Shared.Collections
open Destiny.Shared.Profile
open Fable.React
open Fable.React.Props

type Model =
    private
        { Username : Username
          Password : Password }

type ViewModel =
    private
    | LoggingIn of Username * Password
    | LoggedIn of Profile

type Message =
    private
    | SetUsername of Username
    | SetPassword of Password
    | SubmitLogin
    | SubmitSignup

type Event =
    | NoEvent
    | LogIn of Username * Password
    | SignUp of Username * Password

// Model

let empty =
    { Username = Username ""
      Password = Password "" }

let makeViewModel model profile =
    profile |> Option.unwrap (LoggingIn (model.Username, model.Password)) LoggedIn

// View

let view model dispatch =
    match model with
    | LoggingIn (Username username, Password password) ->
        [ input [ OnChange <| fun event -> Username event.Value |> SetUsername |> dispatch
                  Placeholder "Username"
                  Value username ]
          input [ OnChange <| fun event -> Password event.Value |> SetPassword |> dispatch
                  Placeholder "Password"
                  Type "password"
                  Value password ]
          button [ OnClick <| fun _ -> dispatch SubmitLogin ] [ str "Log in" ]
          button [ OnClick <| fun _ -> dispatch SubmitSignup ] [ str "Sign up" ] ]
    | LoggedIn { Username = Username username; Role = Player } -> [ str username ]
    | LoggedIn { Username = Username username; Role = DM } -> [ str <| username + " (DM)" ]
    |> div [ Class "user" ]

// Update

let update message model =
    match message with
    | SetUsername username -> { model with Model.Username = username }, NoEvent
    | SetPassword password -> { model with Password = password }, NoEvent
    | SubmitLogin -> { model with Password = Password "" }, LogIn (model.Username, model.Password)
    | SubmitSignup -> { model with Password = Password "" }, SignUp (model.Username, model.Password)

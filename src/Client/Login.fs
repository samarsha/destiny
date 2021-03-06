﻿module internal Destiny.Client.Login

open Destiny.Shared
open Fable.React
open Fable.React.Props

type Model =
    private
        { Username : Username
          Password : Password }

type ViewModel =
    private
    | LoggingIn of Username * Password
    | LoggedIn of Profile * Team

type Message =
    private
    | SetUsername of Username
    | SetPassword of Password
    | SubmitLogin
    | SubmitSignup
    | SelectTeam of Team

type Event =
    | NoEvent
    | LogIn of Username * Password
    | SignUp of Username * Password
    | Impersonate of Team

// Model

let init =
    { Username = Username ""
      Password = Password "" }

let makeViewModel model profile impersonation =
    match profile with
    | Some profile' -> LoggedIn (profile', impersonation)
    | None -> LoggingIn (model.Username, model.Password)

// View

let private viewImpersonation dispatch (profile : Profile) impersonation =
    [ option [ Value <| upcast Player.ToString () ] [ str "Player" ] |> Some
      option [ Value <| upcast DM.ToString () ] [ str "DM" ] |> Option.iff (profile.Team = DM) ]
    |> List.choose id
    |> select [ OnChange <| fun event -> Team.ofString event.Value |> Option.iter (SelectTeam >> dispatch)
                Value <| upcast impersonation.ToString () ]

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
    | LoggedIn (profile, impersonation) ->
        [ Username.toString profile.Username |> str
          viewImpersonation dispatch profile impersonation ]
    |> div [ Class "user" ]

// Update

let update message model =
    match message with
    | SetUsername username -> { model with Model.Username = username }, NoEvent
    | SetPassword password -> { model with Password = password }, NoEvent
    | SubmitLogin -> { model with Password = Password "" }, LogIn (model.Username, model.Password)
    | SubmitSignup -> { model with Password = Password "" }, SignUp (model.Username, model.Password)
    | SelectTeam team -> model, Impersonate team

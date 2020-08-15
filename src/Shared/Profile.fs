namespace Destiny.Shared.Profile

type Username = Username of string

type Password = Password of string

type Role =
    | Player
    | DM

type Profile =
    { Username : Username
      Role : Role }

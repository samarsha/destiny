namespace Destiny.Server.User

open Destiny.Shared.Profile
open Microsoft.AspNetCore.Cryptography.KeyDerivation
open System.Security.Cryptography

type internal 'a User =
    { Profile : 'a
      Salt : byte list
      Hash : byte list }

type internal 'a Token = private Token of 'a

module internal User =
    let private hash (Password password) salt =
        KeyDerivation.Pbkdf2 (password, List.toArray salt, KeyDerivationPrf.HMACSHA512, 10000, 32)
        |> Array.toList

    let create (random : RandomNumberGenerator) profile password =
        let salt = Array.zeroCreate 16
        random.GetBytes salt
        let salt' = Array.toList salt
        let user =
            { Profile = profile
              Salt = salt'
              Hash = hash password salt' }
        user, Token profile

    let authenticate user password =
        if hash password user.Salt = user.Hash
        then Token user.Profile |> Some
        else None

module internal Token =
    let profile (Token profile') = profile'

module Fake.Shake.RuleBuilders
open System.IO
open System.Security.Cryptography
open Fake
open Fake.Shake.Core
open Fake.Shake.Control

let private hashFile path =
    let sha = SHA1.Create()
    using (File.OpenRead path)
        (fun stream ->
            sha.ComputeHash(stream) |> ContentHash)

let fileRule pattern (fileAction : Key -> Action<unit>) =
    {
        Action = fun (Key k) ->
            action {
                let fullPath = System.IO.Path.GetFullPath k
                if k <> fullPath then
                    return! require<ContentHash> (Key fullPath)
                else
                    do! fileAction (Key k)
                    return hashFile k
            }
        Provides = fun (Key k) ->
            Globbing.isMatch pattern k
        ValidStored = fun (Key k) old ->
            try
                File.Exists k && (old = hashFile k)
            with
            | _ -> false
    }
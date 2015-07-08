#if INTERACTIVE
#r "packages/FAKE/tools/FakeLib.dll"
#load "Fake.Shake.Core.fsx"
#load "Fake.Shake.Control.fsx"
#else
module Fake.Shake.DefaultRules
#endif
open System.IO
open System.Security.Cryptography
open Fake
open Fake.Shake.Core
open Fake.Shake.Control

let private hashFile path =
    let sha = SHA1.Create()
    sha.ComputeHash(File.OpenRead path) |> ContentHash

let defaultFile =
    {
        Action = fun (Key k) ->
            action {
                return hashFile k
            }
        Provides = fun (Key k) -> File.Exists k
        ValidStored = fun (Key k) bytes ->
            try
                let old = binary.UnPickle bytes
                File.Exists k && (old = hashFile k)
            with
            | _ -> false
    }

let defaultDir =
    {
        Action = fun (Key k) -> action { tracefn "Found directory %s" k; return () }
        Provides = fun (Key k) -> Directory.Exists k
        ValidStored = fun (Key k) _ -> Directory.Exists k
    }

let allDefaults : IRule list =
    [defaultFile;defaultDir]

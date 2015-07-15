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
    using (File.OpenRead path)
        (fun stream ->
            sha.ComputeHash(stream) |> ContentHash)

let rec defaultFile =
    {
        Action = fun (Key k) ->
            action {
                let fullPath = System.IO.Path.GetFullPath k
                if k <> fullPath then
                    return! require (Key fullPath)
                else
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
        Action = fun (Key k) -> action {
                tracefn "Found directory %s, requiring contents" k
                do!
                    Directory.GetFiles k
                    |> Seq.map (System.IO.Path.GetFullPath)
                    |> Seq.map Key |> List.ofSeq |> needs
                let! _ = Directory.GetDirectories k |> Seq.map Key |> List.ofSeq |> requires
                return ()
            }
        Provides = fun (Key k) -> Directory.Exists k
        ValidStored = fun (Key k) _ -> Directory.Exists k
    }

let allDefaults : IRule list =
    [defaultFile;defaultDir]

module Fake.Shake.DefaultRules
open System.IO
open System.Security.Cryptography
open Fake
open Fake.Shake.Core
open Fake.Shake.Control
open Fake.Shake.RuleBuilders

module FileRules =
    let defaultFile =
        let act (Key k) =
            action {
                return ()
            }
        { fileRule "**/*" act with
            Provides = fun (Key k) -> File.Exists k }

    let defaultDir =
        {
            Action = fun (Key k) -> action {
                    let fullPath = System.IO.Path.GetFullPath k
                    if k <> fullPath then
                        return! require (Key fullPath)
                    else
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
    [
        FileRules.defaultFile
        FileRules.defaultDir
    ]

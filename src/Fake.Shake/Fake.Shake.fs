[<AutoOpen>]
module Fake.Shake.Build
open System.Collections.Concurrent
open Fake
open Fake.Shake.Core
open Fake.Shake.Control
open Hopac

let [<Literal>] cacheFile = ".fake.shake.cache"

type [<NoComparison>] Cache =
    {
        Dependencies : ConcurrentDictionary<Key, Key list>
        Results : Map<Key, byte []>
    }

let build rules key =
    let (old : Cache) =
        try
            System.IO.File.ReadAllBytes cacheFile
            |> binary.UnPickle
        with
        | _ ->
            { Dependencies = ConcurrentDictionary(); Results = Map.empty }
    let state =
        {
            Rules = rules
            Results = ConcurrentDictionary()
            OldResults = old.Results
            Current = None
            Dependencies = old.Dependencies
            Stack = []
        }
    let finalState, result =
       require key state |> Job.Global.run
    let mergedResults =
        finalState.Results.ToArray()
        |> Seq.map (fun kv -> kv.Key, kv.Value |> Job.Global.run)
        |> Map.ofSeq
        |> Map.fold (fun old k bytes -> Map.add k bytes old) old.Results
    let cache = { Dependencies = finalState.Dependencies; Results = mergedResults }
    System.IO.File.WriteAllBytes(cacheFile, binary.Pickle cache)
    result

let readLines fileName =
  action {
       do! need (Key fileName)
       return System.IO.File.ReadAllLines fileName |> Array.toList
    }

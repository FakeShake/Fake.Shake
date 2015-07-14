#if INTERACTIVE
#r "packages/FAKE/tools/FakeLib.dll"
#r "packages/FsPickler/lib/net45/FsPickler.dll"
#r "packages/Hopac/lib/net45/Hopac.Core.dll"
#r "packages/Hopac/lib/net45/Hopac.dll"
#r "packages/Hopac/lib/net45/Hopac.Platform.dll"
#load "Fake.Shake.Core.fsx"
#load "Fake.Shake.Control.fsx"
#load "Fake.Shake.DefaultRules.fsx"
#else
[<AutoOpen>]
module Fake.Shake.Build
#endif
open Fake
open Fake.Shake.Core
open Fake.Shake.Control
open Hopac

let [<Literal>] cacheFile = ".fake.shake.cache"

type Cache =
    {
        Dependencies : Map<Key, Key list>
        Results : Map<Key, byte []>
    }

let build rules key =
    let (old : Cache) =
        try
            System.IO.File.ReadAllBytes cacheFile
            |> binary.UnPickle
        with
        | _ ->
        { Dependencies = Map.empty; Results = Map.empty }
    let state =
        {
            Rules = rules
            Results = Map.empty
            OldResults = old.Results
            Current = None
            Dependencies = old.Dependencies
            Stack = []
        }
    let finalState, buildComputation = require key state
    let result = buildComputation |> Job.Global.run
    let mergedResults =
        finalState.Results
        |> Map.map (fun _ lazy' -> Job.Global.run lazy')
        |> Map.fold (fun old k bytes -> Map.add k bytes old) old.Results
    let cache = { Dependencies = finalState.Dependencies; Results = mergedResults }
    System.IO.File.WriteAllBytes(cacheFile, binary.Pickle cache)
    result

let readLines fileName =
  action {
       do! need (Key fileName)
       return System.IO.File.ReadAllLines fileName |> Array.toList
    }

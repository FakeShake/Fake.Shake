module Fake.Shake.Tests
open Fake.Shake.Core
open Fake.Shake.Control
open FsCheck
open NUnit.Framework

let state = {
                Rules = Seq.empty
                Results = System.Collections.Concurrent.ConcurrentDictionary()
                OldResults = Map.empty
                Current = None
                Dependencies = System.Collections.Concurrent.ConcurrentDictionary()
                Stack = []
            }

[<Test>]
let ``Try finally works`` () =
    let finallyFired = ref false
    let act = action {
        try
            failwith "I will always fail!"   
        finally
            finallyFired := true            
    }
    try
        (act state).Force() |> Async.RunSynchronously |> ignore
    with _ -> ()
    Assert.True(!finallyFired)

[<Test>]
let ``Try with works`` () =
    let finallyFired = ref false
    let act = action {
        try
            failwith "I will always fail!"   
        with
        | _ -> finallyFired := true            
    }
    (act state).Force() |> Async.RunSynchronously |> ignore
    Assert.True(!finallyFired)
    
module Fake.Shake.Tests
open Fake.Shake.Core
open Fake.Shake.Control
open FsCheck
open Hopac
open NUnit.Framework

let bindable =
    gen {
        return (fun i -> action { return 1 + i })
    }

let state = {
                Rules = Seq.empty
                Results = System.Collections.Concurrent.ConcurrentDictionary()
                OldResults = Map.empty
                Current = None
                Dependencies = System.Collections.Concurrent.ConcurrentDictionary()
                Stack = []
            }

type ActionGenerators =
    static member Bindable() =
        { new Arbitrary<int -> Action<int>>() with
              override x.Generator = bindable
        }

type ActionMonadProps () =
    static member ``First law`` (f : int -> Action<int>) x =
        let r = ((return' x) >>= f) state |> Job.Global.run |> snd
        let r' = (f x state) |> Job.Global.run |> snd
        r = r'

[<Test>]
let ``Action is a Monad`` () =
    Check.All<ActionMonadProps> { Config.QuickThrowOnFailure with Arbitrary = [typeof<ActionGenerators>] }

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
        act state |> Job.Global.run |> ignore
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
    act state |> Job.Global.run |> ignore
    Assert.True(!finallyFired)
    
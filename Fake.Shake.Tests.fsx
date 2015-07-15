#if INTERACTIVE
#r "bin/Fake.Shake.dll"
#r "packages/NUnit.Runners/tools/nunit.framework.dll"
#r "packages/FsCheck/lib/net45/FsCheck.dll"
#r "packages/Hopac/lib/net45/Hopac.Core.dll"
#r "packages/Hopac/lib/net45/Hopac.dll"
#endif

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

// random comment
type ActionMonadProps () =
    static member ``First law`` (f : int -> Action<int>) x =
        let r = ((return' x) >>= f) state |> Job.Global.run |> snd
        let r' = (f x state) |> Job.Global.run |> snd
        r = r'

[<Test>]
let ``Action is a Monad`` () =
    Check.All<ActionMonadProps> { Config.QuickThrowOnFailure with Arbitrary = [typeof<ActionGenerators>] }

#if INTERACTIVE
#r "output/Fake.Shake.dll"
#r "packages/NUnit.Runners/tools/nunit.framework.dll"
#r "packages/FsCheck/lib/net45/FsCheck.dll"
#endif

open FsCheck
open NUnit.Framework

type ActionMonadProps () =
    static member IsTrue () =
        true


[<Test>]
let ``Action is a Monad`` () =
    Check.QuickThrowOnFailureAll<ActionMonadProps>()
    

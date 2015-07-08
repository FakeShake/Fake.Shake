#load "Fake.Shake.fsx"
#r "packages/FAKE/tools/FakeLib.dll"
open System.IO
open System.Text.RegularExpressions
open Fake
open Fake.FscHelper
open Fake.Shake
open Fake.Shake.Core
open Fake.Shake.Control
open Fake.Shake.DefaultRules

let refReg = Regex("""^#r "(?<ref>.*)"$""", RegexOptions.Compiled)
let sourceReg = Regex("""^#load "(?<src>.*)"$""", RegexOptions.Compiled)
let dllReg = Regex("""output/.*.dll""", RegexOptions.Compiled)
let packageReg = Regex("""packages/.*.[dll|exe]""", RegexOptions.Compiled)

let outputDir =
    {
        Action = fun (Key k) -> action { return ensureDirectory k }
        Provides = fun (Key k) -> k = "output"
        ValidStored = defaultDir.ValidStored
    }

let compileLib =
    {
        Action =
            fun (Key k) -> action {
                tracefn "Building %s" k
                let sourceFile =
                    k |> filename |> changeExt "fsx"
                let! lines =
                    readLines sourceFile
                let refs =
                    lines
                    |> List.filter (fun l -> refReg.IsMatch l)
                    |> List.map (fun l -> refReg.Match(l).Groups.["ref"].Value)
                let copyLocalRefs =
                    refs
                    |> List.filter (fun ref -> filename ref <> ref)
                do! require (Key "output")
                let copyLocal localRef =
                    let target = "output" @@ (filename localRef)
                    if not <| File.Exists target then
                        CopyFile target localRef
                    Key target
                do! needs (copyLocalRefs |> List.map Key)
                do! needs (copyLocalRefs |> List.map copyLocal)
                let otherSourceFiles =
                    lines
                    |> List.filter (fun l -> sourceReg.IsMatch l)
                    |> List.map (fun l -> sourceReg.Match(l).Groups.["src"].Value)
                do! needs (otherSourceFiles |> List.map Key)
                let setParams (p : FscParams) =
                    { p with
                        Debug = false
                        FscTarget = Library
                        Platform = AnyCpu
                        Output = k
                        References = refs }
                do Fsc setParams (otherSourceFiles @ [sourceFile])
                return! defaultFile.Action (Key k)
            }
        Provides = fun (Key k) -> dllReg.IsMatch k && File.Exists (k |> filename |> changeExt "fsx")
        ValidStored = defaultFile.ValidStored
    }

let runTests =
    {
        Provides = fun (Key k) -> k = "TestResult.xml"
        Action =
            fun (Key k) ->
                action {
                    do! need (Key "output/Fake.Shake.Tests.dll")
                    NUnit id ["output/Fake.Shake.Tests.dll"]
                    return! defaultFile.Action (Key "TestResult.xml")
                }
        ValidStored = defaultFile.ValidStored
    }

let main =
    {
        Provides = fun (Key k) -> k = "main"
        Action =
            fun (Key k) -> action { do! need (Key "TestResult.xml") }
        ValidStored = fun (Key k) _ -> true
    }

let rules : IRule list = [main;outputDir;compileLib;runTests]

#time "on"
do build (rules @ allDefaults) (Key "main")
#time "off"

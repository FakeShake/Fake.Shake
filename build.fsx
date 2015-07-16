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
let packageReg = Regex("""packages/.*.[dll|exe]""", RegexOptions.Compiled)
let binGlob = Globbing.isMatch "bin/*"
let compileReg = Regex("""^compile::(?<fullpath>.*)""", RegexOptions.Compiled)

let outputDir =
    {
        Action = fun (Key k) -> action {
                CleanDir k
                do! need (Key "paket.lock")
                let! templateLines = readLines "paket.template"
                do!
                    templateLines
                    |> List.map (fun s -> s.Trim())
                    |> List.filter binGlob
                    |> List.map Key
                    |> needs
                let setParams (p : Paket.PaketPackParams) =
                    { p with OutputPath = "output" }
                Paket.Pack setParams
                return! defaultDir.Action (Key k)
            }
        Provides = fun (Key k) -> "output" = k
        ValidStored = defaultDir.ValidStored
    }

let binDir =
    {
        Action = fun (Key k) -> action { return ensureDirectory k }
        Provides = fun (Key k) -> k = "bin"
        ValidStored = fun (Key k) _ -> Directory.Exists k
    }

let compile =
    {
        Action =
            fun (Key k) -> action {
                let path = compileReg.Match(k).Groups.["fullpath"].Value
                tracefn "Building %s" path
                let sourceFile =
                    path |> filename |> changeExt "fsx" |> System.IO.Path.GetFullPath
                let! lines =
                    readLines sourceFile
                let refs =
                    lines
                    |> List.filter (fun l -> refReg.IsMatch l)
                    |> List.map (fun l -> refReg.Match(l).Groups.["ref"].Value)
                let copyLocalRefs =
                    refs
                    |> List.filter (fun ref -> filename ref <> ref)
                    |> List.map Key
                do! require (Key "bin")
                let copyLocal (Key localRef) =
                    let target = "bin" @@ (filename localRef)
                    if not <| File.Exists target then
                        CopyFile target localRef
                    Key target
                let otherSourceFiles =
                    lines
                    |> List.filter (fun l -> sourceReg.IsMatch l)
                    |> List.map (fun l -> sourceReg.Match(l).Groups.["src"].Value)
                do! needs <| List.concat [copyLocalRefs
                                          (copyLocalRefs |> List.map copyLocal)
                                          otherSourceFiles |> List.map Key]
                let setParams (p : FscParams) =
                    { p with
                        Debug = false
                        FscTarget = Library
                        Platform = AnyCpu
                        Output = path
                        References = refs }
                do Fsc setParams (otherSourceFiles @ [sourceFile])
                return! require<ContentHash> (Key path)
            }
        Provides = fun (Key k) -> compileReg.IsMatch k
        ValidStored = fun (Key k) -> defaultFile.ValidStored (compileReg.Match(k).Groups.["fullpath"].Value |> Key)
    }

let isCompileLib =
    {
        Action =
            fun (Key k) -> action {
                let fullpath = System.IO.Path.GetFullPath k
                return! require<ContentHash> (Key ("compile::" + fullpath))
            }
        Provides = fun (Key k) -> binGlob k && File.Exists (k |> filename |> changeExt "fsx")
        ValidStored = defaultFile.ValidStored
    }

let nunit =
    {
        Provides = fun (Key k) -> k = "TestResult.xml"
        Action =
            fun (Key k) ->
                action {
                    do! need (Key <| "bin" @@ "Fake.Shake.Tests.dll")
                    NUnit id ["bin" @@ "Fake.Shake.Tests.dll"]
                    return! defaultFile.Action (Key "TestResult.xml")
                }
        ValidStored = defaultFile.ValidStored
    }

let runTests =
    {
        Provides = fun (Key k) -> k = "test"
        Action =
            fun (Key k) ->
                action {
                    do! need (Key "TestResult.xml")
                }
        ValidStored = fun _ _ -> true
    }

let main =
    {
        Provides = fun (Key k) -> k = "main"
        Action =
            fun (Key k) -> action {
                // Just so we detect changes to the build script...
                do! need (Key "build.fsx")
                do! doAll [Key "test"; Key "output"]
            }
        ValidStored = fun _ _ -> true
    }

let rules : IRule list = [main;binDir;outputDir;isCompileLib;compile;runTests;nunit]

#time "on"
do build (rules @ allDefaults) (Key "main")
#time "off"

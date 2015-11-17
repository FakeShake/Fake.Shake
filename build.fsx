// Manually add dll search paths for mono builds
#I "packages/FAKE/tools"
#I "packages/NUnit.Runners/tools"
#r "packages/FAKE/tools/FakeLib.dll"
#r "packages/FsCheck/lib/net45/FsCheck.dll"
#r "packages/FsPickler/lib/net45/FsPickler.dll"
#load "paket-files/mavnn/FSharp.Control.AsyncLazy/FSharp.Control.AsyncLazy/AsyncLazy.fs"
#load "src/Fake.Shake/Core.fs"
#load "src/Fake.Shake/Control.fs"
#load "src/Fake.Shake/RuleBuilders.fs"
#load "src/Fake.Shake/DefaultRules.fs"
#load "src/Fake.Shake/Build.fs"
open Fake
open Fake.Shake
open Fake.Shake.Core
open Fake.Shake.Control
open Fake.Shake.RuleBuilders
open Fake.Shake.DefaultRules
open Fake.Shake.DefaultRules.FileRules

let configuration = environVarOrDefault "Configuration" "Release"

let analyseXml = """<?xml version="1.0" encoding="utf-8"?>
<Project ToolsVersion="12.0" DefaultTargets="WriteStuff" xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
  <Import Project="$(TargetProject)"/>
  <Target Name="WriteStuff" DependsOnTargets="ResolveReferences">
    <Message Importance="high" Text="References::@(ReferencePath)"/>
    <Message Importance="high" Text="Compiles::@(BeforeCompile);@(Compile);@(AfterCompile)"/>
    <Message Importance="high" Text="Output::$(OutputPath)"/>
  </Target>
</Project>
"""

type ProjectData =
    {
        References : string list
        Compiles : string list
        OutputDir : string
    }

let extractLineData (prefix : string) (line : string) =
    line
        .Substring(prefix.Length)
        .Split(';')
    |> Array.filter ((<>) "")

let extractData project path =
    let projDir = directory project
    let prefixes = [|"References::";"Compiles::";"Output::"|]
    System.IO.File.ReadAllLines path
    |> Array.toSeq
    |> Seq.map (fun s -> s.Trim())
    |> Seq.filter ((<>) "")
    |> Seq.skipWhile (fun s -> not <| s.StartsWith(prefixes.[0]))
    |> Seq.take 3
    |> Seq.zip prefixes
    |> Seq.map (fun (prefix, line) -> extractLineData prefix line)
    |> Seq.map Seq.toList
    |> Seq.toList
    |> function
       | [references;compiles;[outputDir]] ->
        {
            References = references
            Compiles = compiles |> List.map ((@@) projDir)
            OutputDir = projDir @@ outputDir
        }
       | _ -> failwithf "Invalid project analysis file generated at %s" path

let loggerProp filename =
    { 
        Number = 1
        Filename = Some filename
        Verbosity = Some MSBuildVerbosity.Minimal
        Parameters = None
    }

let analyseParams projLoc loggerProp (p : MSBuildParams) =
    { p with
        NoLogo = true
        Properties =
            [
                "TargetProject", projLoc
                "Configuration", configuration
            ]
        FileLoggers = Some [loggerProp]
    }

let buildParams (p : MSBuildParams) =
    { p with
        NoLogo = true
        Properties =
            [
                "Configuration", configuration
            ] }

let defaultProj =
    {
        Action = fun (Key k) -> action {
                let temp = System.IO.Path.GetTempFileName()
                let projDir = directory k
                let analyseFile = projDir @@ "Analyse.msbuild"
                try
                    // Make sure project references are built
                    let projectReferences =
                        getProjectReferences k
                        |> Set.map Key
                    do! needs projectReferences

                    // Use MsBuild to get full requirements list
                    // TODO: Add resource files
                    System.IO.File.WriteAllText(analyseFile, analyseXml)
                    let logger = loggerProp temp
                    MSBuildHelper.build
                        (analyseParams k logger)
                        analyseFile
                    let data =
                        extractData k temp
                    do! needs (List.concat [data.References;data.Compiles] |> List.map Key)

                    // Actually build the project with MSBuild
                    MSBuildHelper.build buildParams k

                    // Needs the output directory
                    do! require (Key data.OutputDir)

                    if System.IO.File.Exists temp then
                        System.IO.File.Delete temp
                    if System.IO.File.Exists analyseFile then
                        System.IO.File.Delete analyseFile

                    // Make sure we capture current proj file
                    return! defaultFile.Action (Key k)
                finally
                    if System.IO.File.Exists temp then
                        System.IO.File.Delete temp
                    if System.IO.File.Exists analyseFile then
                        System.IO.File.Delete analyseFile

            }
        Provides = fun (Key k) -> Globbing.isMatch "**/*.*proj" k
        ValidStored = defaultFile.ValidStored
    }

let defaultPack =
    {
        Action = fun (Key k) -> action {
                let fullPath = System.IO.Path.GetFullPath k
                if fullPath <> k then
                    return! require (Key fullPath)
                else
                    let matchingProjectFile =
                        !! (directory k @@ "*.*proj")
                        |> Seq.exactlyOne
                    do! need (Key matchingProjectFile)
                    let setParams (p : Paket.PaketPackParams) =
                        { p with
                            OutputPath = "output"
                            TemplateFile = k }
                    Paket.Pack setParams
                    return! defaultFile.Action (Key k)
            }
        Provides = fun (Key k) -> Globbing.isMatch "**/*paket.template" k
        ValidStored = defaultFile.ValidStored
    }

let private nunitParams (p : NUnitParams) =
    { p with Framework = "net-4.5" }

let nunit =
    let act (Key k) =
        action {
            do! needs (!! "test/**/*.*proj" |> Seq.map Key)
            let testDlls = !! (sprintf "test/**/bin/%s/*.Tests.dll" configuration) |> Seq.toList
            do! needs (testDlls |> Seq.map Key)
            NUnit nunitParams testDlls
        }
    fileRule "**/TestResult.xml" act

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

let runPack =
    {
        Provides = fun (Key k) -> k = "pack"
        Action =
            fun (Key k) ->
                action {
                    CleanDir "output"
                    do! needs (!! "**/*paket.template" |> Seq.map Key)
                    do! require (Key "output")
                }
        ValidStored = fun _ _ -> true
    }

let main =
    {
        Provides = fun (Key k) -> k = "main"
        Action =
            fun (Key k) -> action {
                do! doAll[Key "pack";Key "test"]
            }
        ValidStored = fun _ _ -> true
    }

let rules : IRule list = [main;runTests;nunit;defaultProj;runPack;defaultPack]

#time "on"
do build (FakeShakeConfig.Default) (rules @ allDefaults) (Key "main")
#time "off"

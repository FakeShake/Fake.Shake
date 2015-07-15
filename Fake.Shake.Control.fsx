#if INTERACTIVE
#r "packages/FAKE/tools/FakeLib.dll"
#r "packages/FsPickler/lib/net45/FsPickler.dll"
#load "Fake.Shake.Core.fsx"
#else
[<AutoOpen>]
module Fake.Shake.Control
#endif
open Fake
open Fake.Shake.Core
open Hopac
open Nessos.FsPickler

let binary = FsPickler.CreateBinary()

let bind (continuation : 'a -> Action<'b>) (expr : Action<'a>) : Action<'b> =
    fun state ->
        job {
            let! results = expr state
            let state', result = results
            return! continuation result state'
        } |> Promise.Now.delay

let (>>=) expr continuation =
    bind continuation expr

let return' x =
    fun state ->
        Promise.Now.withValue (state, x)

let map f act =
    bind (f >> return') act

let combine expr1 expr2 =
    expr1 |> bind (fun () -> expr2)

let tryWith expr handler =
    fun state ->
        try expr state
        with e -> (handler e) state

let tryFinally expr comp =
    fun state ->
        try expr state
        finally comp()

let rec skip key state =
    let maybeRule = State.rawFind state key
    match maybeRule with
    | None -> false
    | Some rule ->
        match state.OldResults |> Map.tryFind key with
        | Some old ->
            let deps = match state.Dependencies.TryGetValue key with false, _ -> [] | true, ds -> ds
            rule.ValidStored key old
            && deps |> List.forall (fun dep -> skip dep state)
        | None -> false


let private run<'a> key state =
    match skip key state with
    | true ->
        job {
            tracefn "Skipped %A, valid stored result" key
            let pickle = state.OldResults.[key] |> Promise.Now.withValue
            let result : 'a =
                state.OldResults.[key] |> (binary.UnPickle)
            state.Results.GetOrAdd(key, pickle) |> ignore
        }
    | false ->
        job {
            tracefn "No valid stored value for %A, running" key
            let rule = State.find state key
            let (result : Promise<State * 'a>) =
                State.clearDeps key state
                rule.Action key state
            let pickled =
                result
                |> Job.map (fun (state, r) -> binary.Pickle r)
                |> Promise.Now.delay
            state.Results.GetOrAdd(key, pickled) |> ignore
        }

let private requireCh : Ch<Key * State * (Key -> State -> Job<unit>) * Ch<unit>> =
    Ch.create ()
    |> Hopac.TopLevel.run

let rec private processRequire () =
    job {
        let! key, state, run, ack = Ch.take requireCh
        do!
            match state.Results.TryGetValue key with
            | true, r -> Alt.always () :> Job<unit>
            | false, _ -> run key state
        do! Ch.send ack ()
        return! processRequire ()
    }

do processRequire () |> Hopac.TopLevel.start

let require<'a> key : Action<'a> =
    fun state ->
        job {
            tracefn "%A required via stack %A" key state.Stack
            let state =
                state
                |> State.push key
            let! ackChannel = Ch.create ()
            do! Ch.send requireCh (key, state, run<'a>, ackChannel)
            do! Ch.take ackChannel
            let! state, result =
                state.Results.[key]
                |> Job.bind (fun bytes -> Promise.Now.withValue (state, binary.UnPickle bytes))
            return (State.pop state, result)
        } |> Promise.Now.delay

let requires<'a> keys : Action<'a list> =
    fun state ->
        Extensions.Seq.Con.mapJob (fun k -> require<'a> k state) keys
        |> Job.map Seq.toList
        |> Job.map (List.map snd)
        |> Job.map (fun results -> state, results)
        |> Promise.Now.delay

let doAll keys : Action<unit> =
    fun state ->
        Extensions.Seq.Con.mapJob (fun k -> (require<unit> k) state) keys
        |> Job.map (fun _ -> state, ())
        |> Promise.Now.delay

let need key : Action<unit> =
    require<ContentHash> key
    |> map ignore

let needs keys : Action<unit> =
    requires<ContentHash> keys
    |> map ignore

type ActionBuilder () =
    member __.Bind(expr, cont) =
        bind cont expr
    member __.Return x =
        return' x
    member __.Zero () =
        return' ()
    member __.ReturnFrom x =
        x
    member this.Delay (cont) =
        this.Bind (this.Return (), cont)
    member __.Combine (expr1, expr2) =
        combine expr1 expr2
    member this.Yield x =
        this.Return x
    member __.TryFinally (expr, compensation) =
        tryFinally expr compensation
    member __.TryWith (expr, handler) =
        tryWith expr handler
    member this.Using (res : #System.IDisposable, body) =
        this.TryFinally (body res, (fun () ->
            match res with
            | null -> ()
            | disp -> disp.Dispose()))
    member this.While (guard, expr) =
        match guard () with
        | true -> this.Bind(expr, fun () -> this.While (guard, expr))
        | _ -> this.Zero ()
    member this.For (sequence : seq<'a>, body) =
        this.Using (sequence.GetEnumerator(), fun enum ->
            this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))

let action = ActionBuilder()

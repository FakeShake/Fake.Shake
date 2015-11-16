[<AutoOpen>]
module Fake.Shake.Control

open System.Threading.Tasks
open Fake
open Fake.Shake.Core
open FSharp.Control.AsyncLazy
open Nessos.FsPickler

let binary = FsPickler.CreateBinarySerializer()

let bind (continuation : 'a -> Action<'b>) (expr : Action<'a>) : Action<'b> =
    fun state ->
        async {
            let! results = (expr state).Force()
            let state', result = results
            return! (continuation result state').Force()
        } |> AsyncLazy.Create

let (>>=) expr continuation =
    bind continuation expr

let return' x =
    fun state ->
        AsyncLazy.CreateFromValue (state, x)

let map f act =
    bind (f >> return') act

let combine expr1 expr2 =
    expr1 |> bind (fun () -> expr2)

let liftAsync (expr : Async<'a>) : Action<'a> =
    fun state ->
        async {
            let! a = expr
            return state, a
        } |> AsyncLazy.Create

let liftTask (expr : Task<'a>) : Action<'a> =
    fun state ->
        async {
            let! a = Async.AwaitTask expr
            return state, a
        } |> AsyncLazy.Create

let liftTask' (expr : Task) : Action<unit> =
    fun state ->
        let cont (t : Task) =
            match t.IsFaulted with
            | true -> raise t.Exception
            | false -> ()
        async {
            do!
                expr.ContinueWith cont
                |> Async.AwaitTask
            return state, ()
        } |> AsyncLazy.Create

let tryWith (expr : Action<_>) (handler : exn -> Action<_>) =
    fun state ->
        async {
            try
                return! (expr state).Force()
            with
            | e ->
                return! (handler e state).Force()
        } |> AsyncLazy.Create

let tryFinally (expr : Action<_>) comp : Action<_> =
    fun state ->
        async {
            try
                return! (expr state).Force()
            finally
                comp()
        } |> AsyncLazy.Create

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
        async {
            tracefn "Skipped %A, valid stored result" key
            let pickle = state.OldResults.[key] |> AsyncLazy.CreateFromValue
            state.Results.GetOrAdd(key, pickle) |> ignore
        }
    | false ->
        async {
            tracefn "No valid stored value for %A, running" key
            let rule = State.find state key
            let (result : AsyncLazy<State * 'a>) =
                State.clearDeps key state
                rule.Action key state
            let pickled =
                result
                |> AsyncLazy.map (fun (_, r) -> binary.Pickle r)
            state.Results.GetOrAdd(key, pickled) |> ignore
        }

let require<'a> key : Action<'a> =
    fun state ->
        async {
            let state =
                state
                |> State.push key
            do! run<'a> key state
            let! state, result =
                state.Results.[key]
                |> (fun bytes -> 
                        async { 
                            let! b = bytes.Force()
                            return state, binary.UnPickle b })
            return (State.pop state, result)
        } |> AsyncLazy.Create

let requires<'a> keys : Action<'a list> =
    fun state ->
        keys
        |> Seq.map (fun k -> require<'a> k state)
        |> AsyncLazy.Parallel
        |> AsyncLazy.map Seq.toList
        |> AsyncLazy.map (List.map snd)
        |> AsyncLazy.map (fun results -> state, results)

let doAll keys : Action<unit> =
    fun state ->
        Seq.map (fun k -> (require<unit> k) state) keys
        |> AsyncLazy.Parallel
        |> AsyncLazy.map (fun _ -> state, ())

let need key : Action<unit> =
    require<ContentHash> key
    |> map ignore

let needs keys : Action<unit> =
    requires<ContentHash> keys
    |> map ignore

type ActionBuilder () =
    member __.Bind(expr, cont) =
        bind cont expr
    member __.Bind(expr, cont) =
        bind cont (liftAsync expr)
    member __.Bind(expr, cont) =
        bind cont (liftTask expr)
    member __.Bind(expr, cont) =
        bind cont (liftTask' expr)
    member __.Return x =
        return' x
    member __.Zero () =
        return' ()
    member __.ReturnFrom x =
        x
    member __.ReturnFrom x =
        liftAsync x
    member __.ReturnFrom x =
        liftTask x
    member __.ReturnFrom x =
        liftTask' x
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
    member this.While (guard, expr : Action<_>) =
        match guard () with
        | true -> this.Bind(expr, fun () -> this.While (guard, expr))
        | _ -> this.Zero ()
    member this.For (sequence : seq<'a>, body) =
        this.Using (sequence.GetEnumerator(), fun enum ->
            this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))

let action = ActionBuilder()

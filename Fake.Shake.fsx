#if INTERACTIVE
#r "packages/FAKE/tools/FakeLib.dll"
#r "packages/FsPickler/lib/net45/FsPickler.dll"
#else
module Fake.Shake
#endif
open Nessos.FsPickler

let binary = FsPickler.CreateBinary()

type Key = | Key of string
type IRule =
    abstract member Provides : Key -> bool
type [<NoEquality>][<NoComparison>] Rule<'a> =
    {
        Action : Key -> Action<'a>
        Provides : Key -> bool
        ValidStored : Key -> byte [] -> bool
    }
    interface IRule with
        member this.Provides k = this.Provides k
and Action<'a> = State -> State * Lazy<'a>
and [<NoComparison>] State =
    {
      Rules : seq<IRule>
      Results : Map<Key, Lazy<byte []>>
    }
    static member find<'a> state key =
        match Map.tryFind key state.Results with
        | Some result -> state, lazy (result.Force() |> binary.UnPickle)
        | None ->
            let rule =
                try
                    state.Rules
                    |> Seq.filter (fun r -> r.Provides key)
                    |> Seq.choose (fun r -> match r with :? Rule<'a> as r' -> Some r' | _ -> None)
                    |> Seq.head
                with
                | _ ->
                    failwithf "Unable to find rule matching key %A and returning type %A" key typeof<'a>
            let (state', result) = rule.Action key state
            let pickled =
                lazy
                    result.Force()
                    |> binary.Pickle
            { state' with Results = Map.add key pickled state'.Results }, result

let bind (continuation : 'a -> Action<'b>) (expr : Action<'a>) : Action<'b> =
    fun state ->
        let state', result = (expr state)
        continuation (result.Force()) state'

let (>>=) expr continuation =
    bind continuation expr

let return' x =
    fun state ->
        state, lazy x

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

let require<'a> key : Action<'a> =
    fun state ->
        State.find<'a> state key

let requires<'a> keys : Action<'a list> =
    let rec inner keys (values : Action<'a list>) =
        match keys with
        | key::t ->
            values
            >>= (fun values' ->
                    require<'a> key
                    >>= fun (value' : 'a) ->
                            return' <| value'::values')
            |> inner t
        | [] -> values |> map (List.rev)
    inner keys (return' List.empty<'a>)

let need key : Action<unit> =
    require<unit> key

let needs keys : Action<unit> =
    requires<unit> keys
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

let build rules key =
    let old =
        try
            System.IO.File.ReadAllBytes ".fake.shake.cache"
            |> binary.UnPickle
            |> Some
        with
        | _ -> None
    let state = { Rules = rules; Results = match old with Some r -> r | None -> Map.empty }
    let finalState, buildComputation = (State.find state key)
    let result = buildComputation.Force()
    System.IO.File.WriteAllBytes(".fake.shake.cache", binary.Pickle finalState.Results)
    result


let readLines fileName =
  action {
       do! need (Key fileName)
       return System.IO.File.ReadAllLines fileName |> Array.toList
    }

module DefaultRules =
    open System.IO
    open Fake

    let defaultFile =
        {
            Action = fun (Key k) -> action { tracefn "Found file %s" k; return () }
            Provides = fun (Key k) -> File.Exists k
            ValidStored = fun _ _ -> true
        }

    let defaultDir =
        {
            Action = fun (Key k) -> action { tracefn "Found directory %s" k; return () }
            Provides = fun (Key k) -> Directory.Exists k
            ValidStored = fun _ _ -> true
        }

    let allDefaults : IRule list =
        [defaultFile;defaultDir]

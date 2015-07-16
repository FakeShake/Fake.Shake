#if INTERACTIVE
#r "packages/Hopac/lib/net45/Hopac.Core.dll"
#r "packages/Hopac/lib/net45/Hopac.dll"
#else
module Fake.Shake.Core
#endif
open System
open System.Collections.Concurrent
open Hopac

type Key =
    | Key of string
type ContentHash =
    | ContentHash of byte []
type Result<'a> =
    | Checked of 'a
    | UnChecked of 'a
type IRule =
    abstract member Provides : Key -> bool
    abstract member ValidStored : Key -> byte [] -> bool
type [<NoEquality>][<NoComparison>] Rule<'a> =
    {
        Action : Key -> Action<'a>
        Provides : Key -> bool
        ValidStored : Key -> byte [] -> bool
    }
    interface IRule with
        member this.Provides k = this.Provides k
        member this.ValidStored key bytes = this.ValidStored key bytes
and Action<'a> = State -> Promise<State * 'a>
and [<NoComparison>] State =
    {
      Rules : seq<IRule>
      Results : ConcurrentDictionary<Key, Promise<byte []>>
      OldResults : Map<Key, byte[]>
      Current : Key option
      Dependencies : ConcurrentDictionary<Key, Key list>
      Stack : Key list
    }
    static member rawFind state key =
        let providers =
            state.Rules
            |> Seq.filter (fun r -> r.Provides key)
        match providers with
        | x when Seq.isEmpty x -> None
        | ps ->
            Seq.head ps |> Some
    static member find<'a> state key =
        match State.rawFind state key with
        | Some irule ->
            match irule with
            | :? Rule<'a> as rule -> rule
            | _ ->
                failwithf "The first rule matching %A does not provide requested type %A" key typeof<'a>
        | None ->
            failwithf "Unable to find rule matching %A" key
    static member addDep key dep state =
        state.Dependencies.AddOrUpdate(key, [dep], fun _ ks -> dep::ks |> Seq.distinct |> Seq.toList)
        |> ignore
    static member clearDeps key state =
        state.Dependencies.AddOrUpdate(key, [], fun _ _ -> [])
        |> ignore
    static member push key state =
        match state.Current with
        | None -> ()
        | Some k ->
            State.addDep k key state
        { state with Stack = key::state.Stack; Current = Some key }
    static member pop state =
        match state.Stack with
        | _::previous::rest ->
            { state with Stack = previous::rest; Current = Some previous }
        | _ ->
            { state with Stack = []; Current = None }

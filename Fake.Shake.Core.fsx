#if INTERACTIVE
#else
module Fake.Shake.Core
#endif

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
and Action<'a> = State -> State * Lazy<'a>
and [<NoComparison>] State =
    {
      Rules : seq<IRule>
      Results : Map<Key, Lazy<byte []>>
      OldResults : Map<Key, byte[]>
      Current : Key option
      Dependencies : Map<Key, Key list>
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
        let currentDeps =
            match Map.tryFind key state.Dependencies with
            | Some deps -> deps
            | None -> []
        { state with Dependencies = Map.add key (dep::currentDeps |> Seq.distinct |> Seq.toList) state.Dependencies }
    static member clearDeps key state =
        { state with Dependencies = Map.add key [] state.Dependencies }
    static member push key state =
        match state.Current with
        | None ->
            { state with Stack = key::state.Stack; Current = Some key }
        | Some k ->
            { state with Stack = key::state.Stack; Current = Some key }
            |> State.addDep k key
    static member pop state =
        match state.Stack with
        | _::previous::rest ->
            { state with Stack = previous::rest; Current = Some previous }
        | _ ->
            { state with Stack = []; Current = None }

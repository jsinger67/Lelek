namespace LelekParser

module CompiledLADfa =
    open Token
    open LADfa
    
    /// Transition
    type Tr =
        | Tr of Tok: int * Next: int

    module Tr =
        open LAAutomaton

        let ofTransition (stateTranslateTable: Map<Set<int>, int>) (DfaTransition(Item = i; ToState = s)): Tr =
            Tr(Tok = LAItem.tokenType i, Next = stateTranslateTable.[s])

    type State = {
        Id: int
        Transitions: Tr array
        Accepted: bool
        /// This is the relative index of the concluded rule in the rule set of a given non-terminal.
        Prediction: int
    }

    module State =
        let ofDfaState (stateTranslateTable: Map<Set<int>, int>) composedId ({DfaState.Transitions = ts; Accepted = a; NfAStates = ns}): State =
            {
                Id = stateTranslateTable.[composedId]
                Transitions = ts |> Array.ofList |> Array.map (Tr.ofTransition stateTranslateTable)
                Accepted = a
                Prediction =
                    if a then
                        ns |> List.filter (fun (i, s) -> i <> 0 && s.Accepted) |> List.map (fun (i, _) -> (i >>> 16) - 1) |> List.head
                    else
                        -1  // No prediction in non-accepting state
            }

    type CompiledDfa =
        State array

    module CompiledDfa =
        let empty = [||]

        /// Note: This is not a complete DFA minimization but the most effective one.
        ///       Here we could improve further.
        let minimize (cdfa: CompiledDfa): CompiledDfa =
            // Group the states into accepting and non-accepting ones
            let acc, nacc =
                cdfa
                |> Array.partition (fun s -> s.Accepted)

            // Group the accepting states into those with the same prediction and the same transitions
            let equGroups =
                acc
                |> Array.groupBy (fun s -> (s.Prediction, s.Transitions))
                |> Array.map snd

            // Create a new state for each group
            let newStates =
                equGroups
                |> Array.map (fun g -> g |> Array.minBy (fun s -> s.Id))

            let idMapping =
                Array.zip
                    (equGroups |> Array.map(fun ss -> ss |> Array.map (fun s -> s.Id)))
                    (newStates |> Array.map(fun s -> s.Id))

            let findNewId id =
                idMapping
                |> Array.tryFind (fun (ids, _) -> ids |> Array.contains id)
                |> Option.map (fun (_, id) -> id)
                |> Option.defaultValue id

            newStates
            |> Array.append nacc
            |> Array.map (fun s ->
                { s with
                    Id = findNewId s.Id
                    Transitions = s.Transitions |> Array.map (fun (Tr(t, n)) -> Tr(t, findNewId n))
                })


        let ofDfa (dfa: Dfa): CompiledDfa =
            let stateTranslateTable =
                dfa
                |> Map.toSeq
                |> Seq.mapi (fun i (k, _) -> k, i)
                |> Map.ofSeq
            dfa
            |> Map.toList
            |> Array.ofList
            |> Array.map (fun (k, v) -> State.ofDfaState stateTranslateTable k v)
            |> minimize

        let eval (lookahead: int -> Token) (printTokenError: State -> Token -> unit) kMax (cdfa: CompiledDfa): int =
            let rec _eval s k =
                if k > kMax then
                    -1
                else
                    match cdfa with
                    | [||] ->
                        -1  // Possibly wrong index in rule array!
                    | _ ->
                        let thisState = cdfa |> Array.find (fun ({Id = id}) -> id = s)
                        let tok = k |> lookahead
                        let nT = tok |> Token.typeOf
                        let toState =
                            thisState.Transitions
                            |> Array.tryFind (fun (Tr(Tok = t)) -> t = nT)
                            |> Option.map (fun (Tr(Next = toState)) -> toState)
                            |> Option.defaultValue -1
                        if toState = -1 then
                            if thisState.Accepted then
                                thisState.Prediction
                            else
                                printTokenError thisState tok
                                -1
                        else
                            _eval toState (k + 1)

            _eval 0 0

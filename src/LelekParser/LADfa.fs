namespace LelekParser

module LADfa =
    open LANfa
    open LAAutomaton
    open Token

    let intSetToFs s =
        s
        |> Set.map (sprintf ".Add(0x%x)")
        |> String.concat ""
        |> sprintf "Set.empty%s"

    type DfaTransition =
        | DfaTransition of Item: LAItem * ToState: Set<int>

    type DfaState = {
        Transitions: DfaTransition list
        Accepted: bool
        /// Used for disambiguation when state is accepted and multiple states contributed to it.
        NfAStates: (int * NfaState) list
    }

    module DfaState =
        let newState = {
                Transitions = List.empty
                Accepted = false
            }

        let addTransition (s: DfaState) (t: DfaTransition) =
            if s.Transitions |> List.contains t then
                s
            else
                { s with Transitions = s.Transitions @ [t] }

        let getAcceptingUnionPart  (s: DfaState) : Result<int, string> =
            if s.Accepted |> not then
                Error "Not an accepting state!"
            else
                let contributers =
                    s.NfAStates
                    |> List.filter (fun (k, s) ->
                        // Note that state 0 is no contributing NFA state.
                        // It's just the in-connector for the union.
                        // It is 'accepting' only as a result of other NFA states in the
                        // epsilon closure of state 0.
                        // Those other NFA states are the real contributors.
                        k <> 0 && s.Accepted)

                match contributers with
                | [] -> Error "No contributers!"
                | [(i, _)] ->
                    if i >= 0 then
                        Ok ((i >>> 16) - 1)
                    else
                        Error "Invalid contributor!"
                | _ ->
                    let modules =
                        contributers
                        |> List.map (fst >> (fun i -> (i >>> 16) - 1))
                    if (modules |> List.min) = (modules |> List.max) && ((modules |> List.min) >= 0) then
                        Ok (modules.Head)
                    else
                        Error ("Ambiguous contributors - internal error!")

    type Dfa = Map<Set<int>, DfaState>

    module Dfa =
        let empty =
            Map.empty

        let findState (dfa: Dfa) (k: Set<int>) =
            dfa.[k]

        let addState (k: Set<int>) (state: DfaState) (dfa: Dfa) =
            dfa
            |> Map.add k state

        let replaceState (k: Set<int>) (s: DfaState) (dfa: Dfa) =
            dfa
            |> Map.remove k
            |> Map.add k s


        let ofNfa (nfa: Nfa): Dfa =
            let items = Nfa.alphabet nfa
            let startState = Set.singleton 0

            let addIfNotExists mp k v =
                if mp |> Map.containsKey k then
                    mp
                else
                    mp |> addState k v

            let containsAcceptingNfaState (nfa: Nfa) (dfaState: Set<int>): bool =
                dfaState
                |> Nfa.epsilonClosureOfStates nfa
                |> Set.exists (fun n -> Nfa.isAcceptingState n nfa)

            let getNfaStates (keys: Set<int>): (int * NfaState) list =
                let keys' = Nfa.epsilonClosureOfStates nfa keys |> List.ofSeq
                List.zip
                    keys'
                    (List.zip 
                        (keys'
                        |> List.map (fun i -> Nfa.epsilonClosure nfa i |> containsAcceptingNfaState nfa))
                        (keys'
                        |> List.map (Nfa.findState nfa)))
                    |> List.map (fun (i, (a,s)) -> i, { s with Accepted = a })

            let buildNewState key = {
                    Transitions = List.empty
                    Accepted = containsAcceptingNfaState nfa key
                    NfAStates = getNfaStates key
                }

            let startDfa = 
                Map.empty
                |> Map.add startState (startState |> buildNewState)

            let rec buildDFA (fromStateKey: Set<int>) (currentDfa: Dfa) =
                let newDfa =
                    items
                    |> Set.fold (fun acc elem ->
                        let newStateKey = Nfa.followerStateViaItem nfa fromStateKey elem
                        if newStateKey |> Set.isEmpty then
                            acc
                        else
                            let acc1 =
                                newStateKey
                                |> buildNewState
                                |> addIfNotExists acc newStateKey
                            let fromState = findState acc fromStateKey
                            let newTransition = DfaTransition(Item = elem, ToState = newStateKey)
                            let modifiedCurrentState = DfaState.addTransition fromState newTransition
                            replaceState fromStateKey modifiedCurrentState acc1
                    ) currentDfa

                if newDfa = currentDfa then
                    currentDfa
                else
                    let newStates =
                        newDfa
                        |> Map.filter (fun k _ ->
                            currentDfa |> Map.containsKey k |> not
                        )
                        |> Map.toList
                        |> List.map fst
                    newStates
                    |> List.fold (fun acc elem ->
                        buildDFA elem acc
                    ) newDfa

            buildDFA startState startDfa


        let longestPath (dfa: Dfa): int =
            let rec traverse (depth: int) (num: Set<int>): int =
                let children = dfa.[num].Transitions
                let depth' = depth + 1
                let pathDepths =
                    children
                    |> List.map (fun elem ->
                        let (DfaTransition(ToState = toState)) = elem
                        traverse depth' toState)
                match pathDepths with
                | [] -> depth
                | _ -> pathDepths |> List.max

            0
            |> Set.singleton
            |> traverse 0


        let eval (lookahead: int -> Token) (printTokenError: DfaState -> Token -> unit) kMax (dfa: Dfa): int =
            let rec _eval s k =
                if k > kMax then
                    -1
                else
                    let thisState = dfa.[s]
                    let nT = lookahead k
                    let toState =
                        thisState.Transitions
                        |> List.tryFind (fun (DfaTransition(Item = item)) -> item = (nT |> LAItem.ofToken))
                        |> Option.map (fun (DfaTransition(ToState = toState)) -> toState)
                        |> Option.defaultValue Set.empty
                    if toState.IsEmpty then
                        if thisState.Accepted then
                            match thisState |> DfaState.getAcceptingUnionPart with
                            | Ok part   -> part
                            | _         -> -1
                        else
                            printTokenError thisState nT
                            -1
                    else
                        _eval toState (k + 1)

            (0 |> Set.singleton |> _eval) 0


namespace LelekParser

module LANfa =
    open LAAutomaton

    type Transition =
        | Transition of Item: LAItem * ToState: int

    module Transition =
        let isEpsilonTransition (t: Transition) =
            let (Transition(Item = i)) = t
            match i with
            | Eps -> true
            | _ -> false

    type NfaState = {
        Transitions: Transition list
        Accepted: bool
    }

    module NfaState =
        let newState = {
                Transitions = List.empty
                Accepted = false
            }

        let addTransition (s: NfaState) (t: Transition) =
            if s.Transitions |> List.contains t then
                s
            else
                { s with Transitions = s.Transitions @ [t] }

        let makeAccepting (s: NfaState) =
            { s with Accepted = true }

    type Nfa = Map<int, NfaState>

    module Nfa =
        let findState (nfa: Nfa) (i: int) =
            nfa.[i]

        let addState (n: int) (state: NfaState) (nfa: Nfa) =
            nfa
            |> Map.add n state

        let replaceState (n: int) (s: NfaState) (nfa: Nfa) =
            nfa
            |> Map.remove n
            |> Map.add n s

        let isAcceptingState (n: int) (nfa: Nfa) =
            let state = findState nfa n
            state.Accepted

        let addStateOffset (offset: int) (nfa: Nfa) : Nfa =
            nfa
            |> Map.toList
            |> List.map (fun (k, v) ->
                k + offset, {
                    v with
                        Transitions =
                            v.Transitions
                            |> List.map (fun (Transition(Item = i; ToState = t)) -> Transition(Item = i, ToState = t + offset))
                }
            )
            |> Map.ofList

        let makeUnion (nfas: Nfa list): Nfa =
            let separatedNfas =
                nfas
                |> List.mapi (fun i n -> addStateOffset ((i + 1) <<< 16) n, (i + 1) <<< 16)

            let startState =
                let transitions =
                    separatedNfas
                    |> List.map (snd >> (fun toState -> Transition(Eps, toState)))
                { NfaState.newState with Transitions = transitions }

            let composedNfa =
                separatedNfas
                |> List.collect (fst >> (fun nfa -> nfa |> Map.toList))
                |> Map.ofList
                |> addState 0 startState

            composedNfa

        let alphabet (nfa: Nfa): Set<LAItem> =
            let isToken = function
                | Te _ | Eos -> true
                | _ -> false

            nfa
            |> Map.fold (fun acc _ v ->
                v.Transitions
                |> List.map (fun (Transition(Item = i)) -> i)
                |> List.filter isToken
                |> Set.ofList
                |> Set.union acc
            ) Set.empty

        let epsilonClosure (nfa: Nfa) (n: int): Set<int> =
            let findState = findState nfa

            let rec collect (n: int) (states: Set<int>) : Set<int> =
                let s0 = findState n
                let toStates =
                    n::
                    (s0.Transitions
                    |> List.filter (Transition.isEpsilonTransition)
                    |> List.map (fun (Transition(ToState = ts)) -> ts))
                    |> Set.ofList

                let closure =
                    states
                    |> Set.union toStates

                if closure <> states then
                    toStates
                    |> Set.fold (fun acc elem ->
                        collect elem acc
                    ) closure
                else
                    closure

            collect n Set.empty

        let epsilonClosureOfStates (nfa: Nfa) (states: Set<int>): Set<int> =
            states
            |> Set.fold (fun acc elem ->
                epsilonClosure nfa elem
                |> Set.union acc
            ) Set.empty

        let followerStateViaItem (nfa: Nfa) (states: Set<int>) (item: LAItem): Set<int> =
            let findState = findState nfa
            states
            |> epsilonClosureOfStates nfa
            |> Set.map findState
            |> Set.map (fun s -> s.Transitions |> List.filter (fun (Transition(Item = i)) -> i = item))
            |> Set.filter (fun ts -> ts.Length > 0)
            |> Set.fold (fun acc elem ->
                elem
                |> List.fold (fun acc (Transition(ToState = ts)) ->
                    acc |> Set.add ts
                ) acc            
            ) Set.empty

        let minimizeNfa (nfa: Nfa) : Nfa =
            let reachableStates =
                let collectReachables (reachables: Set<int>): Set<int> =
                    reachables
                    |> Set.fold (fun acc elem ->
                        let state = nfa.Item elem
                        state.Transitions
                        |> List.fold (fun acc elem ->
                            let (Transition(ToState = t)) = elem
                            acc |> Set.add t
                        ) acc
                    ) reachables
                Utils.doWhileChanging
                    collectReachables (0 |> Set.singleton)

            let isReachable (state: int): bool =
                reachableStates |> Set.contains state

            nfa
            |> Map.filter (fun k _ -> isReachable k)

        let ofLAData (k: int) (data: LAData): Nfa =
            let mutable lastNumber = 0

            let newNumber() =
                lastNumber <- lastNumber + 1
                lastNumber
        
            let rec crunchData (tokenCount: int) (data: LAData) (nfa: Nfa) (fromNumber: int): Nfa * int * int =

                match data with
                | Item i ->
                    let fromState = findState nfa fromNumber
                    let newStateNum = newNumber()
                    let newState = NfaState.newState
                    let modifiedFromState = NfaState.addTransition fromState (Transition(Item = i, ToState = newStateNum))
                    let nfa1 = 
                        nfa
                        |> addState newStateNum newState
                        |> replaceState fromNumber modifiedFromState
                    let newTokenCount = if LAItem.isPhysical i then (tokenCount + 1) else tokenCount
                    nfa1, newStateNum, newTokenCount
                | Union u ->
                    match u.Count with
                    | 0 ->
                        nfa, fromNumber, tokenCount
                    | 1 ->
                        let item = u |> List.ofSeq |> List.head
                        crunchData tokenCount item nfa fromNumber
                    | _ ->
                        let endStateNum = newNumber()
                        let endState = NfaState.newState
                        let nfa1 =
                            nfa
                            |> addState endStateNum endState
                        let nfa2, newTokenCount =
                            u
                            |> Set.fold (fun (acc, tc) elem ->
                                let newStateNum = newNumber()
                                let newState = NfaState.newState
                                let fromState = findState acc fromNumber
                                let modifiedFromState = NfaState.addTransition fromState (Transition(Eps, ToState = newStateNum))
                                let nfa1 =
                                    acc
                                    |> addState newStateNum newState
                                    |> replaceState fromNumber modifiedFromState
                                let nfa2, n, newTokenCount = crunchData tokenCount elem nfa1 newStateNum
                                let endState = findState nfa2 n
                                let modifiedLastState =
                                    if newTokenCount < k then
                                        NfaState.addTransition endState (Transition(Eps, ToState = endStateNum))
                                    else
                                        NfaState.makeAccepting endState
                                nfa2
                                |> replaceState n modifiedLastState, newTokenCount
                            ) (nfa1, tokenCount)
                        nfa2, endStateNum, newTokenCount
                | Concat c ->
                    let nfa1 = 
                        c
                        |> List.fold (fun (dacc, sacc, tc) elem ->
                            crunchData tc elem dacc sacc
                        ) (nfa, fromNumber, tokenCount)
                    nfa1

        
            let startNfa =
                Map.empty
                |> Map.add lastNumber (NfaState.newState)
            let endNfa, endNumber, tokenCount = crunchData 0 data startNfa lastNumber
            let acceptedState = { findState endNfa endNumber with Accepted = true }
            endNfa
            |> replaceState endNumber acceptedState
            |> minimizeNfa


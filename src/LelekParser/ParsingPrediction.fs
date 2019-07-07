namespace LelekParser

module ParsingPrediction =
    open LinLlkGrammar
    open First1
    open LANfa
    open LADfa
    open ParserGenLogger

    type First1SetOfRule = | First1SetOfRule of Rule: LinLlkRule * F1Set: Set<LinLlkSymbol>

    [<RequireQualifiedAccess>]
    module First1SetOfRule =
        let ruleOf (First1SetOfRule(Rule = rl)) =
            rl
        let f1SetOf (First1SetOfRule(F1Set = f1)) =
            f1

    type LL1ConfilctForRuleSet = First1SetOfRule list

    type LL1Conflicts = LL1ConfilctForRuleSet list


    let logFirstKSet (logger: Logger) (msg: string) (f1: First1SetOfRule list) =
        let symbolSetToString s =
            s
            |> Seq.map (LinLlkSymbol.toString)
            |> String.concat " | "
        logger.log msg
        f1
        |> List.iter (fun (First1SetOfRule(r, s)) ->
            System.String('-', 40) |> logger.log
            "Rule:" |> logger.log
            r |> LinLlkRule.toShortString |> logger.log
            s |> symbolSetToString |> logger.log
        )

    let findLL1Conflicts (logger: Logger) (g: LinLlkData): LL1Conflicts =
        let gAugmented = LinLlkData.augmentWithEnds g 10
        let nullables = LinLlkData.calcNullables gAugmented
        let f1s = calcFirst1Sets gAugmented
        let conflictsInRule (rules: LinLlkRule list): LL1ConfilctForRuleSet =
            let firstSetTuples =
                rules
                |> List.map (calculateFirstFollow1 f1s gAugmented nullables)
                |> List.zip rules
                |> List.map (fun (r,s) -> First1SetOfRule(r, s))


            System.String('(', 40) |> logger.log
            firstSetTuples
            |> logFirstKSet logger "First1SetTupels"
            System.String(')', 40) |> logger.log

            List.zip
                (firstSetTuples
                    |> List.mapi (fun i (First1SetOfRule(F1Set = s)) ->
                        firstSetTuples
                        |> List.mapi (fun j (First1SetOfRule(F1Set = st)) ->
                            if i = j then
                                true    // No intersection
                            else
                                let distinct = Set.intersect s st |> Set.isEmpty
                                distinct
                        )
                        |> List.forall id
                    ))
                firstSetTuples
            |> List.filter (fun (distinct, _) -> not distinct)
            |> List.map snd

        gAugmented
        |> LinLlkData.varNames
        |> List.map (LinLlkData.matchingRules gAugmented)
        |> List.filter (fun rules -> rules.Length > 1)  // Conflicts on a single rule impossible.
        |> List.fold (fun acc elem ->
            let conflicts = conflictsInRule elem
            match conflicts with
            | [] -> acc
            | _ -> conflicts :: acc
        ) List.empty
        |> List.rev

    let canResolveConflicts (bnf: LinLlkData) k (conflict: LL1ConfilctForRuleSet) =
        let dfaOfUnion =            
            conflict
            |> List.map (fun (First1SetOfRule(sym, _)) -> sym |> LAAutomaton.calcLAData bnf k |> Nfa.ofLAData k)
            |> Nfa.makeUnion
            |> Dfa.ofNfa
        
        // Check if all accepted state can be asked for a single contributing union part
        let ok =
            dfaOfUnion
            |> Map.filter (fun _ v -> v.Accepted)
            |> Map.forall (fun _ v -> match LADfa.DfaState.getAcceptingUnionPart v with | Ok _ -> true | _ -> false)
        let actualK =
            if ok then
                dfaOfUnion |> Dfa.longestPath
            else
                k
        ok, actualK


    let lookaheadToPredict (kMax: int) (bnf: LinLlkData) (conflict: LL1ConfilctForRuleSet): bool * (int * int) =
        let rec tryToDecide k =
            if k < kMax then
                let ok, maxk = conflict |> canResolveConflicts bnf k
                if ok then
                    true, (k, maxk)
                else
                    tryToDecide (k + 1)
            else
                false, (k, k)

        tryToDecide 1


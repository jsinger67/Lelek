namespace LelekParser

#nowarn "40"

module First1 =
    open LinLlkGrammar

    /// -------------------------------------------------------------------------
    /// Types and their modules for FIRST1(x) related functions
    type First1Set =
        | First1Set of Symbol: LinLlkSymbol * Followers: Set<LinLlkSymbol>

    module First1Set =
        let toTupel fks =
            let (First1Set(Symbol = sym; Followers = flwrs)) = fks
            sym, flwrs

    type First1Sets = Map<LinLlkSymbol, Set<LinLlkSymbol>>

    /// -------------------------------------------------------------------------
    /// Calculates the FIRST1(x) set of a symbol.
    let calcFirst1Set (g: LinLlkData) (nullables: string list) (sym: LinLlkSymbol) =

        let rec calcFirst1SetOfVariable (sym: LinLlkSymbol): Set<LinLlkSymbol> =
            let s = LinLlkSymbol.vName sym
            let f1s =
                LinLlkData.matchingRules g s
                |> List.fold (fun acc elem ->
                    let (LinLlkRule(Prod = symbols)) = elem
                    calcFirstSetOfSymbols symbols symbols acc
                ) Set.empty
            f1s

        and calcFirstSetOfSymbol (sym: LinLlkSymbol): Set<LinLlkSymbol> =
            match sym with
            | LinLlkEnd
            | LinLlkEpsilon
            | LinLlkTerminal _ ->
                Set.singleton sym
            | LinLlkVariable _ ->
                calcFirst1SetOfVariable sym

        and calcFirstSetOfSymbols (symbols: LinLlkSymbol list) (symbolsRest: LinLlkSymbol list) (currentSet: Set<LinLlkSymbol>): Set<LinLlkSymbol> =
            match symbolsRest with
            | [] ->
                let isNullable sym =
                    LinLlkSymbol.isEpsilon sym ||
                    LinLlkSymbol.isVariable sym && nullables |> List.contains (LinLlkSymbol.vName sym)
                    // This is equivalent!
                    //(memoizedCalcFirstSetOfSymbol sym) |> Set.contains LinLlkEpsilon

                let allNullable =
                    nullables.Length = 1 && LinLlkSymbol.isEpsilon (symbols.Head) ||
                    symbols |> List.forall isNullable

                // Only if epsilon is in all FIRST1(x) sets of symbols add Epsilon here!
                if allNullable then
                    currentSet + Set.singleton LinLlkEpsilon
                else
                    currentSet
            | h::t  ->
                let fksh = calcFirstSetOfSymbol h
                let currentSet' = currentSet + fksh
                match h with
                | LinLlkVariable _ ->
                    if fksh |> Set.contains LinLlkEpsilon then
                        calcFirstSetOfSymbols symbols t (currentSet' - Set.singleton LinLlkEpsilon)
                    else
                        currentSet'
                | LinLlkEpsilon ->
                    calcFirstSetOfSymbols symbols t currentSet'
                | _ ->
                    currentSet'

        if LinLlkData.detectLeftRecursions g |> List.isEmpty |> not then
            failwith "Can't process left recursive grammar!"

        First1Set(Symbol = sym, Followers = calcFirst1SetOfVariable sym)


    /// -------------------------------------------------------------------------
    /// Calculates the FIRST1(x) set of a production (RHS of a rule).
    let calcFirst1SetOfProduction (g: LinLlkData) (nullables: string list) (rule: LinLlkRule) =
        let rec calcFirst1SetOfVariable (sym: LinLlkSymbol): Set<LinLlkSymbol> =
            let s = LinLlkSymbol.vName sym
            LinLlkData.matchingRules g s
            |> List.fold (fun acc elem ->
                let (LinLlkRule(Prod = symbols)) = elem
                calcFirstSetOfSymbols symbols symbols acc
            ) Set.empty

        and calcFirstSetOfSymbol (sym: LinLlkSymbol): Set<LinLlkSymbol> =
            match sym with
            | LinLlkEnd
            | LinLlkEpsilon
            | LinLlkTerminal _ ->
                Set.singleton sym
            | LinLlkVariable _ ->
                calcFirst1SetOfVariable sym

        and calcFirstSetOfSymbols (symbols: LinLlkSymbol list) (symbolsRest: LinLlkSymbol list) (currentSet: Set<LinLlkSymbol>): Set<LinLlkSymbol> =
            match symbolsRest with
            | [] ->
                let isNullable sym =
                    LinLlkSymbol.isEpsilon sym ||
                    LinLlkSymbol.isVariable sym && nullables |> List.contains (LinLlkSymbol.vName sym)
                    // This is equivalent!
                    //(memoizedCalcFirstSetOfSymbol sym) |> Set.contains LinLlkEpsilon

                let allNullable =
                    nullables.Length = 1 && LinLlkSymbol.isEpsilon (symbols.Head) ||
                    symbols |> List.forall isNullable

                // Only if epsilon is in all FIRST1(x) sets of symbols add Epsilon here!
                if allNullable then
                    currentSet + Set.singleton LinLlkEpsilon
                else
                    currentSet - Set.singleton LinLlkEpsilon
            | h::t  ->
                let fksh = calcFirstSetOfSymbol h
                let currentSet' = currentSet + fksh
                match h with
                | LinLlkVariable _ ->
                    if fksh |> Set.contains LinLlkEpsilon then
                        calcFirstSetOfSymbols symbols t (currentSet' - Set.singleton LinLlkEpsilon)
                    else
                        currentSet'
                | LinLlkEpsilon ->
                    calcFirstSetOfSymbols symbols t currentSet'
                | _ ->
                    currentSet'

        if LinLlkData.detectLeftRecursions g |> List.isEmpty |> not then
            failwith "Can't process left recursive grammar!"

        let (LinLlkRule(Var = vn; Prod = pr)) = rule
        let followers1 = calcFirstSetOfSymbols pr pr Set.empty
        First1Set(Symbol = LinLlkVariable vn, Followers = followers1)


    /// -------------------------------------------------------------------------
    /// Calculates the FIRST1(x) set of all symbols of a given LinLlk data structure.
    let calcFirst1Sets (g: LinLlkData): First1Sets =
        let nullables = LinLlkData.calcNullables g
        let calcFS = calcFirst1Set g nullables
        g
        |> LinLlkData.varNames
        |> List.map (LinLlkVariable >> calcFS >> First1Set.toTupel)
        |> Map.ofList

    /// -------------------------------------------------------------------------
    let calculateFollow1Set (f1s: First1Sets) (g: LinLlkData) (rule: LinLlkRule): Set<LinLlkSymbol> =
        let (LinLlkRule(Var = vName)) = rule

        let rec collectF1 (traversedRules: string list) ruleName =
            if traversedRules |> List.contains ruleName then
                Set.empty
            else
                let (LinLlkData(_, rl, _)) = g
                let traversedRules1 = traversedRules @ [ruleName]
                let  f1 =
                    rl
                    |> List.filter (fun (LinLlkRule(Prod = symbols)) -> symbols |> List.contains (LinLlkVariable ruleName))
                    |> List.map (fun (LinLlkRule(Var = vName; Prod = symbols)) -> vName, symbols)
                    |> List.fold (fun acc (v, s) ->
                        let symIndex = s |> List.findIndex (fun sym -> sym = (LinLlkVariable ruleName))
                        let s' = s |> List.skip (symIndex + 1)
                        let k1 = collectFollowerOfProdRest traversedRules1 v s'
                        acc + k1
                    ) Set.empty
                f1

        and collectFollowerOfProdRest (traversedRules: string list) ruleName prodRest =
            match prodRest with
            | h::t ->
                match h with
                | LinLlkEpsilon    -> collectF1 traversedRules ruleName
                | LinLlkTerminal _ -> h |> Set.singleton
                | LinLlkVariable s ->
                    let fi1 = f1s.[h]
                    if fi1 |> Set.contains LinLlkEpsilon then
                        let traversedRules1 = traversedRules @ [s]
                        let fo1 = collectFollowerOfProdRest traversedRules1 ruleName t
                        fo1 + fi1 - (Set.singleton LinLlkEpsilon)
                    else
                        fi1
                | LinLlkEnd        -> h |> Set.singleton
            | [] ->
                collectF1 traversedRules ruleName

        collectF1 List.empty vName
            
    /// -------------------------------------------------------------------------
    /// Calculates the 1-concatenation of FIRST1(x) and FOLLOW1(x) for a rule x.
    /// This yields the effective set of terminals that can occur at the beginning of
    /// the given rule x.
    let calculateFirstFollow1 (f1s: First1Sets) (g: LinLlkData) (nullables: string list) (rule: LinLlkRule) =

        let fi1 = calcFirst1SetOfProduction g nullables rule

        let (First1Set(Followers = flwrs)) = fi1
        if flwrs |> Set.contains LinLlkEpsilon then
            let fo1 = calculateFollow1Set f1s g rule
            flwrs + fo1 - (Set.singleton LinLlkEpsilon)
        else
            flwrs - (Set.singleton LinLlkEpsilon)

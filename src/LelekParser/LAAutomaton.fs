namespace LelekParser

module LAAutomaton =
    open LinLlkGrammar
    open LexerTerminals
    open Token


    type LAItem =
        /// A terminal token type
        | Te of int
        /// An ε symbol (empty string)
        | Eps
        /// A $ at the end of the grammar used as placeholder for tokens at the end
        | Eos

    module LAItem =
        let toString (printToken: (int -> string)) = function
            | Te s -> printToken s //sprintf @"""%s""" s;
            | Eps -> "ε"
            | Eos -> "$"

        let isPhysical = function
            | Te _ | Eos -> true
            | Eps -> false

        let ofToken = function
            | Token(TokenType = tt) -> Te tt
            | EndOfInput -> Eos

        let tokenType = function
            | Te tt -> tt
            | Eps -> -2
            | Eos -> Token.EndOfInputToken

    type LAData =
        | Item of LAItem
        | Union of Set<LAData>
        | Concat of LAData list

    module LAData =
        let toString (printToken: (int -> string)) (data: LAData) =
            let (+|) a b = if System.String.IsNullOrEmpty a then b else a + "|" + b
            let (+<) a b = if System.String.IsNullOrEmpty a then b else a + "" + b
            let printItem = LAItem.toString printToken

            let rec reduce data res =
                match data with
                | Item i ->
                    res +< (printItem i)
                | Union u ->
                    match u with
                    | s when s.IsEmpty ->
                        res
                    | s when s.Count = 1 ->
                        res +| (reduce (s |> Set.toList |> List.head) "")
                    | _ ->
                        u
                        |> Set.fold (fun acc elem -> acc +| (reduce elem "")) res
                        |> (sprintf "(%s)")
                | Concat c ->
                    match c with
                    | [] ->
                        res
                    | [h] ->
                        res +< (reduce h "")
                    | _ ->
                        c
                        |> List.fold (fun acc elem -> acc +< (reduce elem "")) res
                        |> (sprintf "(%s)")

            reduce data ""

        let isUnion = function
            | Union _ -> true
            | _ -> false

    /// Returns a list of 'production parts' for a given variable which represents the
    /// followers parts of that symbol all over the rules of the grammar.
    /// Note: If a production part after the given variable is empty we must continue to collect
    /// the followers of the rule that is currently processed. This has to be done recursively until
    /// a follower production part is found (this is guaranteed because the grammer is augmented with k*$).
    let collectFollowersProductionParts (g: LinLlkData) (vName: string) : LinLlkSymbol list list =
        let processedRules = System.Collections.Generic.HashSet<string>(HashIdentity.Structural)
        let (LinLlkData(_, rl, _)) = g

        let rec _collectFollowers vName: (string * LinLlkSymbol list) list =
            if processedRules.Add vName then
                let thisVar = (LinLlkVariable vName)
                let followerRules =
                    rl
                    |> List.map (fun rl ->
                        let (LinLlkRule(Var = var; Prod = symbols)) = rl
                        var, symbols)
                    |> List.filter (fun (_, prod) -> prod |> List.contains thisVar)
                    |> List.map (fun (ruleName, prod) ->
                        let prodRest = prod |> List.skipWhile (fun sym -> sym <> thisVar)
                        match prodRest with
                        | s::t when s = thisVar -> ruleName, t
                        | _ -> ruleName, []
                    )
                    |> List.fold (fun acc (ruleName, prod) ->
                        if prod.IsEmpty then
                            acc @ _collectFollowers ruleName
                        else
                            acc @ (List.singleton (ruleName, prod))
                    ) List.empty
                followerRules
            else
                List.empty

        vName
        |> _collectFollowers
        |> List.map snd

    /// Calculates a NFA in regex-like format with integer token types
    let calcLAData (g: LinLlkData) (k: int) (rule: LinLlkRule) : LAData =

        let (+<) (a: LAData) (b: LAData): LAData =
            match b with
            | Concat [] ->
                a
            | _ ->
                match a with
                | Concat ca -> 
                    match b with
                    | Concat cb ->
                        Concat (ca@cb)
                    | _ ->
                        Concat (ca@[b])
                | _ -> Concat [a; b]

        let nameToIndex = nameToIndex g

        let g' = LinLlkData.augmentWithEnds g k

        let rec laRxOfSymbols (currentTree: LAData) (kRest: int) (prodRest: LinLlkSymbol list) : LAData * int =
            if kRest <= 0 then
                currentTree, kRest
            else
                match prodRest with
                | [] ->
                    currentTree, kRest
                | h::t ->
                    let nextLaRx, nextK = laRxOfSymbol h kRest
                    let modLaRx = currentTree +< nextLaRx
                    let tailLaRx, nextNextK = laRxOfSymbols (Concat []) nextK t
                    (modLaRx +< tailLaRx), nextNextK

        and laRxOfSymbol (sym: LinLlkSymbol) (kRest: int) : LAData * int =
            if kRest = 0 then
                (Concat []), kRest
            else
                match sym with
                | LinLlkEnd        -> Eos |> Item, kRest - 1
                | LinLlkEpsilon    -> Eps |> Item, kRest
                | LinLlkTerminal s -> s |> nameToIndex |> Te |> Item, kRest - 1
                | LinLlkVariable s ->
                    let varTree, kRestRest = laRxOfVariable s kRest
                    varTree, kRestRest

        and laRxOfVariable (vName: string) (kRest: int) : LAData * int =
            let laRxs =
                vName
                |> LinLlkData.matchingRules g'
                |> List.map ((fun (LinLlkRule(Prod = prod)) -> prod) >> (laRxOfSymbols (Concat []) kRest))
            
            // "Or'ed" alternatives
            let alternatives =
                laRxs
                |> List.map fst
                |> Set.ofList
                |> Union

            // The earliest exit value (the biggest k) must be taken
            let kNext =
                match laRxs with
                | [] -> 1
                | _ ->
                    laRxs
                    |> List.map snd
                    |> List.max

            alternatives, kNext

        and laRxOfRule rule =
            let (LinLlkRule(Var = vName; Prod = symbols)) = rule
            let laRxs, kRest = laRxOfSymbols (Concat []) k symbols
            if (kRest > 0) then
                // Consider all followers of sym!!!
                let childSubRules =
                    vName
                    |> collectFollowersProductionParts g'
                    |> List.map (fun syms ->
                        let laRxs, _ = laRxOfSymbols (Concat []) kRest syms
                        laRxs
                    )
                    |> Set.ofList

                laRxs +< (Union childSubRules)
            else
                laRxs


        if LinLlkData.detectLeftRecursions g |> List.isEmpty |> not then
            failwith "Can't process left recursive grammar!"

        laRxOfRule rule

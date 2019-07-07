namespace LelekParser

module LeftFactoring =
    open Utils
    open LinLlkGrammar

    let safeTake n l =
        if List.length l < n
        then l
        else List.take n l


    let findPrefix (candidates: 'a list list) : 'a list =
        let findPrefix (len : int) (candidates: 'a list list)  : 'a list =
            let l' =
                candidates
                |> List.map (safeTake len)
                |> List.filter (fun e -> List.length e = len)

            match l' with
            | [] -> []
            | [_] -> [] // We need at least two items that share the same prefix!
            | _ ->
                let l'' =
                    l'
                    |> List.groupBy id
                    |> List.filter (fun (g, l) -> (l |> List.length) > 1)
                match l'' with
                | [] -> []
                | _ ->
                    l''
                    |> List.maxBy (fun (l, _) -> List.length l)
                    |> fst

        let rec findLongestPrefix (len : int) (candidates: 'a list list)  : 'a list =
            let p1 = findPrefix len candidates
            let p2 = findPrefix (len + 1) candidates

            match p1, p2 with
            | [], [] -> []
            | _, [] -> p1
            | _, _ ->
                let p3 = findLongestPrefix (len + 2) candidates
                match p3 with
                | [] -> p2
                | _ -> p3

        findLongestPrefix 1 candidates


    /// -------------------------------------------------------------------------
    /// Substitutes a list of rules with given name in the ebnf data with the result of
    /// the transformation
    let applyGTrans (g: LinLlkData) (vName: string) (trans: (LinLlkRule list -> LinLlkRule list)) =
        let (LinLlkData(cm, rl, cr)) = g
        let idx = rl |> List.findIndex (fun (LinLlkRule(Var = v)) -> v = vName)
        let affectedRules = LinLlkData.matchingRules g vName
        let filteredRules = rl |> List.filter (fun (LinLlkRule(Var = v)) -> v <> vName)
        let newRules = trans affectedRules
        let lower, upper = filteredRules |> List.splitAt idx
        LinLlkData(cm, lower @ newRules @ upper, cr)


    /// -------------------------------------------------------------------------
    /// Finds the longest left prefixes in rules of a given EBNF grammar.
    /// Can be use the factor out these left prefixes later.
    let findLongestPrefixes (g: LinLlkData) : (string * LinLlkSymbol list) list =
        let ruleName (LinLlkRule(Var = v)) = v
        let ruleProd (LinLlkRule(Prod = p)) = p
        
        let (LinLlkData(_, rl, _)) = g

        rl
        |> List.groupBy ruleName
        |> List.fold (fun acc elem ->
            let rn, rules = elem
            let prefix =
                rules
                |> List.map ruleProd
                |> findPrefix
            match prefix with
            | [] -> acc
            | _  -> (rn, prefix) :: acc
        ) List.empty


    /// -------------------------------------------------------------------------
    /// Facor out all left prefixes in rules of a given EBNF grammar.
    let leftFactor (g: LinLlkData) : LinLlkData =

        let modFactor (exclusions: string list) (prefix: LinLlkSymbol list) (rules: LinLlkRule list) : LinLlkRule list =
            let factorOutRule (rN: string) (rule: LinLlkRule): LinLlkRule =
                let (LinLlkRule(RuleComments = rc; Prod = pr; PTOp = ptOpt; Action = act)) = rule
                let pref = pr |> safeTake (prefix.Length)
                if pref <> prefix then
                    // This rule of a rule-set dont share the prefix - leave it unchanged.
                    rule
                else
                    let newProd = pr |> List.skip (prefix.Length)
                    LinLlkRule(RuleComments = rc, Var = rN, Prod = newProd, PTOp = ptOpt, Action = act)

            // We change the productions A -> pref suff to one new production A -> pref A'
            // where A' is a new production of the form:
            // A' -> suff1|suff2|... i.e. A' -> suff1;  A' -> suff2;  ...
            match rules with
            | h::_ ->
                let (LinLlkRule(Var = ruleName; PTOp = ptOpt; Action = act)) = h

                let prefixRuleName = generateName exclusions (ruleName + "_rest")
                let prefixRule =
                    LinLlkRule(RuleComments = [ sprintf "(* %s: Extracted prefix *)" ruleName ],
                        Var = ruleName,
                        Prod = prefix @ [ LinLlkVariable prefixRuleName ],
                        PTOp = ptOpt,
                        Action = act)
                let factoredRules =
                    rules
                    |> List.map (factorOutRule prefixRuleName)
                prefixRule :: factoredRules
            | _ ->
                internalError __SOURCE_DIRECTORY__ __SOURCE_FILE__ __LINE__ "Expected a non-empty rule list!"

        let factorOutPrefix g (factor: string * LinLlkSymbol list) =
            let (ruleName, prefix) = factor
            let names = LinLlkData.varNames g
            applyGTrans g ruleName (modFactor names prefix)

        let factorOut g =
            let prefixes = findLongestPrefixes g
            prefixes
            |> List.fold factorOutPrefix g

        doWhileChanging factorOut g

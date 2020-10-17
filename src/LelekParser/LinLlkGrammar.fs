namespace LelekParser

/// The 'Linearized LLK' Grammer which is a spimplified form of LLK grammar.
module LinLlkGrammar =
    open LlkGrammar

    type LinLlkSymbol =
        /// Represents an empty string (an empty alternative) in Llk
        | LinLlkEpsilon
        /// The string represents a regular expression
        | LinLlkTerminal of string
        /// The string represents the name of the nonterminal
        | LinLlkVariable of string
        /// Represents the end of the grammar
        | LinLlkEnd

    module LinLlkSymbol =
        let ofFactor (factor: Factor) : LinLlkSymbol =
            match factor with
            | N(LlkVariable s) -> LinLlkVariable s
            | T(LlkTerminal s) -> LinLlkTerminal s
            | T(LlkEpsilon)    -> LinLlkEpsilon
            | _ -> failwith "Unsupported factor kind for a LinLlkSymbol!"

        let toString (symbol: LinLlkSymbol) : string =
            /// Escape double quote characters iff they are not escaped yet!
            match symbol with
            | LinLlkEpsilon    -> "(* eps *)"
            | LinLlkTerminal s -> sprintf "\"%s\"" (Utils.escape s)
            | LinLlkVariable s -> s
            | LinLlkEnd        -> "(* end *)"

        /// -------------------------------------------------------------------------
        /// Extracts the name of a variable.
        let vName = function
            | LinLlkVariable s -> s
            | _ -> failwith "Expecting a variable here!"

        let isEpsilon = function
            | LinLlkEpsilon -> true
            | _ -> false

        let isVariable = function
            | LinLlkVariable _ -> true
            | _ -> false

    type LinLlkRule =
        | LinLlkRule of
            RuleComments: (string list) *
            Var: string *
            Prod: LinLlkSymbol list *
            PTOp: PTOperation *
            Action: string option

    module LinLlkRule =
        let ruleName (LinLlkRule(Var=ruleName)) =
            ruleName

        let toString (rule: LinLlkRule) : string =
            let (LinLlkRule(RuleComments = cmnts; Var = vName; Prod = symbols; PTOp = ptOp; Action = act)) = rule
            (cmnts
            |> String.concat "").Trim()
            +
            "\n" + vName + " = "
            +
            (symbols
            |> List.map LinLlkSymbol.toString
            |> String.concat " ")
            + " " + (ptOp |> (sprintf "(* %O *)"))
            + ((act |> Option.map (sprintf " @%s")) |> Option.defaultValue "")
            + "\n;\n"

        let toShortString (rule: LinLlkRule) : string =
            let (LinLlkRule(Var = vName; Prod = symbols)) = rule
            vName + " = "
            +
            (symbols
            |> List.map LinLlkSymbol.toString
            |> String.concat " ")
            + "\n;\n"

    type LinLlkData =
        | LinLlkData of CommentDcl: CommentDecl * LinLlkRules: LinLlkRule list * FileEndComments: string list


    module LinLlkData =
        open Utils

        let toString (g: LinLlkData) : string =
            let (LinLlkData(cm, rl, cr)) = g
            let res =
                (cm
                |> CommentDecl.toString)
                +
                (rl
                |> List.map LinLlkRule.toString
                |> String.concat "")
                +
                (cr
                |> String.concat "")
            res.Trim()


        /// -------------------------------------------------------------------------
        /// Collects all terminals of a given Llk grammar.
        let terminals (g: LinLlkData) : string list =
            let (LinLlkData(_, rl, _)) = g
            rl
            |> List.fold (fun acc (LinLlkRule(Prod = rhs)) ->
                rhs
                |> List.fold (fun acc sym ->
                    match sym with
                    | LinLlkTerminal s -> s :: acc
                    | _ -> acc
                ) acc
            ) List.empty
            |> List.rev
            |> List.distinct

        /// -------------------------------------------------------------------------
        /// Collects all variable names of a given EBNF grammar.
        let varNames (g: LinLlkData) : string list =
            let (LinLlkData(_, rl, _)) = g
            rl
            |> List.fold (fun acc (LinLlkRule(Var = name; Prod = prod)) ->
                prod
                |> List.fold (fun acc sym ->
                    let acc' = name :: acc
                    match sym with
                    | LinLlkVariable s -> s :: acc'
                    | _ -> acc'
                ) acc
            ) List.empty
            |> List.rev
            |> List.distinct


        /// -------------------------------------------------------------------------
        /// Collects all rules of the given variable name.
        let matchingRules (g: LinLlkData) (vn: string) =
            let (LinLlkData(_, rl, _)) = g
            rl
            |> List.filter (fun (LinLlkRule(Var = v)) -> v = vn)

        /// -------------------------------------------------------------------------
        /// Returns the start rule which is the first rule of the grammar.
        let startRule (g: LinLlkData) =
            let (LinLlkData(_, rl, _)) = g
            rl.Head

        /// -------------------------------------------------------------------------
        /// Collects all nullable variables of a given Llk grammar.
        let calcNullables (g: LinLlkData) : string list =
            let vars = varNames g
        
            let initialNullables =
                let isEpsilon = function
                    | LinLlkEpsilon -> true
                    | _ -> false

                vars
                |> List.filter (fun vn ->
                    vn
                    |> (matchingRules g)
                    |> List.exists (fun rule ->
                        let (LinLlkRule(Prod = symbols)) = rule
                        symbols |> List.forall isEpsilon
                    )
                )
        
            let collectNullables (current: string list) : string list =
                let isNullable current sym =
                    match sym with
                    | LinLlkEpsilon ->
                        true
                    | LinLlkVariable s ->
                        current |> List.contains s
                    | _ ->
                        false

                let hasNullableAlt (current: string list) (rules: LinLlkRule list) : bool =
                    rules
                    |> List.exists (fun rule ->
                        let (LinLlkRule(Var = v; Prod = symbols)) = rule
                        symbols
                        |> List.forall (isNullable current)
                    )

                vars
                |> List.fold (fun acc vn ->
                    let rulesWithName = vn |> (matchingRules g)
                    if hasNullableAlt current rulesWithName then
                        if acc |> List.contains vn |> not then
                            vn :: acc
                        else
                            acc
                    else
                        acc
                ) current
        
            doWhileChanging collectNullables initialNullables


        /// -------------------------------------------------------------------------
        /// Detects left recursions.
        /// Per symbol only the first recursive path will be found.
        /// The result is a list of recursive paths over grammar variables.
        let detectLeftRecursions (g: LinLlkData) : string list list =
            let vars = varNames g
            let nlls = calcNullables g

            let containsRecursion (visited: string list): bool =
                let recursionFound =
                    visited
                    |> List.groupBy id |> List.exists (fun (_, v) -> v.Length > 1)
                recursionFound

            let visitSymbol (symbol: LinLlkSymbol) (visited: string list): string list =
                match symbol with
                | LinLlkEnd
                | LinLlkEpsilon
                | LinLlkTerminal _ ->
                    visited
                | LinLlkVariable s ->
                    visited @ [s]

            let rec findRecursionAtRule (rule: LinLlkRule) (visited: string list) =
                let (LinLlkRule(Prod = symbols)) = rule
                findRecursionAtSymbols symbols visited

            and findRecursionAtSymbols (symbols: LinLlkSymbol list) (visited: string list): string list =
                match symbols with
                | []    ->
                    visited
                | h::t  ->
                    let visited1 = visitSymbol h visited
                    let recursionFound = visited1 |> containsRecursion
                    if recursionFound then
                        visited1
                    else
                        match h with
                        | LinLlkEnd
                        | LinLlkEpsilon
                        | LinLlkTerminal _ ->
                            visited1
                        | LinLlkVariable s ->
                            let visited' = findRecursionAtVariable s visited1
                            if nlls |> List.contains s then
                                findRecursionAtSymbols t visited'
                            else
                                visited'
                                

            and findRecursionAtVariable (var: string) (visited: string list): string list =
                var
                |> matchingRules g
                |> List.fold (fun acc rule ->
                    let (LinLlkRule(Var = v)) = rule
                    if acc.Length > 0 && acc |> containsRecursion then
                        acc // Only collect one recursion path!
                    else
                        findRecursionAtRule rule visited
                ) List.empty

            vars
            |> List.map (fun var -> findRecursionAtVariable var List.empty)
            |> List.filter containsRecursion
            |> List.distinct


        let findUnreachables (g: LinLlkData) : string list =
            let allVars = varNames g

            let (LinLlkData(_, rl, _)) = g
            let definedRules =
                rl
                |> List.map (fun (LinLlkRule(Var = name)) -> name)
                |> List.distinct

            allVars
            |> List.filter (fun vN ->
                definedRules |> List.contains vN |> not
            )


        let rulesInParseOrder (g: LinLlkData): string list =
            let (LinLlkData(_, rl, _)) = g
            let startRule = rl.Head |> LinLlkRule.ruleName
            let vars = varNames g
            assert (vars.Head = startRule)
            vars

        /// Append Ends ($'s) to the grammar to get always k LA tokens!
        let augmentWithEnds(g: LinLlkData) (k: int): LinLlkData =
            let exclusions = varNames g
            let startRuleName = generateName exclusions "start"
            let (LinLlkData(cm, rl, cr)) = g
            let (LinLlkRule(Var=oldStartRuleName; PTOp = ptOp; Action = act)) = rl.Head
            let dollars = List.init k (fun _ -> LinLlkEnd)
            let newStartRule = LinLlkRule(RuleComments=[], Var=startRuleName, Prod= LinLlkVariable(oldStartRuleName) :: dollars, PTOp = ptOp, Action = act)
            LinLlkData(CommentDcl = cm, LinLlkRules = newStartRule :: rl, FileEndComments = cr)


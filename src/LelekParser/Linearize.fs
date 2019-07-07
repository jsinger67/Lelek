namespace LelekParser

/// This module provides functions that support the conversion of Llk constructs
/// to their elementary representation.

// The steps of conversion should be as follows:

// 1. Substitute rules with multiple alternatives by multiple rules with one alternative.
// 2. Substitute repetitions
// 3. Substitute options
// 4. Substitute groups by new rules

module Linearize =
    open Utils
    open LlkGrammar
    open LinLlkGrammar


    /// Try to find the index of the first rule that satisfies the predicate
    let tryFindRule (rl: Rule list) (pred: (Rule -> bool)) : int option =
        rl
        |> List.tryFindIndex pred

    /// Try to find the index of the first rule that satisfies the predicate
    let tryFindRuleWithFactor (rl: Rule list) (pred: (Factor -> bool)) : int option =
        rl
        |> List.tryFindIndex (fun (Rule(RHS = rhs)) ->
            let (E alts) = rhs
            alts
            |> List.exists (fun alt ->
                let (A(factors, _)) = alt
                factors
                |> List.exists pred
            )
        )

    /// Substitutes a rule on index idx in the ebnf data with the result of
    /// the transformation
    let applyGTrans (g: LlkData) (idx: int) (trans: (Rule -> Rule list)) =
        let (LlkData(cm, rl, cr)) = g
        let lower, upper = rl |> List.splitAt idx
        match upper with
        | []    -> failwith "Unexpected split result"
        | h::t  ->
            let newRules = trans h
            LlkData(cm, lower @ newRules @ t, cr)

    
    /// Substitutes a factor on index idx in the rule with the result of
    /// the transformation
    let applyRTrans (rule: Rule) (idx: int) (trans: (Factor -> Factor)) =
        let (Rule(Comments = cmnts; LHS = lhs; RHS = rhs; PTOp = ptOp)) = rule
        let (E alts) = rhs
        match alts with
        | [a]   ->
            let (A(factors, action)) = a
            let lower, upper = factors |> List.splitAt idx
            match upper with
            | []    -> failwith "Unexpected split result"
            | h::t  ->
                let newRule = trans h
                let newFactors = lower @ (newRule :: t)
                let alt = A(newFactors, action)
                Rule(Comments = cmnts, LHS = lhs, RHS = E([alt]), PTOp = ptOp)

        | _     -> failwith "Expected only one alternative per rule!"


    let setPTOperation ptOp rule =
        let (Rule(Comments = cmnts; LHS = lhs; RHS = rhs)) = rule
        Rule(Comments = cmnts, LHS = lhs, RHS = rhs, PTOp = ptOp)


    // -------------------------------------------------------------------------
    // Replace the given rule with multiple alternatives by a list of new rules.
    // Keep the rule comment only at the first rule.
    // -------------------------------------------------------------------------
    let separateAlts (rule: Rule) : Rule list =
        let (Rule(Comments = cmnts; LHS = lhs; RHS = rhs; PTOp = ptOp)) = rule
        let (E alts) = rhs
        let newRules =
            alts
            |> List.mapi (fun i a ->
                let cmts = if i = 0 then cmnts else List.empty
                Rule(Comments = cmts, LHS = lhs, RHS = E([a]), PTOp = ptOp)
            )
        newRules

    // -------------------------------------------------------------------------
    // Replace the first Factor that is a G with a new rule.
    // -------------------------------------------------------------------------
    // R  -> x ( g ) y.
    // =>
    // R  -> x G y.     (1)
    // G  -> g.         (2)
    let elimGrp (exclusions: string list) (rule: Rule) : Rule list =
        let substGWithV vName _ =
            Factor.mkVar(vName)

        let (Rule(LHS = lhs; RHS = rhs; PTOp = ptOp)) = rule

        // G should have the form <ruleName>_rpt[<num>] => grpRuleName
        let (LlkVariable ruleName) = lhs
        let grpRuleName = generateName exclusions (ruleName + "_itm1")

        // Find the index of the R factor in the alternative
        let (E alts) = rhs
        match alts with
        | [a]   ->
            let (A(factors, _)) = a
            let idxOpt =
                factors
                |> List.tryFindIndex Factor.isGrp
            
            if idxOpt.IsNone then
                failwith "Expected a G factor!"

            let rule1 =     // Rule (1)
                idxOpt.Value
                |> (fun idx -> applyRTrans rule idx (substGWithV grpRuleName))

            let expr =
                match factors.[idxOpt.Value] with
                | G(Group(expr))   -> expr
                | _                 -> failwith "Internal error!"

            let rule2 =     // Rule (2)
                Rule(Comments = [sprintf "(* %s: Extracted group *)" ruleName],
                    LHS = LlkVariable.mkVar(grpRuleName), 
                    RHS = expr,
                    PTOp = Nop)
            
            [ rule1; rule2 ]

        | _     ->  // Temporarily possible! Checked in 'transform2BNF'
            [ rule ]

    // -------------------------------------------------------------------------
    // Replace the first Factor that is a O with a new rule.
    // -------------------------------------------------------------------------
    // R  -> x [ a ] y.
    // =>
    // R  -> x R' y.    (1)
    // R' -> a.         (2a)
    // R' -> eps.       (2b)
    let elimOpt (exclusions: string list) (rule: Rule) : Rule list =
        let substGWithV vName _ =
            Factor.mkVar(vName)

        let (Rule(LHS = lhs; RHS = rhs; PTOp = ptOp)) = rule

        // R' should have the form <ruleName>_rpt[<num>] => grpRuleName
        let (LlkVariable ruleName) = lhs
        let optRuleName = generateName exclusions (ruleName + "_opt1")

        // Find the index of the R factor in the alternative
        let (E alts) = rhs
        match alts with
        | [a]   ->
            let (A(factors, _)) = a
            let idxOpt =
                factors
                |> List.tryFindIndex Factor.isOpt
            
            if idxOpt.IsNone then
                failwith "Expected a O factor!"

            let rule1 =     // Rule (1)
                idxOpt.Value
                |> (fun idx -> applyRTrans rule idx (substGWithV optRuleName))
                |> setPTOperation Clip

            let expr =
                match factors.[idxOpt.Value] with
                | O(Optional(expr)) -> expr
                | _                 -> failwith "Internal error!"

            let rule2a =     // Rule (2a)
                Rule(Comments = [sprintf "(* %s: Optional: Actual alternative *)" ruleName],
                    LHS = LlkVariable.mkVar(optRuleName), 
                    RHS = expr,
                    PTOp = ptOp)
            
            let rule2b =    // Rule (2b)
                Rule(Comments = [sprintf "(* %s: Optional: Empty alternative *)" ruleName],
                    LHS = LlkVariable.mkVar(optRuleName), 
                    RHS = E([A([T(LlkEpsilon)], None)]),
                    PTOp = ptOp)

            [ rule1; rule2a; rule2b ]

        | _     ->  // Temporarily possible! Checked in 'transform2BNF'
            [ rule ]

    // -------------------------------------------------------------------------
    // Replace the first Factor that is a R with a non-left-recursive substitution.
    // -------------------------------------------------------------------------
    // R  -> x { a } y
    // =>
    // R  -> x R' y     (1)
    // R' -> a R'       (2)
    // R' -> eps.       (3)
    let elimRep (exclusions: string list) (rule: Rule) : Rule list =
        let substRWithV vName _ =
            Factor.mkVar(vName)

        let (Rule(LHS = lhs; RHS = rhs; PTOp = ptOp)) = rule

        // R' should have the form <ruleName>_rpt[<num>] =>  a repRuleName2
        //                         <ruleName>_rpt[<num>] =>  (* eps *)
        let (LlkVariable ruleName) = lhs
        let repRuleName2 = generateName exclusions (ruleName + "_lst1")

        // Find the index of the R factor in the alternative
        let (E alts) = rhs
        match alts with
        | [a]   ->
            let (A(factors, _)) = a
            let idxOpt =
                factors
                |> List.tryFindIndex Factor.isRep
            
            if idxOpt.IsNone then
                failwith "Expected a R factor!"

            let rule1 =     // Rule (1)
                idxOpt.Value
                |> (fun idx -> applyRTrans rule idx (substRWithV repRuleName2))
                |> setPTOperation Clip

            let expr =
                match factors.[idxOpt.Value] with
                | R(Repeat(exp))    -> G(Group(exp))
                | _                 -> failwith ""

            let rule2 =     // Rule (2)
                Rule(Comments = [sprintf "(* %s: Repetition list *)" ruleName],
                    LHS = LlkVariable.mkVar(repRuleName2), 
                    RHS = E([A([ expr ; N(LlkVariable(repRuleName2))], None)]),
                    PTOp = Collect)
            
            let rule3 =     // Rule (3)
                Rule(Comments = [sprintf "(* %s: Repetition: Empty alternative *)" ruleName],
                    LHS = LlkVariable.mkVar(repRuleName2), 
                    RHS = E([A([T(LlkEpsilon)], None)]),
                    PTOp = ptOp)


            [ rule1; rule2; rule3 ]

        | _     ->  // Temporarily possible! Checked in 'transform2BNF'
            [ rule ]


    // -------------------------------------------------------------------------
    // Guidelines:
    // After applying all tranformations inner (sub-) expressions should be factored out.
    // The grammar's structure should be linear then (i.e no loops like in {}).
    // Epsilon elements are allowed at this stage.
    // The input order should be preserved as much as possible.
    // The comments should be preserved too.
    // -------------------------------------------------------------------------

    // Transform rules with multiple alternatives
    let separateMultipleAlternatives (g: LlkData) : LlkData =
        let predHasMultipleAlts (rule: Rule) : bool =
            let (Rule(RHS = rhs)) = rule
            let (E alts) = rhs
            alts.Length > 1

        let tryFindRuleWithMultipleAlts (LlkData(_, rl, _)) : int option =
            tryFindRule rl predHasMultipleAlts

        let separateAlternatives g =
            tryFindRuleWithMultipleAlts g
            |> Option.map (fun idx -> applyGTrans g idx separateAlts)
            |> Option.defaultValue g

        doWhileChanging separateAlternatives g


    // Eliminate repetitions
    let eliminateRepetitions (g: LlkData) : LlkData =
        let tryFindRuleWithRep (LlkData(_, rl, _)) : int option =
            tryFindRuleWithFactor rl Factor.isRep

        let eliminateRepetition g =
            let names = LlkData.variableNames g
            tryFindRuleWithRep g
            |> Option.map (fun idx -> applyGTrans g idx (elimRep names))
            |> Option.defaultValue g

        doWhileChanging eliminateRepetition g

    // Eliminate optionals
    let eliminateOptionals (g: LlkData) : LlkData =
        let tryFindRuleWithOpt (LlkData(_, rl, _)) : int option =
            tryFindRuleWithFactor rl Factor.isOpt

        let eliminateOptionals g =
            let names = LlkData.variableNames g
            tryFindRuleWithOpt g
            |> Option.map (fun idx -> applyGTrans g idx (elimOpt names))
            |> Option.defaultValue g

        doWhileChanging eliminateOptionals g

    // Eliminate groups
    let eliminateGroups (g: LlkData) : LlkData =
        let tryFindRuleWithGrp (LlkData(_, rl, _)) : int option =
            tryFindRuleWithFactor rl Factor.isGrp

        let eliminateGroup g =
            let names = LlkData.variableNames g
            tryFindRuleWithGrp g
            |> Option.map (fun idx -> applyGTrans g idx (elimGrp names))
            |> Option.defaultValue g

        doWhileChanging eliminateGroup g


    let transform2BNF (g: LlkData) : LinLlkData =
        let (LlkData(cm, rl, cr)) = g
        let rl2 =
            rl
            |> List.map (fun rule ->
                let (Rule(Comments = cmnts; LHS = lhs; RHS = rhs; PTOp = ptOp)) = rule
                let (E alts) = rhs
                let symbols, action =
                    match alts with
                    | [a]   ->
                        let (A(factors, action)) = a
                        factors |> List.map LinLlkSymbol.ofFactor, action
                    | _     ->
                        (sprintf "%s: Expected exactly one alternative per rule but found %d!" (LlkVariable.varName lhs) (alts.Length))
                        |> Utils.internalError __SOURCE_DIRECTORY__ __SOURCE_FILE__ __LINE__

                LinLlkRule(RuleComments = cmnts, Var = LlkVariable.varName lhs, Prod = symbols, PTOp = ptOp, Action = action)
            )
        LinLlkData(CommentDcl = cm, LinLlkRules = rl2, FileEndComments = cr)


    let linearizeGrammar g =
        let trans =
            separateMultipleAlternatives >>
            eliminateRepetitions >>
            eliminateOptionals >>
            eliminateGroups

        doWhileChanging trans g
        |> transform2BNF

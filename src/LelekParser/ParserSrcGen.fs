namespace LelekParser

module ParserSrcGen =
    open System.IO
    open DotLiquid
    open LinLlkGrammar
    open Token
    open ParserFeedback
    open LANfa
    open LADfa
    open ParserData
    open System.Reflection
    open ParserGenLogger
    open LexerTerminals
    open CompiledLADfa
    open FSharpSrcGen
    open ParsingPrediction

    type ParserLiquidData = {
        name_space_name: string
        module_name: string
        lexer_module_name: string
        user_ast_type: string
        parser_data: string
        k: int
    }

    let private buildParserData (logger: Logger) (kMax: int) (g: LinLlkData) : Result<string * int, string> =
        let recursions = g |> LinLlkData.detectLeftRecursions
        if recursions.IsEmpty |> not then
            recursions |> ParserSrcGenDiagnosis.logLeftRecursion logger
            "ERROR: Can't process left recursive grammars!" |> Error 
        else
            let gReduced =
                let unreachables = g |> LinLlkData.findUnreachables
                let (LinLlkData(cm, rl, cr)) = g
                LinLlkData(cm, (rl |> List.filter (fun (LinLlkRule(Var = v)) -> unreachables |> List.contains v |> not)), cr)

            let gLeftFactored =
                gReduced |> LeftFactoring.leftFactor

            let printToken = (indexToTokenTypeName gLeftFactored)

            let printTokenError fileName (state: State) token: string =
                (sprintf "Error %s%s\nExpected one of" fileName (Token.toString token)) +
                (state.Transitions
                |> Array.map (fun (Tr(Tok = tok)) ->
                    tok |> int |> printToken)
                |> String.concat " ")

            let orderedRules =
                gLeftFactored |> LinLlkData.rulesInParseOrder

            let feedback = provideParserFeedback (printTokenError logger.inputFile)

            if orderedRules.Length <> (gLeftFactored |> LinLlkData.varNames).Length then
                let _lfv = gLeftFactored |> LinLlkData.varNames
                failwith "Failure sorting rules in dependency order! Check grammar for unreachable variables"

            let conflicts =
                gLeftFactored |> ParsingPrediction.findLL1Conflicts logger

            ParserSrcGenDiagnosis.logConflicts logger conflicts

            let assessment =
                conflicts
                |> List.map (ParsingPrediction.lookaheadToPredict kMax gLeftFactored)

            let predictable =
                assessment
                |> List.map fst
                |> List.forall id

            if not predictable then
                let ruleName =                    
                    conflicts
                    |> List.item (assessment |> List.findIndex (fun (b, _) -> not b))
                    |> List.head
                    |> (fun (ParsingPrediction.First1SetOfRule(Rule = r)) -> r)
                    |> LinLlkRule.ruleName

                ParserSrcGenDiagnosis.diagnoseOutputNotPredictable logger feedback kMax ruleName conflicts assessment gLeftFactored
                Error (sprintf "Can't predict rule case with lookahead of %d in rule %s" kMax ruleName)
            else
                let maxMinK =
                    if assessment.IsEmpty then
                        1   // No conflicts => LL(1)
                    else
                        assessment
                        |> List.map (snd >> fst)
                        |> List.max

                let maxMaxK =
                    if assessment.IsEmpty then
                        1   // No conflicts => LL(1)
                    else
                        assessment
                        |> List.map (snd >> snd)
                        |> List.max

                let calcLookaheadAutomaton (ruleSet: LinLlkRule list): Result<LADfa.Dfa, string> =
                    // All rules in the rule set have the same names!
                    let ruleName = ruleSet.Head |> LinLlkRule.ruleName
                    let conflictIndex =
                        conflicts
                        |> List.tryFindIndex (fun conflict -> conflict.Head |> First1SetOfRule.ruleOf |> LinLlkRule.ruleName = ruleName)
                    let k =
                        match conflictIndex with
                        | Some index    -> assessment.[index] |> snd |> fst
                        | None          -> 1
                    let composedDfa =
                        if k <> 1 then
                            let conflictCase = conflicts.[conflictIndex.Value] |> List.map First1SetOfRule.ruleOf
                            List.zip
                                ruleSet
                                (ruleSet
                                |> List.map (fun rule ->
                                    // Not all rules in the rule set must be predicted with k > 1!
                                    if conflictCase |> List.contains rule then k else 1
                                ))
                            |> List.map (fun (rule, kRule) -> rule |> LAAutomaton.calcLAData gLeftFactored kRule |> Nfa.ofLAData kRule)
                            |> List.mapi (fun i nfa -> logger.logSubNfa ruleName i nfa; nfa)
                            |> Nfa.makeUnion
                            |> (fun nfa -> logger.logMainNfa ruleName nfa; nfa)
                            |> Dfa.ofNfa
                            |> (fun dfa -> logger.logDfa ruleName dfa; dfa)
                        else
                            ruleSet
                            |> List.map (fun (rule) -> rule |> LAAutomaton.calcLAData gLeftFactored k |> Nfa.ofLAData k)
                            |> List.mapi (fun i nfa -> logger.logSubNfa ruleName i nfa; nfa)
                            |> Nfa.makeUnion
                            |> (fun nfa -> logger.logMainNfa ruleName nfa; nfa)
                            |> Dfa.ofNfa
                            |> (fun dfa -> logger.logDfa ruleName dfa; dfa)

                    // Check if all accepting states can be asked for a single contributing union part (i.e. for a production to choose)
                    let ok =
                        composedDfa
                        // Note start state {0} is filtered out because it is no valid contributor
                        |> Map.filter (fun key v -> key <> (Set.singleton 0) && v.Accepted )
                        |> Map.forall (fun state v ->
                            match LADfa.DfaState.getAcceptingUnionPart v with
                            | Ok _ -> true
                            | Error msg ->
                                feedback.printError msg
                                ParserSrcGenDiagnosis.diagnoseOutputDFAAmbiguity feedback ruleName state v composedDfa
                                false)
                    if ok then
                        Ok composedDfa
                    else
                        Error (sprintf "Resulting DFA for rule %s yields ambiguity for k=%d" ruleName k)

                let preliminaryParserData: (LinLlkRule list * Result<LADfa.Dfa, string>) list =
                    orderedRules
                    |> List.map (LinLlkData.matchingRules gLeftFactored)
                    |> List.fold (fun acc elem ->
                        (elem, elem |> calcLookaheadAutomaton)
                        |> List.singleton
                        |> List.append acc
                    ) List.empty

                let toTokenType s =
                    let stoi = nameToIndex gLeftFactored
                    stoi s

                let toRuleNumber =
                    let ruleNames =
                        preliminaryParserData
                        |> List.collect fst
                        |> List.map LinLlkRule.ruleName
                    (fun n -> ruleNames |> List.findIndex ((=) n))

                let parserData: RuleSetDataSrc list =
                    let rd =
                        preliminaryParserData
                        |> List.map (fun (rl, res) ->
                            let dfa = match res with Ok d -> d | _ -> Dfa.empty
                            let rule = (rl, dfa) |> (RuleSetDataSrc.ofBNFData toRuleNumber toTokenType)
                            let (RuleSetDataSrc(LookaheadDFA = cdfa)) = rule
                            cdfa |> logger.logCompiledDfa printToken (rl.Head |> LinLlkRule.ruleName)
                            rule
                        )
                    rd
                    |> List.fold (fun (rdIdx, idx) elem ->
                        let (RuleSetDataSrc(Rules = rl; LookaheadDFA = cdfa)) = elem
                        rdIdx @ [ RuleSetDataSrc(Rules = rl, LookaheadDFA = cdfa, BaseIndex = idx) ], idx + rl.Length
                    ) ([], 0)
                    |> fst

                if preliminaryParserData |> ParserSrcGenDiagnosis.hasDfaGenErrors then
                    ParserSrcGenDiagnosis.logDfaGenErrors logger preliminaryParserData
                    preliminaryParserData |> ParserSrcGenDiagnosis.extractDfaGenErrors |> Error
                else
                    ((parserData |> ParserData.toFs), maxMaxK) |> Ok

    type ParserSrcGenParams = {
        Logger: Logger
        MaxLookahead: int
        Grammar: LinLlkData
        OutFileName: string
        ParserNamespace: string
        ParserModule: string
        LexerModule: string
        ASTTypeName: string
    }

    let generateParserSrc (par: ParserSrcGenParams): Result<unit, string> =

        let buildParserSrc (pd, k) =
            let data = {
                name_space_name = par.ParserNamespace
                module_name = par.ParserModule
                lexer_module_name = par.LexerModule
                user_ast_type = par.ASTTypeName
                parser_data = pd
                k = k
            }
            let folder = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
            use sr = new StreamReader(File.OpenRead(folder + @"./templates/ParserSrc.liquid"))            
            let template = Template.Parse(sr.ReadToEnd())
            let compiledSrc = template.Render(Hash.FromAnonymousObject(data))
            use swParserSrc = new StreamWriter(File.Open(par.OutFileName, FileMode.Create))
            swParserSrc.Write compiledSrc

        par.Grammar
        |> buildParserData par.Logger par.MaxLookahead
        |> Result.map buildParserSrc

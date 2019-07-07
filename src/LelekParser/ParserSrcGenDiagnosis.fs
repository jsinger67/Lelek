namespace LelekParser

open ParserTypes

module ParserSrcGenDiagnosis =
        open LinLlkGrammar
        open ParserGenLogger
        open LANfa
        open LADfa
        open ParserFeedback
        open LAAutomaton
        open GraphvisConversions
        open ParsingPrediction
        open TokenStream
        open Token

        let logLeftRecursion (logger: Logger) (recursions: string list list) =
            recursions
            |> List.fold (fun acc elem ->
                [acc; (elem |> String.concat "->" |> sprintf "  %s")] |> String.concat "\n"
            ) (sprintf "Grammar contains at least %d left recursions:" (recursions.Length))
            |> logger.log

        let logConflicts (logger: Logger) (conflicts: LL1Conflicts) =
            logger.log "Confilcts:"
            conflicts
            |> List.iter (fun c ->
                c |> List.iter (fun (First1SetOfRule(r, c)) ->
                    r |> LinLlkRule.toString |> logger.log
                    c |> Set.map (fun sym -> sym |> LinLlkSymbol.toString) |> String.concat "-" |> logger.log))

        let diagnoseOutputPrediction
            (feedback: ParserFeedback)
            (ruleName: string)
            (stream: TokenStream)
            (printToken: int -> string) =
            let tokens =
                [0 .. stream.k]
                |> List.map stream.lookahead
                |> List.map (fun tok ->
                    sprintf "%s%s (%s)" stream.fileName (tok |> Token.toString) (tok |> Token.typeOf |> printToken))
                |> String.concat "\n"
            feedback.printError (sprintf "Can't predict rule of type %s\nLookahead tokens:\n%s" ruleName tokens)

        let diagnoseOutputDFAAmbiguity (feedback: ParserFeedback) (ruleName: string) (k: Set<int>) (v: DfaState) (composedDfa: Dfa) =
            ruleName
            |> sprintf "Ambiguity in rule %s"
            |> feedback.printError
            
            k
            |> Seq.map (sprintf "%x")
            |> String.concat "-"
            |> sprintf "  in state %s"
            |> feedback.printError

            v.NfAStates
            |> List.map (fun (i,s) ->
                sprintf "    %x: %b" i s.Accepted)
            |> String.concat "\n"
            |> feedback.printError
            
            composedDfa
            |> Dfa.toDot ruleName (LAItem.toString (sprintf " %d "))
            |> feedback.printMessage

        let diagnoseOutputNotPredictable
            (logger: Logger)
            (feedback: ParserFeedback)
            (kMax: int)
            ruleName
            (conflicts: LL1Conflicts)
            (assessment: (bool * (int * int)) list)
            (g: LinLlkData) =
            
            let b, k =
                (List.zip conflicts assessment)
                |> List.find (fun (cl, _) -> cl.Head |> First1SetOfRule.ruleOf |> LinLlkRule.ruleName = ruleName)
                |> (fun (_, (b, k)) -> b, k)
            let msg =
                if b then 
                    sprintf "Rule %s was predicted to be resolvable with k=%d,%d" ruleName (fst k) (snd k)
                else
                    sprintf "Rule %s couldn't be resolved with k=%d,%d" ruleName (fst k) (snd k)
            feedback.printMessage msg
            logger.log msg
            conflicts
            |> List.item (assessment |> List.findIndex (fun (b, _) -> not b))
            |> List.mapi (fun i (First1SetOfRule(Rule = rl)) ->
                rl
                |> LAAutomaton.calcLAData g kMax
                |> Nfa.ofLAData kMax
                |> (fun nfA -> logger.logSubNfa ruleName i nfA ;nfA)
            )
            |> List.mapi (fun i nfa ->
                nfa |> Nfa.toDot (sprintf "Contibutor %d" i) (LAItem.toString (sprintf " %d ")) |> feedback.printMessage
                nfa
            )
            |> Nfa.makeUnion
            |> (fun nfa -> logger.logMainNfa ruleName nfa; nfa)
            |> Dfa.ofNfa
            |> (fun dfa -> logger.logDfa ruleName dfa; dfa)
            |> Dfa.toDot ruleName (LAItem.toString (sprintf " %d "))
            |> (fun msg -> feedback.printMessage msg; logger.log msg)

        let hasDfaGenErrors (preliminaryParserData: (LinLlkRule list * Result<LADfa.Dfa, string>) list) =
            preliminaryParserData
            |> List.map snd
            |> List.exists (fun r -> match r with | Error _ -> true | _-> false)

        let extractDfaGenErrors (preliminaryParserData: (LinLlkRule list * Result<LADfa.Dfa, string>) list) =
            preliminaryParserData
            |> List.map snd
            |> List.filter (fun r -> match r with | Error _ -> true | _-> false)
            |> List.map (fun r -> match r with | Error s -> s | _-> "")
            |> String.concat "\n"

        let logDfaGenErrors (logger: Logger) (preliminaryParserData: (LinLlkRule list * Result<LADfa.Dfa, string>) list) =
            preliminaryParserData
            |> extractDfaGenErrors
            |> logger.log


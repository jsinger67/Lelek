namespace LelekParser

module ParserData =
    open LinLlkGrammar
    open CompiledLADfa
    open ParserTypes

    module ParserSymbol =
        let ofBNFSymbol (ruleNumber: string -> int) (tokenType: string -> int) = function 
            | LinLlkEpsilon    -> failwith "Won't support epsilon as scanner output"
            | LinLlkTerminal s -> s |> tokenType
            | LinLlkVariable s -> -(ruleNumber s)
            | LinLlkEnd        -> Token.EndOfInputToken


        let toString (printToken: int -> string) (parserData: ParserRule<'a> array) s =
            if s < 1 then
                let (ParserRule(Name = name)) = parserData.[-s]
                name
            else
                printToken s

    type ParserRuleSrc =
        | ParserRuleSrc of
            Comments: (string list) *
            Name: string *
            Production: ParserSymbol list *
            PTOp: string *
            Action: string

    module ParserRuleSrc =
        open LlkGrammar

        let generatePTOperation (ruleName:string) (ptOp: PTOperation): string =
            let src =
                match ptOp with
                | Nop ->        "ParseTree.makePT"
                | Clip ->       "ParseTree.clipPT"
                | Collect ->    "ParseTree.collectPT"
                | Lift ->       "ParseTree.liftPT"

            (sprintf @"(fun args -> %s ""%s"" args)" src ruleName)

        let ofLinLlkRule (ruleNumber: string -> int) (tokenType: string -> int) (LinLlkRule(RuleComments = cmnts; Var = vName; Prod = symbols; PTOp = ptOpt; Action = act)) =
            let ptOp = ptOpt |> generatePTOperation vName
            let action = act |> Option.defaultValue "(fun _ state _ -> state)"
            let parserSymbols =
                symbols
                |> List.filter (LinLlkSymbol.isEpsilon >> not)
                |> List.map (ParserSymbol.ofBNFSymbol ruleNumber tokenType)
            ParserRuleSrc(Comments = cmnts, Name = vName, Production = parserSymbols, PTOp = ptOp, Action = action)

        let nameOf (ParserRuleSrc(Name = vName)) =
            vName

    type RuleSetDataSrc =
        | RuleSetDataSrc of Rules: ParserRuleSrc list * LookaheadDFA: CompiledDfa * BaseIndex: int

    module RuleSetDataSrc =
        let ofBNFData (ruleNumber: string -> int) (tokenType: string -> int) (bnfRules, dfa) =
            let parserRules =
                bnfRules
                |> List.map (ParserRuleSrc.ofLinLlkRule ruleNumber tokenType)
            RuleSetDataSrc(Rules = parserRules, LookaheadDFA = (dfa |> CompiledDfa.ofDfa), BaseIndex = 0)

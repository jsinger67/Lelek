namespace BP

module BPParser =
    open LelekParser
    open TokenStream
    open ParserFeedback
    open Token
    open ParseTree
    open CompiledLADfa
    open ParserTypes

    type UserParserRule = ParserRule<AST.AST>
    type UserParserData = UserParserRule array

    let diagnoseOutputWrongToken fileName (feedback: ParserFeedback) token toExpected =
        sprintf "Error Wrong token\nReceived \"%s%s(%A)\", Expected %s"
            fileName (Token.toString token) (Token.typeOf token |> enum<BPLexer.TokenType>) toExpected
        |> feedback.printError

    let printToken = BPLexer.tokenTypeToString

    let printTokenError fileName (state: State) token: string =
        (sprintf "Error %s%s <%A> \nExpected one of "
            fileName (Token.toString token) (Token.typeOf token |> enum<BPLexer.TokenType>)) +
        (state.Transitions
        |> Array.map (fun (Tr(Tok = tok)) ->
            tok |> int |> printToken
        )
        |> String.concat " ")

    let k = 1

    let doParse (stream: TokenStream) (feedback: ParserFeedback) traceMode =
        let mutable userStack : AST.AST list = []
        let mutable parserStack: ParseType<AST.AST> list = [Symbol Token.EndOfInputToken]
        let mutable ruleDepth = 0
        let mutable ptStack: ParseTree list = []

        let parserData: UserParserData =
            [|

                // -----------------------------------------------------------------------------
                // 0    Rules of "Expression"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Expression",
                    Production = [|-1; -2|],
                    PTOp = (fun args -> ParseTree.makePT "Expression" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 1)
                                    Tr(Tok = 5, Next = 1)
                                    Tr(Tok = 6, Next = 1)
                                    Tr(Tok = 13, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                        |]
                )

                // -----------------------------------------------------------------------------
                // 1    Rules of "Term"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Term",
                    Production = [|-12; -14|],
                    PTOp = (fun args -> ParseTree.clipPT "Term" args),
                    Action = AST.term,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 1)
                                    Tr(Tok = 5, Next = 1)
                                    Tr(Tok = 6, Next = 1)
                                    Tr(Tok = 13, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                        |]
                )

                // -----------------------------------------------------------------------------
                // 2    Rules of "BinOpsRepeat"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "BinOpsRepeat",
                    Production = [|-3|],
                    PTOp = (fun args -> ParseTree.clipPT "BinOpsRepeat" args),
                    Action = AST.binOpList,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 7, Next = 1)
                                    Tr(Tok = 8, Next = 1)
                                    Tr(Tok = 9, Next = 1)
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 11, Next = 1)
                                    Tr(Tok = 12, Next = 1)
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 2147483647, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                        |]
                )

                // -----------------------------------------------------------------------------
                // 3    Rules of "BinOpsRepeat_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "BinOpsRepeat_lst1",
                    Production = [|-5; -3|],
                    PTOp = (fun args -> ParseTree.collectPT "BinOpsRepeat_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 7, Next = 1)
                                    Tr(Tok = 8, Next = 1)
                                    Tr(Tok = 9, Next = 1)
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 11, Next = 1)
                                    Tr(Tok = 12, Next = 1)
                                    Tr(Tok = 14, Next = 7)
                                    Tr(Tok = 2147483647, Next = 7)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 7
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "BinOpsRepeat_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "BinOpsRepeat_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 5    Rules of "BinOpsRepeat_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "BinOpsRepeat_lst1_itm1",
                    Production = [|-6; -1|],
                    PTOp = (fun args -> ParseTree.makePT "BinOpsRepeat_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 7, Next = 1)
                                    Tr(Tok = 8, Next = 1)
                                    Tr(Tok = 9, Next = 1)
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 11, Next = 1)
                                    Tr(Tok = 12, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                        |]
                )

                // -----------------------------------------------------------------------------
                // 6    Rules of "BinaryOperator"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "BinaryOperator",
                    Production = [|-19|],
                    PTOp = (fun args -> ParseTree.makePT "BinaryOperator" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 7, Next = 1)
                                    Tr(Tok = 8, Next = 2)
                                    Tr(Tok = 9, Next = 3)
                                    Tr(Tok = 10, Next = 4)
                                    Tr(Tok = 11, Next = 5)
                                    Tr(Tok = 12, Next = 6)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 2
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 3
                                Transitions = [||]
                                Accepted = true
                                Prediction = 2
                            }
                            {
                                Id = 4
                                Transitions = [||]
                                Accepted = true
                                Prediction = 3
                            }
                            {
                                Id = 5
                                Transitions = [||]
                                Accepted = true
                                Prediction = 4
                            }
                            {
                                Id = 6
                                Transitions = [||]
                                Accepted = true
                                Prediction = 5
                            }
                        |]
                )
                ParserRule(
                    Name = "BinaryOperator",
                    Production = [|-20|],
                    PTOp = (fun args -> ParseTree.makePT "BinaryOperator" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "BinaryOperator",
                    Production = [|-21|],
                    PTOp = (fun args -> ParseTree.makePT "BinaryOperator" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "BinaryOperator",
                    Production = [|-22|],
                    PTOp = (fun args -> ParseTree.makePT "BinaryOperator" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "BinaryOperator",
                    Production = [|-23|],
                    PTOp = (fun args -> ParseTree.makePT "BinaryOperator" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "BinaryOperator",
                    Production = [|-24|],
                    PTOp = (fun args -> ParseTree.makePT "BinaryOperator" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 12   Rules of "Term_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Term_opt1",
                    Production = [|-16|],
                    PTOp = (fun args -> ParseTree.makePT "Term_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 2)
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 6, Next = 1)
                                    Tr(Tok = 13, Next = 2)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 2
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "Term_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Term_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 14   Rules of "Factor"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Factor",
                    Production = [|-17|],
                    PTOp = (fun args -> ParseTree.makePT "Factor" args),
                    Action = AST.boolean,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 1)
                                    Tr(Tok = 5, Next = 1)
                                    Tr(Tok = 13, Next = 3)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 3
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "Factor",
                    Production = [|-25|],
                    PTOp = (fun args -> ParseTree.makePT "Factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 16   Rules of "UnaryOperator"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "UnaryOperator",
                    Production = [|6|],
                    PTOp = (fun args -> ParseTree.makePT "UnaryOperator" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 6, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                        |]
                )

                // -----------------------------------------------------------------------------
                // 17   Rules of "Boolean"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Boolean",
                    Production = [|4|],
                    PTOp = (fun args -> ParseTree.makePT "Boolean" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 1)
                                    Tr(Tok = 5, Next = 2)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 2
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "Boolean",
                    Production = [|5|],
                    PTOp = (fun args -> ParseTree.makePT "Boolean" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 19   Rules of "AndOp"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "AndOp",
                    Production = [|7|],
                    PTOp = (fun args -> ParseTree.makePT "AndOp" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 7, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                        |]
                )

                // -----------------------------------------------------------------------------
                // 20   Rules of "OrOp"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "OrOp",
                    Production = [|8|],
                    PTOp = (fun args -> ParseTree.makePT "OrOp" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 8, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                        |]
                )

                // -----------------------------------------------------------------------------
                // 21   Rules of "XorOp"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "XorOp",
                    Production = [|9|],
                    PTOp = (fun args -> ParseTree.makePT "XorOp" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 9, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                        |]
                )

                // -----------------------------------------------------------------------------
                // 22   Rules of "NorOp"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "NorOp",
                    Production = [|10|],
                    PTOp = (fun args -> ParseTree.makePT "NorOp" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 10, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                        |]
                )

                // -----------------------------------------------------------------------------
                // 23   Rules of "NandOp"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "NandOp",
                    Production = [|11|],
                    PTOp = (fun args -> ParseTree.makePT "NandOp" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 11, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                        |]
                )

                // -----------------------------------------------------------------------------
                // 24   Rules of "XnorOp"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "XnorOp",
                    Production = [|12|],
                    PTOp = (fun args -> ParseTree.makePT "XnorOp" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 12, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                        |]
                )

                // -----------------------------------------------------------------------------
                // 25   Rules of "ParenthesisedExpression"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ParenthesisedExpression",
                    Production = [|13; 0; 14|],
                    PTOp = (fun args -> ParseTree.makePT "ParenthesisedExpression" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 13, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                        |]
                )
            |]
        
        let printError state token =
            printTokenError (stream.fileName) state token
            |> feedback.printError

        let indent () =
            String.replicate ruleDepth "| "

        let reportStartedRule prod ruleNumber name =
            let prodStr = prod |> Array.map (ParserData.ParserSymbol.toString printToken parserData) |> String.concat " "
            (sprintf "%s(#%-5d %s = %s;" (indent()) ruleNumber name prodStr) |> feedback.printMessage

        let reportTokenConsumption token =
            let tt = (token |> Token.typeOf) |> printToken
            token |> Token.toString |> sprintf "%s%s %s" (indent()) (stream.fileName) |> (fun s -> sprintf "%s %s" s tt) |> feedback.printMessage

        let reportFinishedRule () =
            sprintf "%s)" (indent()) |> feedback.printMessage


        let parserAccepted () =
            match parserStack with
            | [Symbol Token.EndOfInputToken] -> true
            | _ -> false

        let predictRule ruleNumber: (bool * int) =
            let ruleData = parserData.[ruleNumber]
            let (ParserRule(Name = baseRuleName; LookaheadDFA = cdfa)) = ruleData
            let ruleOffset = cdfa |> CompiledDfa.eval (stream.lookahead) printError (stream.k)
            
            if ruleOffset >= 0 then
                let predictedRule = parserData.[ruleNumber + ruleOffset]
                let (ParserRule(Name = predictedRuleName)) = predictedRule
                assert (baseRuleName = predictedRuleName)
                true, ruleNumber + ruleOffset
            else
                false, -1

        let pushRule ruleNumber: unit =
            let ruleData = parserData.[ruleNumber]
            let (ParserRule(Name = name; Production = prod; PTOp = ptOp; Action = action)) = ruleData
            if traceMode then
                reportStartedRule prod ruleNumber name
            ruleDepth <- ruleDepth + 1
            parserStack <-
                (prod
                |> Array.rev
                |> Array.fold (fun acc elem -> (Symbol elem) :: acc) [])
                @
                (PTOp (ptOp, prod.Length) ::Action (action) :: parserStack)

        let _parse ruleNumber: bool * ParseTree =
            let mutable error = false

            assert (ruleNumber = 0)
            let ok, nextRule = predictRule 0
            if ok then
                pushRule nextRule
            else
                let (ParserRule(Name = ruleName)) = parserData.[0]
                ParserSrcGenDiagnosis.diagnoseOutputPrediction feedback ruleName stream printToken
                error <- true

            while (not error) && (not (parserAccepted ())) do
                let entry = parserStack.Head
                match entry with
                | Symbol sym ->
                    if sym < 1 then
                        // Non-terminal
                        let ok, nextRule = predictRule -sym
                        if ok then
                            parserStack <- parserStack.Tail // Pop the non-terminal
                            pushRule nextRule
                        else
                            let (ParserRule(Name = ruleName)) = parserData.[-sym]
                            ParserSrcGenDiagnosis.diagnoseOutputPrediction feedback ruleName stream printToken
                            error <- true
                    else
                        // This is a token
                        let token = stream.lookahead 0
                        let ret = (token |> Token.typeOf) = sym
                        if ret then
                            stream.consume 1
                            parserStack <- parserStack.Tail // Pop the token
                            ptStack <- ParseTree(Tok(token), []) :: ptStack
                            if traceMode then
                                reportTokenConsumption token
                        else
                            diagnoseOutputWrongToken stream.fileName feedback token (printToken sym)
                            error <- true
                | PTOp (op, len) ->
                    let astList = ptStack |> List.take len |> List.rev
                    let modPt = op astList
                    ptStack <- modPt :: (ptStack |> List.skip len)
                    parserStack <- parserStack.Tail         // Pop the operation
                | Action act ->
                    userStack <- act feedback userStack (ptStack.Head |> ParseTree.childrenOf)
                    parserStack <- parserStack.Tail         // Pop the action
                    ruleDepth <- ruleDepth - 1
                    if traceMode then
                        reportFinishedRule ()

            if error then
                false, ParseTree.empty
            else
                true, ptStack.Head

        let res, ast = _parse 0
        if res then
            feedback.printMessage "Parsing succeeded"
        else
            feedback.printError "Parsing failed!"

        let res1 =
            if stream.allInputConsumed() |> not then
                feedback.printError "Input was not completely processed"
                stream.lookahead 0
                |> Token.toString
                |> sprintf "Last token was %s"
                |> feedback.printError
                false
            else
                res
        feedback.printMessage (sprintf "Warning(s): %d, Error(s): %d" (feedback.warningCount()) (feedback.errorCount()))
        res1, ast, userStack

    let lexer = BPLexer.createLexer k

    let parse fileName =
        let stream = provideTokenStream lexer fileName k
        let feedback = provideParserFeedback (printTokenError fileName)
        doParse stream feedback

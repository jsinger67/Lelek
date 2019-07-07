namespace SrcNamespace

module ParserModule =
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
            fileName (Token.toString token) (Token.typeOf token |> enum<LexerModule.TokenType>) toExpected
        |> feedback.printError

    let printToken = LexerModule.tokenTypeToString

    let printTokenError fileName (state: State) token: string =
        (sprintf "Error %s%s <%A> \nExpected one of "
            fileName (Token.toString token) (Token.typeOf token |> enum<LexerModule.TokenType>)) +
        (state.Transitions
        |> Array.map (fun (Tr(Tok = tok)) ->
            tok |> int |> printToken
        )
        |> String.concat " ")

    let k = 3

    let doParse (stream: TokenStream) (feedback: ParserFeedback) traceMode =
        let mutable userStack : AST.AST list = []
        let mutable parserStack: ParseType<AST.AST> list = [Symbol Token.EndOfInputToken]
        let mutable ruleDepth = 0
        let mutable ptStack: ParseTree list = []

        let parserData: UserParserData =
            [|

                // -----------------------------------------------------------------------------
                // 0    Rules of "calc"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "calc",
                    Production = [|-1|],
                    PTOp = (fun args -> ParseTree.clipPT "calc" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 1)
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
                // 1    Rules of "calc_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "calc_lst1",
                    Production = [|-3; -1|],
                    PTOp = (fun args -> ParseTree.collectPT "calc_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 1)
                                    Tr(Tok = 2147483647, Next = 5)
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
                                Id = 5
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "calc_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "calc_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 3    Rules of "calc_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "calc_lst1_itm1",
                    Production = [|-4; 4|],
                    PTOp = (fun args -> ParseTree.makePT "calc_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 1)
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
                // 4    Rules of "instruction"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "instruction",
                    Production = [|-6|],
                    PTOp = (fun args -> ParseTree.makePT "instruction" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 7)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 6)
                                    Tr(Tok = 20, Next = 12)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [|
                                    Tr(Tok = 14, Next = 2)
                                    Tr(Tok = 17, Next = 2)
                                    Tr(Tok = 19, Next = 2)
                                    Tr(Tok = 20, Next = 2)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 7
                                Transitions = [|
                                    Tr(Tok = 14, Next = 2)
                                    Tr(Tok = 17, Next = 2)
                                    Tr(Tok = 19, Next = 2)
                                    Tr(Tok = 20, Next = 2)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 2
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 6
                                Transitions = [|
                                    Tr(Tok = 15, Next = 2)
                                    Tr(Tok = 16, Next = 2)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 12
                                Transitions = [|
                                    Tr(Tok = 6, Next = 15)
                                    Tr(Tok = 15, Next = 2)
                                    Tr(Tok = 16, Next = 2)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 15
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "instruction",
                    Production = [|-7|],
                    PTOp = (fun args -> ParseTree.makePT "instruction" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 6    Rules of "logical_or"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "logical_or",
                    Production = [|-15; -16|],
                    PTOp = (fun args -> ParseTree.clipPT "logical_or" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 1)
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
                // 7    Rules of "assignment"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "assignment",
                    Production = [|-10; -12; -6|],
                    PTOp = (fun args -> ParseTree.clipPT "assignment" args),
                    Action = AST.assign,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 20, Next = 1)
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
                // 8    Rules of "equality_op"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "equality_op",
                    Production = [|5|],
                    PTOp = (fun args -> ParseTree.makePT "equality_op" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 1)
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
                // 9    Rules of "assign_op"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "assign_op",
                    Production = [|6|],
                    PTOp = (fun args -> ParseTree.makePT "assign_op" args),
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
                // 10   Rules of "assign_item"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "assign_item",
                    Production = [|-11; -9|],
                    PTOp = (fun args -> ParseTree.makePT "assign_item" args),
                    Action = AST.assignItem,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 20, Next = 1)
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
                // 11   Rules of "id"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "id",
                    Production = [|20|],
                    PTOp = (fun args -> ParseTree.makePT "id" args),
                    Action = AST.id,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 20, Next = 1)
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
                // 12   Rules of "assignment_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "assignment_lst1",
                    Production = [|-14; -12|],
                    PTOp = (fun args -> ParseTree.collectPT "assignment_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 9)
                                    Tr(Tok = 17, Next = 3)
                                    Tr(Tok = 19, Next = 8)
                                    Tr(Tok = 20, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 3
                                Transitions = [|
                                    Tr(Tok = 14, Next = 4)
                                    Tr(Tok = 17, Next = 4)
                                    Tr(Tok = 19, Next = 4)
                                    Tr(Tok = 20, Next = 4)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 9
                                Transitions = [|
                                    Tr(Tok = 14, Next = 4)
                                    Tr(Tok = 17, Next = 4)
                                    Tr(Tok = 19, Next = 4)
                                    Tr(Tok = 20, Next = 4)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [|
                                    Tr(Tok = 6, Next = 2)
                                    Tr(Tok = 15, Next = 4)
                                    Tr(Tok = 16, Next = 4)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 2
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 4
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 8
                                Transitions = [|
                                    Tr(Tok = 15, Next = 4)
                                    Tr(Tok = 16, Next = 4)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "assignment_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "assignment_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 14   Rules of "assignment_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "assignment_lst1_itm1",
                    Production = [|-10|],
                    PTOp = (fun args -> ParseTree.makePT "assignment_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 20, Next = 1)
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
                // 15   Rules of "logical_and"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "logical_and",
                    Production = [|-21; -22|],
                    PTOp = (fun args -> ParseTree.clipPT "logical_and" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 1)
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
                // 16   Rules of "logical_or_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "logical_or_lst1",
                    Production = [|-18; -16|],
                    PTOp = (fun args -> ParseTree.collectPT "logical_or_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 2)
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
                            {
                                Id = 2
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "logical_or_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "logical_or_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 18   Rules of "logical_or_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "logical_or_lst1_itm1",
                    Production = [|-19|],
                    PTOp = (fun args -> ParseTree.makePT "logical_or_lst1_itm1" args),
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
                // 19   Rules of "logical_or_item"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "logical_or_item",
                    Production = [|-20; -15|],
                    PTOp = (fun args -> ParseTree.makePT "logical_or_item" args),
                    Action = AST.logicalOrItem,
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
                // 20   Rules of "logical_or_op"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "logical_or_op",
                    Production = [|7|],
                    PTOp = (fun args -> ParseTree.makePT "logical_or_op" args),
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
                // 21   Rules of "bitwise_or"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_or",
                    Production = [|-27; -28|],
                    PTOp = (fun args -> ParseTree.clipPT "bitwise_or" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 1)
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
                // 22   Rules of "logical_and_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "logical_and_lst1",
                    Production = [|-24; -22|],
                    PTOp = (fun args -> ParseTree.collectPT "logical_and_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 7, Next = 2)
                                    Tr(Tok = 8, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 1
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
                    Name = "logical_and_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "logical_and_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 24   Rules of "logical_and_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "logical_and_lst1_itm1",
                    Production = [|-25|],
                    PTOp = (fun args -> ParseTree.makePT "logical_and_lst1_itm1" args),
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
                // 25   Rules of "logical_and_item"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "logical_and_item",
                    Production = [|-26; -21|],
                    PTOp = (fun args -> ParseTree.makePT "logical_and_item" args),
                    Action = AST.logicalAndItem,
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
                // 26   Rules of "logical_and_op"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "logical_and_op",
                    Production = [|8|],
                    PTOp = (fun args -> ParseTree.makePT "logical_and_op" args),
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
                // 27   Rules of "bitwise_and"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_and",
                    Production = [|-33; -34|],
                    PTOp = (fun args -> ParseTree.clipPT "bitwise_and" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 1)
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
                // 28   Rules of "bitwise_or_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_or_lst1",
                    Production = [|-30; -28|],
                    PTOp = (fun args -> ParseTree.collectPT "bitwise_or_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 8, Next = 2)
                                    Tr(Tok = 9, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 1
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
                    Name = "bitwise_or_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "bitwise_or_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 30   Rules of "bitwise_or_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_or_lst1_itm1",
                    Production = [|-31|],
                    PTOp = (fun args -> ParseTree.makePT "bitwise_or_lst1_itm1" args),
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
                // 31   Rules of "bitwise_or_item"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_or_item",
                    Production = [|-32; -27|],
                    PTOp = (fun args -> ParseTree.makePT "bitwise_or_item" args),
                    Action = AST.bitwiseOrItem,
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
                // 32   Rules of "bitwise_or_op"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_or_op",
                    Production = [|9|],
                    PTOp = (fun args -> ParseTree.makePT "bitwise_or_op" args),
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
                // 33   Rules of "equality"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "equality",
                    Production = [|-39; -40|],
                    PTOp = (fun args -> ParseTree.clipPT "equality" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 1)
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
                // 34   Rules of "bitwise_and_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_and_lst1",
                    Production = [|-36; -34|],
                    PTOp = (fun args -> ParseTree.collectPT "bitwise_and_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 9, Next = 2)
                                    Tr(Tok = 10, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 1
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
                    Name = "bitwise_and_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "bitwise_and_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 36   Rules of "bitwise_and_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_and_lst1_itm1",
                    Production = [|-37|],
                    PTOp = (fun args -> ParseTree.makePT "bitwise_and_lst1_itm1" args),
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
                // 37   Rules of "bitwise_and_item"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_and_item",
                    Production = [|-38; -33|],
                    PTOp = (fun args -> ParseTree.makePT "bitwise_and_item" args),
                    Action = AST.bitwiseAndItem,
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
                // 38   Rules of "bitwise_and_op"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_and_op",
                    Production = [|10|],
                    PTOp = (fun args -> ParseTree.makePT "bitwise_and_op" args),
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
                // 39   Rules of "relational"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "relational",
                    Production = [|-45; -46|],
                    PTOp = (fun args -> ParseTree.clipPT "relational" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 1)
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
                // 40   Rules of "equality_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "equality_lst1",
                    Production = [|-42; -40|],
                    PTOp = (fun args -> ParseTree.collectPT "equality_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 1)
                                    Tr(Tok = 10, Next = 2)
                                |]
                                Accepted = true
                                Prediction = 1
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
                    Name = "equality_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "equality_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 42   Rules of "equality_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "equality_lst1_itm1",
                    Production = [|-43|],
                    PTOp = (fun args -> ParseTree.makePT "equality_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 1)
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
                // 43   Rules of "equality_item"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "equality_item",
                    Production = [|-8; -39|],
                    PTOp = (fun args -> ParseTree.makePT "equality_item" args),
                    Action = AST.equalityItem,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 1)
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
                // 44   Rules of "bitwise_shift_op"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_shift_op",
                    Production = [|11|],
                    PTOp = (fun args -> ParseTree.makePT "bitwise_shift_op" args),
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
                // 45   Rules of "bitwise_shift"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_shift",
                    Production = [|-51; -52|],
                    PTOp = (fun args -> ParseTree.clipPT "bitwise_shift" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 1)
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
                // 46   Rules of "relational_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "relational_lst1",
                    Production = [|-48; -46|],
                    PTOp = (fun args -> ParseTree.collectPT "relational_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 12, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 1
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
                    Name = "relational_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "relational_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 48   Rules of "relational_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "relational_lst1_itm1",
                    Production = [|-49|],
                    PTOp = (fun args -> ParseTree.makePT "relational_lst1_itm1" args),
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
                // 49   Rules of "relational_item"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "relational_item",
                    Production = [|-50; -45|],
                    PTOp = (fun args -> ParseTree.makePT "relational_item" args),
                    Action = AST.relationalItem,
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
                // 50   Rules of "relational_op"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "relational_op",
                    Production = [|12|],
                    PTOp = (fun args -> ParseTree.makePT "relational_op" args),
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
                // 51   Rules of "summ"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "summ",
                    Production = [|-56; -57|],
                    PTOp = (fun args -> ParseTree.clipPT "summ" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 1)
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
                // 52   Rules of "bitwise_shift_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_shift_lst1",
                    Production = [|-54; -52|],
                    PTOp = (fun args -> ParseTree.collectPT "bitwise_shift_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 11, Next = 1)
                                    Tr(Tok = 12, Next = 2)
                                |]
                                Accepted = true
                                Prediction = 1
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
                    Name = "bitwise_shift_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "bitwise_shift_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 54   Rules of "bitwise_shift_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_shift_lst1_itm1",
                    Production = [|-55|],
                    PTOp = (fun args -> ParseTree.makePT "bitwise_shift_lst1_itm1" args),
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
                // 55   Rules of "bitwise_shift_item"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "bitwise_shift_item",
                    Production = [|-44; -51|],
                    PTOp = (fun args -> ParseTree.makePT "bitwise_shift_item" args),
                    Action = AST.bitwiseShiftItem,
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
                // 56   Rules of "mult"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "mult",
                    Production = [|-66; -67|],
                    PTOp = (fun args -> ParseTree.clipPT "mult" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 1)
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
                // 57   Rules of "summ_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "summ_lst1",
                    Production = [|-59; -57|],
                    PTOp = (fun args -> ParseTree.collectPT "summ_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 11, Next = 3)
                                    Tr(Tok = 13, Next = 1)
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 18, Next = 3)
                                |]
                                Accepted = true
                                Prediction = 1
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
                    Name = "summ_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "summ_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 59   Rules of "summ_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "summ_lst1_itm1",
                    Production = [|-60|],
                    PTOp = (fun args -> ParseTree.makePT "summ_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 13, Next = 1)
                                    Tr(Tok = 14, Next = 1)
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
                // 60   Rules of "summ_item"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "summ_item",
                    Production = [|-63; -56|],
                    PTOp = (fun args -> ParseTree.makePT "summ_item" args),
                    Action = AST.summItem,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 13, Next = 1)
                                    Tr(Tok = 14, Next = 1)
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
                // 61   Rules of "plus"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "plus",
                    Production = [|13|],
                    PTOp = (fun args -> ParseTree.makePT "plus" args),
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

                // -----------------------------------------------------------------------------
                // 62   Rules of "minus"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "minus",
                    Production = [|14|],
                    PTOp = (fun args -> ParseTree.makePT "minus" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
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
                // 63   Rules of "add_op"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "add_op",
                    Production = [|-61|],
                    PTOp = (fun args -> ParseTree.makePT "add_op" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 13, Next = 1)
                                    Tr(Tok = 14, Next = 2)
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
                    Name = "add_op",
                    Production = [|-62|],
                    PTOp = (fun args -> ParseTree.makePT "add_op" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 65   Rules of "pow_op"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "pow_op",
                    Production = [|15|],
                    PTOp = (fun args -> ParseTree.makePT "pow_op" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 15, Next = 1)
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
                // 66   Rules of "power"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "power",
                    Production = [|-72; -76|],
                    PTOp = (fun args -> ParseTree.clipPT "power" args),
                    Action = AST.power,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 17, Next = 1)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 1)
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
                // 67   Rules of "mult_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "mult_lst1",
                    Production = [|-69; -67|],
                    PTOp = (fun args -> ParseTree.collectPT "mult_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 13, Next = 2)
                                    Tr(Tok = 14, Next = 2)
                                    Tr(Tok = 16, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 1
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
                    Name = "mult_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "mult_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 69   Rules of "mult_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "mult_lst1_itm1",
                    Production = [|-70|],
                    PTOp = (fun args -> ParseTree.makePT "mult_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 16, Next = 1)
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
                // 70   Rules of "mult_item"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "mult_item",
                    Production = [|-71; -66|],
                    PTOp = (fun args -> ParseTree.makePT "mult_item" args),
                    Action = AST.multItem,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 16, Next = 1)
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
                // 71   Rules of "mult_op"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "mult_op",
                    Production = [|16|],
                    PTOp = (fun args -> ParseTree.makePT "mult_op" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 16, Next = 1)
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
                // 72   Rules of "factor"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "factor",
                    Production = [|-80|],
                    PTOp = (fun args -> ParseTree.makePT "factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 3)
                                    Tr(Tok = 17, Next = 4)
                                    Tr(Tok = 19, Next = 1)
                                    Tr(Tok = 20, Next = 2)
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
                        |]
                )
                ParserRule(
                    Name = "factor",
                    Production = [|-81|],
                    PTOp = (fun args -> ParseTree.makePT "factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "factor",
                    Production = [|-79; -72|],
                    PTOp = (fun args -> ParseTree.makePT "factor" args),
                    Action = AST.negate,
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "factor",
                    Production = [|17; -51; 18|],
                    PTOp = (fun args -> ParseTree.makePT "factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 76   Rules of "power_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "power_lst1",
                    Production = [|-78; -76|],
                    PTOp = (fun args -> ParseTree.collectPT "power_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 15, Next = 1)
                                    Tr(Tok = 16, Next = 2)
                                |]
                                Accepted = true
                                Prediction = 1
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
                    Name = "power_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "power_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 78   Rules of "power_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "power_lst1_itm1",
                    Production = [|-65; -72|],
                    PTOp = (fun args -> ParseTree.makePT "power_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 15, Next = 1)
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
                // 79   Rules of "negate"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "negate",
                    Production = [|-62|],
                    PTOp = (fun args -> ParseTree.makePT "negate" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
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
                // 80   Rules of "number"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "number",
                    Production = [|19|],
                    PTOp = (fun args -> ParseTree.makePT "number" args),
                    Action = AST.number,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 19, Next = 1)
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
                // 81   Rules of "idref"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "idref",
                    Production = [|-11|],
                    PTOp = (fun args -> ParseTree.makePT "idref" args),
                    Action = AST.idRef,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 20, Next = 1)
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

    let lexer = LexerModule.createLexer k

    let parse fileName =
        let stream = provideTokenStream lexer fileName k
        let feedback = provideParserFeedback (printTokenError fileName)
        doParse stream feedback

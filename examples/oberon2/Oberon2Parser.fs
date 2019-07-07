namespace Oberon2

module Oberon2Parser =
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
            fileName (Token.toString token) (Token.typeOf token |> enum<Oberon2Lexer.TokenType>) toExpected
        |> feedback.printError

    let printToken = Oberon2Lexer.tokenTypeToString

    let printTokenError fileName (state: State) token: string =
        (sprintf "Error %s%s <%A> \nExpected one of "
            fileName (Token.toString token) (Token.typeOf token |> enum<Oberon2Lexer.TokenType>)) +
        (state.Transitions
        |> Array.map (fun (Tr(Tok = tok)) ->
            tok |> int |> printToken
        )
        |> String.concat " ")

    let k = 7

    let doParse (stream: TokenStream) (feedback: ParserFeedback) traceMode =
        let mutable userStack : AST.AST list = []
        let mutable parserStack: ParseType<AST.AST> list = [Symbol Token.EndOfInputToken]
        let mutable ruleDepth = 0
        let mutable ptStack: ParseTree list = []

        let parserData: UserParserData =
            [|

                // -----------------------------------------------------------------------------
                // 0    Rules of "Module"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Module",
                    Production = [|3; -1; 4; -2; -4; -5; 5; -1; 6|],
                    PTOp = (fun args -> ParseTree.clipPT "Module" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 3, Next = 1)
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
                // 1    Rules of "ident"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ident",
                    Production = [|63|],
                    PTOp = (fun args -> ParseTree.makePT "ident" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
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
                // 2    Rules of "Module_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Module_opt1",
                    Production = [|-8|],
                    PTOp = (fun args -> ParseTree.makePT "Module_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 8, Next = 1)
                                    Tr(Tok = 11, Next = 2)
                                    Tr(Tok = 12, Next = 2)
                                    Tr(Tok = 13, Next = 2)
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
                    Name = "Module_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Module_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 4    Rules of "DeclSeq"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "DeclSeq",
                    Production = [|-16; -18|],
                    PTOp = (fun args -> ParseTree.clipPT "DeclSeq" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 11, Next = 1)
                                    Tr(Tok = 12, Next = 1)
                                    Tr(Tok = 13, Next = 1)
                                    Tr(Tok = 16, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
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
                // 5    Rules of "Module_opt2"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Module_opt2",
                    Production = [|7; -7|],
                    PTOp = (fun args -> ParseTree.clipPT "Module_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 2)
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
                    Name = "Module_opt2",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "Module_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 7    Rules of "StatementSeq"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "StatementSeq",
                    Production = [|-90; -91|],
                    PTOp = (fun args -> ParseTree.clipPT "StatementSeq" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 1)
                                    Tr(Tok = 25, Next = 1)
                                    Tr(Tok = 27, Next = 1)
                                    Tr(Tok = 28, Next = 1)
                                    Tr(Tok = 30, Next = 1)
                                    Tr(Tok = 32, Next = 1)
                                    Tr(Tok = 33, Next = 1)
                                    Tr(Tok = 34, Next = 1)
                                    Tr(Tok = 35, Next = 1)
                                    Tr(Tok = 36, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
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
                // 8    Rules of "ImportList"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ImportList",
                    Production = [|8; -9; -1; -11; 4|],
                    PTOp = (fun args -> ParseTree.clipPT "ImportList" args),
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
                // 9    Rules of "ImportList_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ImportList_opt1",
                    Production = [|-1; 9|],
                    PTOp = (fun args -> ParseTree.clipPT "ImportList_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [|
                                    Tr(Tok = 4, Next = 3)
                                    Tr(Tok = 9, Next = 2)
                                    Tr(Tok = 10, Next = 3)
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
                                Id = 3
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "ImportList_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "ImportList_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 11   Rules of "ImportList_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ImportList_lst1",
                    Production = [|-13; -11|],
                    PTOp = (fun args -> ParseTree.collectPT "ImportList_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 2)
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
                            {
                                Id = 2
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "ImportList_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "ImportList_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 13   Rules of "ImportList_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ImportList_lst1_itm1",
                    Production = [|10; -14; -1|],
                    PTOp = (fun args -> ParseTree.clipPT "ImportList_lst1_itm1" args),
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
                // 14   Rules of "ImportList_lst1_itm1_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ImportList_lst1_itm1_opt1",
                    Production = [|-1; 9|],
                    PTOp = (fun args -> ParseTree.makePT "ImportList_lst1_itm1_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [|
                                    Tr(Tok = 9, Next = 2)
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
                        |]
                )
                ParserRule(
                    Name = "ImportList_lst1_itm1_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "ImportList_lst1_itm1_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 16   Rules of "DeclSeq_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "DeclSeq_lst1",
                    Production = [|-24; -16|],
                    PTOp = (fun args -> ParseTree.collectPT "DeclSeq_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 11, Next = 1)
                                    Tr(Tok = 12, Next = 1)
                                    Tr(Tok = 13, Next = 1)
                                    Tr(Tok = 16, Next = 4)
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
                                Id = 4
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "DeclSeq_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "DeclSeq_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 18   Rules of "DeclSeq_lst2"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "DeclSeq_lst2",
                    Production = [|-20; -18|],
                    PTOp = (fun args -> ParseTree.collectPT "DeclSeq_lst2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 7, Next = 2)
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
                            {
                                Id = 2
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "DeclSeq_lst2",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "DeclSeq_lst2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 20   Rules of "DeclSeq_lst2_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "DeclSeq_lst2_itm1",
                    Production = [|-22; 4|],
                    PTOp = (fun args -> ParseTree.makePT "DeclSeq_lst2_itm1" args),
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
                                Transitions = [|
                                    Tr(Tok = 17, Next = 4)
                                    Tr(Tok = 18, Next = 2)
                                    Tr(Tok = 63, Next = 2)
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
                                Id = 4
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "DeclSeq_lst2_itm1",
                    Production = [|-23; 4|],
                    PTOp = (fun args -> ParseTree.makePT "DeclSeq_lst2_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 22   Rules of "ProcDecl"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ProcDecl",
                    Production = [|16; -47; -39; -49; 4; -4; -51; 5; -1|],
                    PTOp = (fun args -> ParseTree.clipPT "ProcDecl" args),
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
                // 23   Rules of "ForwardDecl"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ForwardDecl",
                    Production = [|16; 17; -55; -39; -57|],
                    PTOp = (fun args -> ParseTree.clipPT "ForwardDecl" args),
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
                // 24   Rules of "DeclSeq_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "DeclSeq_lst1_itm1",
                    Production = [|11; -27|],
                    PTOp = (fun args -> ParseTree.clipPT "DeclSeq_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 11, Next = 1)
                                    Tr(Tok = 12, Next = 2)
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
                        |]
                )
                ParserRule(
                    Name = "DeclSeq_lst1_itm1",
                    Production = [|12; -31|],
                    PTOp = (fun args -> ParseTree.clipPT "DeclSeq_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "DeclSeq_lst1_itm1",
                    Production = [|13; -35|],
                    PTOp = (fun args -> ParseTree.clipPT "DeclSeq_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 27   Rules of "DeclSeq_lst1_itm1_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "DeclSeq_lst1_itm1_lst1",
                    Production = [|-29; -27|],
                    PTOp = (fun args -> ParseTree.collectPT "DeclSeq_lst1_itm1_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 11, Next = 2)
                                    Tr(Tok = 12, Next = 2)
                                    Tr(Tok = 13, Next = 2)
                                    Tr(Tok = 63, Next = 1)
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
                    Name = "DeclSeq_lst1_itm1_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "DeclSeq_lst1_itm1_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 29   Rules of "DeclSeq_lst1_itm1_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "DeclSeq_lst1_itm1_lst1_itm1",
                    Production = [|-30; 4|],
                    PTOp = (fun args -> ParseTree.makePT "DeclSeq_lst1_itm1_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
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
                // 30   Rules of "ConstDecl"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ConstDecl",
                    Production = [|-39; 14; -40|],
                    PTOp = (fun args -> ParseTree.makePT "ConstDecl" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
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
                // 31   Rules of "DeclSeq_lst1_itm1_lst2"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "DeclSeq_lst1_itm1_lst2",
                    Production = [|-33; -31|],
                    PTOp = (fun args -> ParseTree.collectPT "DeclSeq_lst1_itm1_lst2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 11, Next = 2)
                                    Tr(Tok = 12, Next = 2)
                                    Tr(Tok = 13, Next = 2)
                                    Tr(Tok = 63, Next = 1)
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
                    Name = "DeclSeq_lst1_itm1_lst2",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "DeclSeq_lst1_itm1_lst2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 33   Rules of "DeclSeq_lst1_itm1_lst2_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "DeclSeq_lst1_itm1_lst2_itm1",
                    Production = [|-34; 4|],
                    PTOp = (fun args -> ParseTree.makePT "DeclSeq_lst1_itm1_lst2_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
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
                // 34   Rules of "TypeDecl"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "TypeDecl",
                    Production = [|-39; 14; -41|],
                    PTOp = (fun args -> ParseTree.makePT "TypeDecl" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
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
                // 35   Rules of "DeclSeq_lst1_itm1_lst3"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "DeclSeq_lst1_itm1_lst3",
                    Production = [|-37; -35|],
                    PTOp = (fun args -> ParseTree.collectPT "DeclSeq_lst1_itm1_lst3" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 11, Next = 2)
                                    Tr(Tok = 12, Next = 2)
                                    Tr(Tok = 13, Next = 2)
                                    Tr(Tok = 63, Next = 1)
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
                    Name = "DeclSeq_lst1_itm1_lst3",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "DeclSeq_lst1_itm1_lst3" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 37   Rules of "DeclSeq_lst1_itm1_lst3_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "DeclSeq_lst1_itm1_lst3_itm1",
                    Production = [|-38; 4|],
                    PTOp = (fun args -> ParseTree.makePT "DeclSeq_lst1_itm1_lst3_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
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
                // 38   Rules of "VarDecl"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "VarDecl",
                    Production = [|-46; 15; -41|],
                    PTOp = (fun args -> ParseTree.makePT "VarDecl" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
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
                // 39   Rules of "IdentDef"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "IdentDef",
                    Production = [|-1; -214|],
                    PTOp = (fun args -> ParseTree.clipPT "IdentDef" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
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
                // 40   Rules of "ConstExpr"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ConstExpr",
                    Production = [|-108|],
                    PTOp = (fun args -> ParseTree.makePT "ConstExpr" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
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
                // 41   Rules of "Type"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Type",
                    Production = [|-63|],
                    PTOp = (fun args -> ParseTree.makePT "Type" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 16, Next = 5)
                                    Tr(Tok = 20, Next = 2)
                                    Tr(Tok = 22, Next = 3)
                                    Tr(Tok = 23, Next = 4)
                                    Tr(Tok = 63, Next = 1)
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
                        |]
                )
                ParserRule(
                    Name = "Type",
                    Production = [|20; -75; 21; -41|],
                    PTOp = (fun args -> ParseTree.clipPT "Type" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Type",
                    Production = [|22; -80; -82; -83; 5|],
                    PTOp = (fun args -> ParseTree.clipPT "Type" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Type",
                    Production = [|23; 24; -41|],
                    PTOp = (fun args -> ParseTree.makePT "Type" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Type",
                    Production = [|16; -86|],
                    PTOp = (fun args -> ParseTree.clipPT "Type" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 46   Rules of "IdentList"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "IdentList",
                    Production = [|-39; -209|],
                    PTOp = (fun args -> ParseTree.clipPT "IdentList" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
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
                // 47   Rules of "ProcDecl_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ProcDecl_opt1",
                    Production = [|-54|],
                    PTOp = (fun args -> ParseTree.makePT "ProcDecl_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 63, Next = 2)
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
                    Name = "ProcDecl_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "ProcDecl_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 49   Rules of "ProcDecl_opt2"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ProcDecl_opt2",
                    Production = [|-53|],
                    PTOp = (fun args -> ParseTree.clipPT "ProcDecl_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 2)
                                    Tr(Tok = 18, Next = 1)
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
                    Name = "ProcDecl_opt2",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "ProcDecl_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 51   Rules of "ProcDecl_opt3"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ProcDecl_opt3",
                    Production = [|7; -7|],
                    PTOp = (fun args -> ParseTree.clipPT "ProcDecl_opt3" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 2)
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
                    Name = "ProcDecl_opt3",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "ProcDecl_opt3" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 53   Rules of "FormalPars"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "FormalPars",
                    Production = [|18; -59; 19; -61|],
                    PTOp = (fun args -> ParseTree.clipPT "FormalPars" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
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
                // 54   Rules of "Receiver"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Receiver",
                    Production = [|18; -73; -1; 15; -1; 19|],
                    PTOp = (fun args -> ParseTree.clipPT "Receiver" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
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
                // 55   Rules of "ForwardDecl_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ForwardDecl_opt1",
                    Production = [|-54|],
                    PTOp = (fun args -> ParseTree.makePT "ForwardDecl_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 63, Next = 2)
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
                    Name = "ForwardDecl_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "ForwardDecl_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 57   Rules of "ForwardDecl_opt2"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ForwardDecl_opt2",
                    Production = [|-53|],
                    PTOp = (fun args -> ParseTree.clipPT "ForwardDecl_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 2)
                                    Tr(Tok = 18, Next = 1)
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
                    Name = "ForwardDecl_opt2",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "ForwardDecl_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 59   Rules of "FormalPars_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "FormalPars_opt1",
                    Production = [|-64; -65|],
                    PTOp = (fun args -> ParseTree.clipPT "FormalPars_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 13, Next = 1)
                                    Tr(Tok = 19, Next = 3)
                                    Tr(Tok = 63, Next = 1)
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
                    Name = "FormalPars_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "FormalPars_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 61   Rules of "FormalPars_opt2"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "FormalPars_opt2",
                    Production = [|15; -63|],
                    PTOp = (fun args -> ParseTree.clipPT "FormalPars_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 2)
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 15, Next = 1)
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
                    Name = "FormalPars_opt2",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "FormalPars_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 63   Rules of "Qualident"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Qualident",
                    Production = [|-212; -1|],
                    PTOp = (fun args -> ParseTree.clipPT "Qualident" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
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
                // 64   Rules of "FPSection"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "FPSection",
                    Production = [|-68; -1; -70; 15; -41|],
                    PTOp = (fun args -> ParseTree.clipPT "FPSection" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 13, Next = 1)
                                    Tr(Tok = 63, Next = 1)
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
                // 65   Rules of "FormalPars_opt1_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "FormalPars_opt1_lst1",
                    Production = [|-67; -65|],
                    PTOp = (fun args -> ParseTree.collectPT "FormalPars_opt1_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 1)
                                    Tr(Tok = 19, Next = 2)
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
                    Name = "FormalPars_opt1_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "FormalPars_opt1_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 67   Rules of "FormalPars_opt1_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "FormalPars_opt1_lst1_itm1",
                    Production = [|4; -64|],
                    PTOp = (fun args -> ParseTree.makePT "FormalPars_opt1_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 1)
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
                // 68   Rules of "FPSection_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "FPSection_opt1",
                    Production = [|13|],
                    PTOp = (fun args -> ParseTree.clipPT "FPSection_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 13, Next = 1)
                                    Tr(Tok = 63, Next = 2)
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
                    Name = "FPSection_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "FPSection_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 70   Rules of "FPSection_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "FPSection_lst1",
                    Production = [|-72; -70|],
                    PTOp = (fun args -> ParseTree.collectPT "FPSection_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 15, Next = 2)
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
                    Name = "FPSection_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "FPSection_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 72   Rules of "FPSection_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "FPSection_lst1_itm1",
                    Production = [|10; -1|],
                    PTOp = (fun args -> ParseTree.makePT "FPSection_lst1_itm1" args),
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
                // 73   Rules of "Receiver_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Receiver_opt1",
                    Production = [|13|],
                    PTOp = (fun args -> ParseTree.makePT "Receiver_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 13, Next = 1)
                                    Tr(Tok = 63, Next = 2)
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
                    Name = "Receiver_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Receiver_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 75   Rules of "Type_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Type_opt1",
                    Production = [|-40; -77|],
                    PTOp = (fun args -> ParseTree.clipPT "Type_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 21, Next = 12)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 12
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "Type_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Type_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 77   Rules of "Type_opt1_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Type_opt1_lst1",
                    Production = [|-79; -77|],
                    PTOp = (fun args -> ParseTree.collectPT "Type_opt1_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 21, Next = 2)
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
                    Name = "Type_opt1_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Type_opt1_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 79   Rules of "Type_opt1_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Type_opt1_lst1_itm1",
                    Production = [|10; -40|],
                    PTOp = (fun args -> ParseTree.makePT "Type_opt1_lst1_itm1" args),
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
                // 80   Rules of "Type_opt2"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Type_opt2",
                    Production = [|18; -63; 19|],
                    PTOp = (fun args -> ParseTree.clipPT "Type_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 2)
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 63, Next = 2)
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
                    Name = "Type_opt2",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "Type_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 82   Rules of "FieldList"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "FieldList",
                    Production = [|-88|],
                    PTOp = (fun args -> ParseTree.clipPT "FieldList" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
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
                // 83   Rules of "Type_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Type_lst1",
                    Production = [|-85; -83|],
                    PTOp = (fun args -> ParseTree.collectPT "Type_lst1" args),
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
                    Name = "Type_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Type_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 85   Rules of "Type_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Type_lst1_itm1",
                    Production = [|4; -82|],
                    PTOp = (fun args -> ParseTree.makePT "Type_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 1)
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
                // 86   Rules of "Type_opt3"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Type_opt3",
                    Production = [|-53|],
                    PTOp = (fun args -> ParseTree.makePT "Type_opt3" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 2)
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 18, Next = 1)
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
                    Name = "Type_opt3",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Type_opt3" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 88   Rules of "FieldList_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "FieldList_opt1",
                    Production = [|-46; 15; -41|],
                    PTOp = (fun args -> ParseTree.makePT "FieldList_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 2)
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 63, Next = 1)
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
                    Name = "FieldList_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "FieldList_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 90   Rules of "Statement"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement",
                    Production = [|-94|],
                    PTOp = (fun args -> ParseTree.clipPT "Statement" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 1)
                                    Tr(Tok = 25, Next = 1)
                                    Tr(Tok = 27, Next = 1)
                                    Tr(Tok = 28, Next = 1)
                                    Tr(Tok = 30, Next = 1)
                                    Tr(Tok = 32, Next = 1)
                                    Tr(Tok = 33, Next = 1)
                                    Tr(Tok = 34, Next = 1)
                                    Tr(Tok = 35, Next = 1)
                                    Tr(Tok = 36, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
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
                // 91   Rules of "StatementSeq_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "StatementSeq_lst1",
                    Production = [|-93; -91|],
                    PTOp = (fun args -> ParseTree.collectPT "StatementSeq_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 1)
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 31, Next = 2)
                                    Tr(Tok = 37, Next = 2)
                                    Tr(Tok = 38, Next = 2)
                                    Tr(Tok = 39, Next = 2)
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
                    Name = "StatementSeq_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "StatementSeq_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 93   Rules of "StatementSeq_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "StatementSeq_lst1_itm1",
                    Production = [|4; -90|],
                    PTOp = (fun args -> ParseTree.makePT "StatementSeq_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 1)
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
                // 94   Rules of "Statement_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1",
                    Production = [|-105; -106|],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 11)
                                    Tr(Tok = 25, Next = 2)
                                    Tr(Tok = 27, Next = 3)
                                    Tr(Tok = 28, Next = 4)
                                    Tr(Tok = 30, Next = 5)
                                    Tr(Tok = 32, Next = 6)
                                    Tr(Tok = 33, Next = 7)
                                    Tr(Tok = 34, Next = 8)
                                    Tr(Tok = 35, Next = 9)
                                    Tr(Tok = 36, Next = 10)
                                    Tr(Tok = 63, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 10
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
                            {
                                Id = 7
                                Transitions = [||]
                                Accepted = true
                                Prediction = 6
                            }
                            {
                                Id = 8
                                Transitions = [||]
                                Accepted = true
                                Prediction = 7
                            }
                            {
                                Id = 9
                                Transitions = [||]
                                Accepted = true
                                Prediction = 8
                            }
                            {
                                Id = 10
                                Transitions = [||]
                                Accepted = true
                                Prediction = 9
                            }
                            {
                                Id = 11
                                Transitions = [||]
                                Accepted = true
                                Prediction = 10
                            }
                        |]
                )
                ParserRule(
                    Name = "Statement_opt1",
                    Production = [|25; -108; 26; -7; -111; -113; 5|],
                    PTOp = (fun args -> ParseTree.clipPT "Statement_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Statement_opt1",
                    Production = [|27; -108; 21; -115; -116; -118; 5|],
                    PTOp = (fun args -> ParseTree.clipPT "Statement_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Statement_opt1",
                    Production = [|28; -108; 29; -7; 5|],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Statement_opt1",
                    Production = [|30; -7; 31; -108|],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Statement_opt1",
                    Production = [|32; -1; 9; -108; 24; -108; -120; 29; -7; 5|],
                    PTOp = (fun args -> ParseTree.clipPT "Statement_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Statement_opt1",
                    Production = [|33; -7; 5|],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Statement_opt1",
                    Production = [|34; -122; 29; -7; -123; -125; 5|],
                    PTOp = (fun args -> ParseTree.clipPT "Statement_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Statement_opt1",
                    Production = [|35|],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Statement_opt1",
                    Production = [|36; -127|],
                    PTOp = (fun args -> ParseTree.clipPT "Statement_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Statement_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 105  Rules of "Designator"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Designator",
                    Production = [|-63; -200|],
                    PTOp = (fun args -> ParseTree.clipPT "Designator" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
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
                // 106  Rules of "Statement_opt1_rest"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_rest",
                    Production = [|9; -108|],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_rest" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 2)
                                    Tr(Tok = 9, Next = 1)
                                    Tr(Tok = 18, Next = 2)
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
                    Name = "Statement_opt1_rest",
                    Production = [|-109|],
                    PTOp = (fun args -> ParseTree.clipPT "Statement_opt1_rest" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 108  Rules of "Expr"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Expr",
                    Production = [|-143; -144|],
                    PTOp = (fun args -> ParseTree.clipPT "Expr" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
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
                // 109  Rules of "Statement_opt1_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_opt1",
                    Production = [|18; -129; 19|],
                    PTOp = (fun args -> ParseTree.clipPT "Statement_opt1_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 2)
                                    Tr(Tok = 18, Next = 1)
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
                    Name = "Statement_opt1_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 111  Rules of "Statement_opt1_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_lst1",
                    Production = [|-132; -111|],
                    PTOp = (fun args -> ParseTree.collectPT "Statement_opt1_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 37, Next = 2)
                                    Tr(Tok = 38, Next = 1)
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
                    Name = "Statement_opt1_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 113  Rules of "Statement_opt1_opt2"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_opt2",
                    Production = [|37; -7|],
                    PTOp = (fun args -> ParseTree.clipPT "Statement_opt1_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 37, Next = 1)
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
                    Name = "Statement_opt1_opt2",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "Statement_opt1_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 115  Rules of "Case"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Case",
                    Production = [|-135|],
                    PTOp = (fun args -> ParseTree.clipPT "Case" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 39, Next = 1)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
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
                // 116  Rules of "Statement_opt1_lst2"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_lst2",
                    Production = [|-133; -116|],
                    PTOp = (fun args -> ParseTree.collectPT "Statement_opt1_lst2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 37, Next = 2)
                                    Tr(Tok = 39, Next = 1)
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
                    Name = "Statement_opt1_lst2",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_lst2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 118  Rules of "Statement_opt1_opt3"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_opt3",
                    Production = [|37; -7|],
                    PTOp = (fun args -> ParseTree.clipPT "Statement_opt1_opt3" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 37, Next = 1)
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
                    Name = "Statement_opt1_opt3",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "Statement_opt1_opt3" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 120  Rules of "Statement_opt1_opt4"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_opt4",
                    Production = [|40; -40|],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_opt4" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 29, Next = 2)
                                    Tr(Tok = 40, Next = 1)
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
                    Name = "Statement_opt1_opt4",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_opt4" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 122  Rules of "Guard"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Guard",
                    Production = [|-63; 15; -63|],
                    PTOp = (fun args -> ParseTree.makePT "Guard" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
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
                // 123  Rules of "Statement_opt1_lst3"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_lst3",
                    Production = [|-134; -123|],
                    PTOp = (fun args -> ParseTree.collectPT "Statement_opt1_lst3" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 37, Next = 2)
                                    Tr(Tok = 39, Next = 1)
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
                    Name = "Statement_opt1_lst3",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_lst3" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 125  Rules of "Statement_opt1_opt5"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_opt5",
                    Production = [|37; -7|],
                    PTOp = (fun args -> ParseTree.clipPT "Statement_opt1_opt5" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 37, Next = 1)
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
                    Name = "Statement_opt1_opt5",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "Statement_opt1_opt5" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 127  Rules of "Statement_opt1_opt6"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_opt6",
                    Production = [|-108|],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_opt6" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 12)
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 12
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "Statement_opt1_opt6",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_opt6" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 129  Rules of "Statement_opt1_opt1_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_opt1_opt1",
                    Production = [|-131|],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_opt1_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 19, Next = 12)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 12
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "Statement_opt1_opt1_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_opt1_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 131  Rules of "ExprList"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ExprList",
                    Production = [|-108; -206|],
                    PTOp = (fun args -> ParseTree.clipPT "ExprList" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
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
                // 132  Rules of "Statement_opt1_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_lst1_itm1",
                    Production = [|38; -108; 26; -7|],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 38, Next = 1)
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
                // 133  Rules of "Statement_opt1_lst2_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_lst2_itm1",
                    Production = [|39; -115|],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_lst2_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 39, Next = 1)
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
                // 134  Rules of "Statement_opt1_lst3_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Statement_opt1_lst3_itm1",
                    Production = [|39; -122; 29; -7|],
                    PTOp = (fun args -> ParseTree.makePT "Statement_opt1_lst3_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 39, Next = 1)
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
                // 135  Rules of "Case_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Case_opt1",
                    Production = [|-137; -138; 15; -7|],
                    PTOp = (fun args -> ParseTree.clipPT "Case_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 37, Next = 12)
                                    Tr(Tok = 39, Next = 12)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 12
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "Case_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Case_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 137  Rules of "CaseLabels"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "CaseLabels",
                    Production = [|-40; -141|],
                    PTOp = (fun args -> ParseTree.clipPT "CaseLabels" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
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
                // 138  Rules of "Case_opt1_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Case_opt1_lst1",
                    Production = [|-140; -138|],
                    PTOp = (fun args -> ParseTree.collectPT "Case_opt1_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 15, Next = 2)
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
                    Name = "Case_opt1_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Case_opt1_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 140  Rules of "Case_opt1_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Case_opt1_lst1_itm1",
                    Production = [|10; -137|],
                    PTOp = (fun args -> ParseTree.makePT "Case_opt1_lst1_itm1" args),
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
                // 141  Rules of "CaseLabels_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "CaseLabels_opt1",
                    Production = [|41; -40|],
                    PTOp = (fun args -> ParseTree.makePT "CaseLabels_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 10, Next = 2)
                                    Tr(Tok = 15, Next = 2)
                                    Tr(Tok = 41, Next = 1)
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
                    Name = "CaseLabels_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "CaseLabels_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 143  Rules of "SimpleExpr"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "SimpleExpr",
                    Production = [|-158; -160; -161|],
                    PTOp = (fun args -> ParseTree.clipPT "SimpleExpr" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
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
                // 144  Rules of "Expr_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Expr_opt1",
                    Production = [|-146; -143|],
                    PTOp = (fun args -> ParseTree.makePT "Expr_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 9)
                                    Tr(Tok = 10, Next = 9)
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 15, Next = 9)
                                    Tr(Tok = 19, Next = 9)
                                    Tr(Tok = 21, Next = 9)
                                    Tr(Tok = 24, Next = 9)
                                    Tr(Tok = 26, Next = 9)
                                    Tr(Tok = 29, Next = 9)
                                    Tr(Tok = 41, Next = 9)
                                    Tr(Tok = 48, Next = 1)
                                    Tr(Tok = 49, Next = 1)
                                    Tr(Tok = 50, Next = 1)
                                    Tr(Tok = 51, Next = 1)
                                    Tr(Tok = 52, Next = 1)
                                    Tr(Tok = 53, Next = 1)
                                    Tr(Tok = 64, Next = 1)
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
                                Id = 9
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "Expr_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Expr_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 146  Rules of "Relation"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Relation",
                    Production = [|14|],
                    PTOp = (fun args -> ParseTree.makePT "Relation" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 48, Next = 2)
                                    Tr(Tok = 49, Next = 3)
                                    Tr(Tok = 50, Next = 4)
                                    Tr(Tok = 51, Next = 5)
                                    Tr(Tok = 52, Next = 6)
                                    Tr(Tok = 53, Next = 8)
                                    Tr(Tok = 64, Next = 7)
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
                            {
                                Id = 7
                                Transitions = [||]
                                Accepted = true
                                Prediction = 6
                            }
                            {
                                Id = 8
                                Transitions = [||]
                                Accepted = true
                                Prediction = 7
                            }
                        |]
                )
                ParserRule(
                    Name = "Relation",
                    Production = [|48|],
                    PTOp = (fun args -> ParseTree.makePT "Relation" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Relation",
                    Production = [|49|],
                    PTOp = (fun args -> ParseTree.makePT "Relation" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Relation",
                    Production = [|50|],
                    PTOp = (fun args -> ParseTree.makePT "Relation" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Relation",
                    Production = [|51|],
                    PTOp = (fun args -> ParseTree.makePT "Relation" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Relation",
                    Production = [|52|],
                    PTOp = (fun args -> ParseTree.makePT "Relation" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Relation",
                    Production = [|-199|],
                    PTOp = (fun args -> ParseTree.makePT "Relation" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Relation",
                    Production = [|53|],
                    PTOp = (fun args -> ParseTree.makePT "Relation" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 154  Rules of "Minus"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Minus",
                    Production = [|42|],
                    PTOp = (fun args -> ParseTree.makePT "Minus" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 42, Next = 1)
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
                // 155  Rules of "AddOpPlus"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "AddOpPlus",
                    Production = [|43|],
                    PTOp = (fun args -> ParseTree.makePT "AddOpPlus" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 43, Next = 1)
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
                // 156  Rules of "AddOpNumeric"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "AddOpNumeric",
                    Production = [|-155|],
                    PTOp = (fun args -> ParseTree.makePT "AddOpNumeric" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 42, Next = 2)
                                    Tr(Tok = 43, Next = 1)
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
                    Name = "AddOpNumeric",
                    Production = [|-154|],
                    PTOp = (fun args -> ParseTree.makePT "AddOpNumeric" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 158  Rules of "SimpleExpr_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "SimpleExpr_opt1",
                    Production = [|-156|],
                    PTOp = (fun args -> ParseTree.clipPT "SimpleExpr_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 3)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 3)
                                    Tr(Tok = 45, Next = 3)
                                    Tr(Tok = 46, Next = 3)
                                    Tr(Tok = 63, Next = 3)
                                    Tr(Tok = 65, Next = 3)
                                    Tr(Tok = 66, Next = 3)
                                    Tr(Tok = 67, Next = 3)
                                    Tr(Tok = 68, Next = 3)
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
                    Name = "SimpleExpr_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "SimpleExpr_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 160  Rules of "Term"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Term",
                    Production = [|-166; -174|],
                    PTOp = (fun args -> ParseTree.clipPT "Term" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
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
                // 161  Rules of "SimpleExpr_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "SimpleExpr_lst1",
                    Production = [|-163; -161|],
                    PTOp = (fun args -> ParseTree.collectPT "SimpleExpr_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 4)
                                    Tr(Tok = 10, Next = 4)
                                    Tr(Tok = 14, Next = 4)
                                    Tr(Tok = 15, Next = 4)
                                    Tr(Tok = 19, Next = 4)
                                    Tr(Tok = 21, Next = 4)
                                    Tr(Tok = 24, Next = 4)
                                    Tr(Tok = 26, Next = 4)
                                    Tr(Tok = 29, Next = 4)
                                    Tr(Tok = 41, Next = 4)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 48, Next = 4)
                                    Tr(Tok = 49, Next = 4)
                                    Tr(Tok = 50, Next = 4)
                                    Tr(Tok = 51, Next = 4)
                                    Tr(Tok = 52, Next = 4)
                                    Tr(Tok = 53, Next = 4)
                                    Tr(Tok = 54, Next = 1)
                                    Tr(Tok = 64, Next = 4)
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
                                Id = 4
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "SimpleExpr_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "SimpleExpr_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 163  Rules of "SimpleExpr_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "SimpleExpr_lst1_itm1",
                    Production = [|-164; -160|],
                    PTOp = (fun args -> ParseTree.makePT "SimpleExpr_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 54, Next = 1)
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
                // 164  Rules of "AddOp"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "AddOp",
                    Production = [|-156|],
                    PTOp = (fun args -> ParseTree.makePT "AddOp" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 54, Next = 3)
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
                    Name = "AddOp",
                    Production = [|54|],
                    PTOp = (fun args -> ParseTree.makePT "AddOp" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 166  Rules of "Factor"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Factor",
                    Production = [|-105; -182|],
                    PTOp = (fun args -> ParseTree.clipPT "Factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 8)
                                    Tr(Tok = 44, Next = 6)
                                    Tr(Tok = 45, Next = 9)
                                    Tr(Tok = 46, Next = 7)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 2)
                                    Tr(Tok = 66, Next = 4)
                                    Tr(Tok = 67, Next = 2)
                                    Tr(Tok = 68, Next = 5)
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
                                Id = 4
                                Transitions = [||]
                                Accepted = true
                                Prediction = 2
                            }
                            {
                                Id = 5
                                Transitions = [||]
                                Accepted = true
                                Prediction = 3
                            }
                            {
                                Id = 6
                                Transitions = [||]
                                Accepted = true
                                Prediction = 4
                            }
                            {
                                Id = 7
                                Transitions = [||]
                                Accepted = true
                                Prediction = 5
                            }
                            {
                                Id = 8
                                Transitions = [||]
                                Accepted = true
                                Prediction = 6
                            }
                            {
                                Id = 9
                                Transitions = [||]
                                Accepted = true
                                Prediction = 7
                            }
                        |]
                )
                ParserRule(
                    Name = "Factor",
                    Production = [|-186|],
                    PTOp = (fun args -> ParseTree.makePT "Factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Factor",
                    Production = [|-188|],
                    PTOp = (fun args -> ParseTree.makePT "Factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Factor",
                    Production = [|-189|],
                    PTOp = (fun args -> ParseTree.makePT "Factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Factor",
                    Production = [|44|],
                    PTOp = (fun args -> ParseTree.makePT "Factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Factor",
                    Production = [|-190|],
                    PTOp = (fun args -> ParseTree.makePT "Factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Factor",
                    Production = [|18; -108; 19|],
                    PTOp = (fun args -> ParseTree.makePT "Factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Factor",
                    Production = [|45; -166|],
                    PTOp = (fun args -> ParseTree.makePT "Factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 174  Rules of "Term_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Term_lst1",
                    Production = [|-176; -174|],
                    PTOp = (fun args -> ParseTree.collectPT "Term_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 42, Next = 6)
                                    Tr(Tok = 43, Next = 6)
                                    Tr(Tok = 54, Next = 6)
                                    Tr(Tok = 55, Next = 1)
                                    Tr(Tok = 56, Next = 1)
                                    Tr(Tok = 57, Next = 1)
                                    Tr(Tok = 58, Next = 1)
                                    Tr(Tok = 59, Next = 1)
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
                                Id = 6
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "Term_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Term_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 176  Rules of "Term_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Term_lst1_itm1",
                    Production = [|-177; -166|],
                    PTOp = (fun args -> ParseTree.makePT "Term_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 55, Next = 1)
                                    Tr(Tok = 56, Next = 1)
                                    Tr(Tok = 57, Next = 1)
                                    Tr(Tok = 58, Next = 1)
                                    Tr(Tok = 59, Next = 1)
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
                // 177  Rules of "MulOp"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "MulOp",
                    Production = [|55|],
                    PTOp = (fun args -> ParseTree.makePT "MulOp" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 55, Next = 1)
                                    Tr(Tok = 56, Next = 2)
                                    Tr(Tok = 57, Next = 3)
                                    Tr(Tok = 58, Next = 4)
                                    Tr(Tok = 59, Next = 5)
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
                        |]
                )
                ParserRule(
                    Name = "MulOp",
                    Production = [|56|],
                    PTOp = (fun args -> ParseTree.makePT "MulOp" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "MulOp",
                    Production = [|57|],
                    PTOp = (fun args -> ParseTree.makePT "MulOp" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "MulOp",
                    Production = [|58|],
                    PTOp = (fun args -> ParseTree.makePT "MulOp" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "MulOp",
                    Production = [|59|],
                    PTOp = (fun args -> ParseTree.makePT "MulOp" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 182  Rules of "Factor_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Factor_opt1",
                    Production = [|18; -184; 19|],
                    PTOp = (fun args -> ParseTree.clipPT "Factor_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 55, Next = 2)
                                    Tr(Tok = 56, Next = 2)
                                    Tr(Tok = 57, Next = 2)
                                    Tr(Tok = 58, Next = 2)
                                    Tr(Tok = 59, Next = 2)
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
                    Name = "Factor_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Factor_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 184  Rules of "Factor_opt1_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Factor_opt1_opt1",
                    Production = [|-131|],
                    PTOp = (fun args -> ParseTree.makePT "Factor_opt1_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 19, Next = 12)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 12
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "Factor_opt1_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Factor_opt1_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 186  Rules of "number"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "number",
                    Production = [|-217|],
                    PTOp = (fun args -> ParseTree.makePT "number" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 65, Next = 2)
                                    Tr(Tok = 67, Next = 1)
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
                    Name = "number",
                    Production = [|-218|],
                    PTOp = (fun args -> ParseTree.makePT "number" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 188  Rules of "character"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "character",
                    Production = [|66|],
                    PTOp = (fun args -> ParseTree.makePT "character" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 66, Next = 1)
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
                // 189  Rules of "string"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "string",
                    Production = [|68|],
                    PTOp = (fun args -> ParseTree.makePT "string" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 68, Next = 1)
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
                // 190  Rules of "Set"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Set",
                    Production = [|46; -191; 47|],
                    PTOp = (fun args -> ParseTree.clipPT "Set" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 46, Next = 1)
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
                // 191  Rules of "Set_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Set_opt1",
                    Production = [|-193; -194|],
                    PTOp = (fun args -> ParseTree.clipPT "Set_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 47, Next = 12)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 1
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 12
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "Set_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Set_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 193  Rules of "Element"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Element",
                    Production = [|-108; -197|],
                    PTOp = (fun args -> ParseTree.clipPT "Element" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 18, Next = 1)
                                    Tr(Tok = 42, Next = 1)
                                    Tr(Tok = 43, Next = 1)
                                    Tr(Tok = 44, Next = 1)
                                    Tr(Tok = 45, Next = 1)
                                    Tr(Tok = 46, Next = 1)
                                    Tr(Tok = 63, Next = 1)
                                    Tr(Tok = 65, Next = 1)
                                    Tr(Tok = 66, Next = 1)
                                    Tr(Tok = 67, Next = 1)
                                    Tr(Tok = 68, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 0
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
                // 194  Rules of "Set_opt1_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Set_opt1_lst1",
                    Production = [|-196; -194|],
                    PTOp = (fun args -> ParseTree.collectPT "Set_opt1_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 47, Next = 2)
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
                    Name = "Set_opt1_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Set_opt1_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 196  Rules of "Set_opt1_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Set_opt1_lst1_itm1",
                    Production = [|10; -193|],
                    PTOp = (fun args -> ParseTree.makePT "Set_opt1_lst1_itm1" args),
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
                // 197  Rules of "Element_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Element_opt1",
                    Production = [|41; -108|],
                    PTOp = (fun args -> ParseTree.makePT "Element_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 10, Next = 2)
                                    Tr(Tok = 41, Next = 1)
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
                    Name = "Element_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Element_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 199  Rules of "in"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "in",
                    Production = [|64|],
                    PTOp = (fun args -> ParseTree.makePT "in" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 64, Next = 1)
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
                // 200  Rules of "Designator_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Designator_lst1",
                    Production = [|-202; -200|],
                    PTOp = (fun args -> ParseTree.collectPT "Designator_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 1
                                Transitions = [|
                                    Tr(Tok = 63, Next = 2)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 3
                                Transitions = [|
                                    Tr(Tok = 18, Next = 277)
                                    Tr(Tok = 19, Next = 177)
                                    Tr(Tok = 42, Next = 275)
                                    Tr(Tok = 43, Next = 276)
                                    Tr(Tok = 44, Next = 303)
                                    Tr(Tok = 45, Next = 304)
                                    Tr(Tok = 46, Next = 331)
                                    Tr(Tok = 63, Next = 4)
                                    Tr(Tok = 65, Next = 303)
                                    Tr(Tok = 66, Next = 303)
                                    Tr(Tok = 67, Next = 303)
                                    Tr(Tok = 68, Next = 303)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 8
                                Transitions = [|
                                    Tr(Tok = 18, Next = 11)
                                    Tr(Tok = 42, Next = 9)
                                    Tr(Tok = 43, Next = 10)
                                    Tr(Tok = 44, Next = 37)
                                    Tr(Tok = 45, Next = 38)
                                    Tr(Tok = 46, Next = 65)
                                    Tr(Tok = 63, Next = 96)
                                    Tr(Tok = 65, Next = 37)
                                    Tr(Tok = 66, Next = 37)
                                    Tr(Tok = 67, Next = 37)
                                    Tr(Tok = 68, Next = 37)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 9
                                Transitions = [|
                                    Tr(Tok = 18, Next = 11)
                                    Tr(Tok = 44, Next = 37)
                                    Tr(Tok = 45, Next = 38)
                                    Tr(Tok = 46, Next = 65)
                                    Tr(Tok = 63, Next = 96)
                                    Tr(Tok = 65, Next = 37)
                                    Tr(Tok = 66, Next = 37)
                                    Tr(Tok = 67, Next = 37)
                                    Tr(Tok = 68, Next = 37)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 10
                                Transitions = [|
                                    Tr(Tok = 18, Next = 11)
                                    Tr(Tok = 44, Next = 37)
                                    Tr(Tok = 45, Next = 38)
                                    Tr(Tok = 46, Next = 65)
                                    Tr(Tok = 63, Next = 96)
                                    Tr(Tok = 65, Next = 37)
                                    Tr(Tok = 66, Next = 37)
                                    Tr(Tok = 67, Next = 37)
                                    Tr(Tok = 68, Next = 37)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 35
                                Transitions = [|
                                    Tr(Tok = 6, Next = 7)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 38
                                Transitions = [|
                                    Tr(Tok = 18, Next = 39)
                                    Tr(Tok = 44, Next = 7)
                                    Tr(Tok = 45, Next = 43)
                                    Tr(Tok = 46, Next = 53)
                                    Tr(Tok = 63, Next = 60)
                                    Tr(Tok = 65, Next = 7)
                                    Tr(Tok = 66, Next = 7)
                                    Tr(Tok = 67, Next = 7)
                                    Tr(Tok = 68, Next = 7)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 89
                                Transitions = [|
                                    Tr(Tok = 6, Next = 7)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 111
                                Transitions = [|
                                    Tr(Tok = 63, Next = 112)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 114
                                Transitions = [|
                                    Tr(Tok = 6, Next = 7)
                                    Tr(Tok = 19, Next = 7)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 117
                                Transitions = [|
                                    Tr(Tok = 18, Next = 120)
                                    Tr(Tok = 42, Next = 118)
                                    Tr(Tok = 43, Next = 119)
                                    Tr(Tok = 44, Next = 7)
                                    Tr(Tok = 45, Next = 124)
                                    Tr(Tok = 46, Next = 134)
                                    Tr(Tok = 63, Next = 141)
                                    Tr(Tok = 65, Next = 7)
                                    Tr(Tok = 66, Next = 7)
                                    Tr(Tok = 67, Next = 7)
                                    Tr(Tok = 68, Next = 7)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 118
                                Transitions = [|
                                    Tr(Tok = 18, Next = 120)
                                    Tr(Tok = 44, Next = 7)
                                    Tr(Tok = 45, Next = 124)
                                    Tr(Tok = 46, Next = 134)
                                    Tr(Tok = 63, Next = 141)
                                    Tr(Tok = 65, Next = 7)
                                    Tr(Tok = 66, Next = 7)
                                    Tr(Tok = 67, Next = 7)
                                    Tr(Tok = 68, Next = 7)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 119
                                Transitions = [|
                                    Tr(Tok = 18, Next = 120)
                                    Tr(Tok = 44, Next = 7)
                                    Tr(Tok = 45, Next = 124)
                                    Tr(Tok = 46, Next = 134)
                                    Tr(Tok = 63, Next = 141)
                                    Tr(Tok = 65, Next = 7)
                                    Tr(Tok = 66, Next = 7)
                                    Tr(Tok = 67, Next = 7)
                                    Tr(Tok = 68, Next = 7)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 147
                                Transitions = [|
                                    Tr(Tok = 63, Next = 7)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 149
                                Transitions = [|
                                    Tr(Tok = 63, Next = 7)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 173
                                Transitions = [|
                                    Tr(Tok = 18, Next = 176)
                                    Tr(Tok = 42, Next = 174)
                                    Tr(Tok = 43, Next = 175)
                                    Tr(Tok = 44, Next = 202)
                                    Tr(Tok = 45, Next = 203)
                                    Tr(Tok = 46, Next = 230)
                                    Tr(Tok = 63, Next = 261)
                                    Tr(Tok = 65, Next = 202)
                                    Tr(Tok = 66, Next = 202)
                                    Tr(Tok = 67, Next = 202)
                                    Tr(Tok = 68, Next = 202)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 174
                                Transitions = [|
                                    Tr(Tok = 18, Next = 176)
                                    Tr(Tok = 44, Next = 202)
                                    Tr(Tok = 45, Next = 203)
                                    Tr(Tok = 46, Next = 230)
                                    Tr(Tok = 63, Next = 261)
                                    Tr(Tok = 65, Next = 202)
                                    Tr(Tok = 66, Next = 202)
                                    Tr(Tok = 67, Next = 202)
                                    Tr(Tok = 68, Next = 202)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 175
                                Transitions = [|
                                    Tr(Tok = 18, Next = 176)
                                    Tr(Tok = 44, Next = 202)
                                    Tr(Tok = 45, Next = 203)
                                    Tr(Tok = 46, Next = 230)
                                    Tr(Tok = 63, Next = 261)
                                    Tr(Tok = 65, Next = 202)
                                    Tr(Tok = 66, Next = 202)
                                    Tr(Tok = 67, Next = 202)
                                    Tr(Tok = 68, Next = 202)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 200
                                Transitions = [|
                                    Tr(Tok = 6, Next = 177)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 203
                                Transitions = [|
                                    Tr(Tok = 18, Next = 204)
                                    Tr(Tok = 44, Next = 177)
                                    Tr(Tok = 45, Next = 208)
                                    Tr(Tok = 46, Next = 218)
                                    Tr(Tok = 63, Next = 225)
                                    Tr(Tok = 65, Next = 177)
                                    Tr(Tok = 66, Next = 177)
                                    Tr(Tok = 67, Next = 177)
                                    Tr(Tok = 68, Next = 177)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 254
                                Transitions = [|
                                    Tr(Tok = 6, Next = 177)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 275
                                Transitions = [|
                                    Tr(Tok = 18, Next = 277)
                                    Tr(Tok = 44, Next = 303)
                                    Tr(Tok = 45, Next = 304)
                                    Tr(Tok = 46, Next = 331)
                                    Tr(Tok = 63, Next = 362)
                                    Tr(Tok = 65, Next = 303)
                                    Tr(Tok = 66, Next = 303)
                                    Tr(Tok = 67, Next = 303)
                                    Tr(Tok = 68, Next = 303)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 276
                                Transitions = [|
                                    Tr(Tok = 18, Next = 277)
                                    Tr(Tok = 44, Next = 303)
                                    Tr(Tok = 45, Next = 304)
                                    Tr(Tok = 46, Next = 331)
                                    Tr(Tok = 63, Next = 362)
                                    Tr(Tok = 65, Next = 303)
                                    Tr(Tok = 66, Next = 303)
                                    Tr(Tok = 67, Next = 303)
                                    Tr(Tok = 68, Next = 303)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 301
                                Transitions = [|
                                    Tr(Tok = 6, Next = 177)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 304
                                Transitions = [|
                                    Tr(Tok = 18, Next = 305)
                                    Tr(Tok = 44, Next = 177)
                                    Tr(Tok = 45, Next = 309)
                                    Tr(Tok = 46, Next = 319)
                                    Tr(Tok = 63, Next = 326)
                                    Tr(Tok = 65, Next = 177)
                                    Tr(Tok = 66, Next = 177)
                                    Tr(Tok = 67, Next = 177)
                                    Tr(Tok = 68, Next = 177)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 355
                                Transitions = [|
                                    Tr(Tok = 6, Next = 177)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 6, Next = 1)
                                    Tr(Tok = 9, Next = 173)
                                    Tr(Tok = 18, Next = 3)
                                    Tr(Tok = 60, Next = 8)
                                    Tr(Tok = 62, Next = 2)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 2
                                Transitions = [|
                                    Tr(Tok = 6, Next = 111)
                                    Tr(Tok = 9, Next = 168)
                                    Tr(Tok = 18, Next = 113)
                                    Tr(Tok = 60, Next = 117)
                                    Tr(Tok = 62, Next = 112)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 4
                                Transitions = [|
                                    Tr(Tok = 6, Next = 5)
                                    Tr(Tok = 18, Next = 365)
                                    Tr(Tok = 19, Next = 7)
                                    Tr(Tok = 60, Next = 367)
                                    Tr(Tok = 62, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 5
                                Transitions = [|
                                    Tr(Tok = 63, Next = 6)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 6
                                Transitions = [|
                                    Tr(Tok = 19, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 7
                                Transitions = [||]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 11
                                Transitions = [|
                                    Tr(Tok = 18, Next = 14)
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                    Tr(Tok = 44, Next = 7)
                                    Tr(Tok = 45, Next = 18)
                                    Tr(Tok = 46, Next = 28)
                                    Tr(Tok = 63, Next = 35)
                                    Tr(Tok = 65, Next = 7)
                                    Tr(Tok = 66, Next = 7)
                                    Tr(Tok = 67, Next = 7)
                                    Tr(Tok = 68, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 14
                                Transitions = [|
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 18
                                Transitions = [|
                                    Tr(Tok = 18, Next = 7)
                                    Tr(Tok = 44, Next = 7)
                                    Tr(Tok = 45, Next = 7)
                                    Tr(Tok = 46, Next = 7)
                                    Tr(Tok = 63, Next = 7)
                                    Tr(Tok = 65, Next = 7)
                                    Tr(Tok = 66, Next = 7)
                                    Tr(Tok = 67, Next = 7)
                                    Tr(Tok = 68, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 28
                                Transitions = [|
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 37
                                Transitions = [|
                                    Tr(Tok = 55, Next = 7)
                                    Tr(Tok = 56, Next = 7)
                                    Tr(Tok = 57, Next = 7)
                                    Tr(Tok = 58, Next = 7)
                                    Tr(Tok = 59, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 39
                                Transitions = [|
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 43
                                Transitions = [|
                                    Tr(Tok = 18, Next = 7)
                                    Tr(Tok = 44, Next = 7)
                                    Tr(Tok = 45, Next = 7)
                                    Tr(Tok = 46, Next = 7)
                                    Tr(Tok = 63, Next = 7)
                                    Tr(Tok = 65, Next = 7)
                                    Tr(Tok = 66, Next = 7)
                                    Tr(Tok = 67, Next = 7)
                                    Tr(Tok = 68, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 53
                                Transitions = [|
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 60
                                Transitions = [|
                                    Tr(Tok = 6, Next = 7)
                                    Tr(Tok = 18, Next = 7)
                                    Tr(Tok = 60, Next = 7)
                                    Tr(Tok = 62, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 65
                                Transitions = [|
                                    Tr(Tok = 18, Next = 68)
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                    Tr(Tok = 44, Next = 7)
                                    Tr(Tok = 45, Next = 72)
                                    Tr(Tok = 46, Next = 82)
                                    Tr(Tok = 47, Next = 7)
                                    Tr(Tok = 63, Next = 89)
                                    Tr(Tok = 65, Next = 7)
                                    Tr(Tok = 66, Next = 7)
                                    Tr(Tok = 67, Next = 7)
                                    Tr(Tok = 68, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 68
                                Transitions = [|
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 72
                                Transitions = [|
                                    Tr(Tok = 18, Next = 7)
                                    Tr(Tok = 44, Next = 7)
                                    Tr(Tok = 45, Next = 7)
                                    Tr(Tok = 46, Next = 7)
                                    Tr(Tok = 63, Next = 7)
                                    Tr(Tok = 65, Next = 7)
                                    Tr(Tok = 66, Next = 7)
                                    Tr(Tok = 67, Next = 7)
                                    Tr(Tok = 68, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 82
                                Transitions = [|
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 96
                                Transitions = [|
                                    Tr(Tok = 6, Next = 97)
                                    Tr(Tok = 18, Next = 99)
                                    Tr(Tok = 60, Next = 101)
                                    Tr(Tok = 62, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 97
                                Transitions = [|
                                    Tr(Tok = 63, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 99
                                Transitions = [|
                                    Tr(Tok = 63, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 101
                                Transitions = [|
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 112
                                Transitions = [|
                                    Tr(Tok = 6, Next = 147)
                                    Tr(Tok = 18, Next = 149)
                                    Tr(Tok = 60, Next = 151)
                                    Tr(Tok = 62, Next = 163)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 113
                                Transitions = [|
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                    Tr(Tok = 63, Next = 114)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 120
                                Transitions = [|
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 124
                                Transitions = [|
                                    Tr(Tok = 18, Next = 7)
                                    Tr(Tok = 44, Next = 7)
                                    Tr(Tok = 45, Next = 7)
                                    Tr(Tok = 46, Next = 7)
                                    Tr(Tok = 63, Next = 7)
                                    Tr(Tok = 65, Next = 7)
                                    Tr(Tok = 66, Next = 7)
                                    Tr(Tok = 67, Next = 7)
                                    Tr(Tok = 68, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 134
                                Transitions = [|
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 141
                                Transitions = [|
                                    Tr(Tok = 6, Next = 7)
                                    Tr(Tok = 18, Next = 7)
                                    Tr(Tok = 60, Next = 7)
                                    Tr(Tok = 62, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 151
                                Transitions = [|
                                    Tr(Tok = 18, Next = 7)
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                    Tr(Tok = 44, Next = 7)
                                    Tr(Tok = 45, Next = 7)
                                    Tr(Tok = 46, Next = 7)
                                    Tr(Tok = 63, Next = 7)
                                    Tr(Tok = 65, Next = 7)
                                    Tr(Tok = 66, Next = 7)
                                    Tr(Tok = 67, Next = 7)
                                    Tr(Tok = 68, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 163
                                Transitions = [|
                                    Tr(Tok = 6, Next = 7)
                                    Tr(Tok = 18, Next = 7)
                                    Tr(Tok = 60, Next = 7)
                                    Tr(Tok = 62, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 168
                                Transitions = [|
                                    Tr(Tok = 42, Next = 7)
                                    Tr(Tok = 43, Next = 7)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 176
                                Transitions = [|
                                    Tr(Tok = 18, Next = 179)
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                    Tr(Tok = 44, Next = 177)
                                    Tr(Tok = 45, Next = 183)
                                    Tr(Tok = 46, Next = 193)
                                    Tr(Tok = 63, Next = 200)
                                    Tr(Tok = 65, Next = 177)
                                    Tr(Tok = 66, Next = 177)
                                    Tr(Tok = 67, Next = 177)
                                    Tr(Tok = 68, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 177
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 179
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 183
                                Transitions = [|
                                    Tr(Tok = 18, Next = 177)
                                    Tr(Tok = 44, Next = 177)
                                    Tr(Tok = 45, Next = 177)
                                    Tr(Tok = 46, Next = 177)
                                    Tr(Tok = 63, Next = 177)
                                    Tr(Tok = 65, Next = 177)
                                    Tr(Tok = 66, Next = 177)
                                    Tr(Tok = 67, Next = 177)
                                    Tr(Tok = 68, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 193
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 202
                                Transitions = [|
                                    Tr(Tok = 55, Next = 177)
                                    Tr(Tok = 56, Next = 177)
                                    Tr(Tok = 57, Next = 177)
                                    Tr(Tok = 58, Next = 177)
                                    Tr(Tok = 59, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 204
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 208
                                Transitions = [|
                                    Tr(Tok = 18, Next = 177)
                                    Tr(Tok = 44, Next = 177)
                                    Tr(Tok = 45, Next = 177)
                                    Tr(Tok = 46, Next = 177)
                                    Tr(Tok = 63, Next = 177)
                                    Tr(Tok = 65, Next = 177)
                                    Tr(Tok = 66, Next = 177)
                                    Tr(Tok = 67, Next = 177)
                                    Tr(Tok = 68, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 218
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 225
                                Transitions = [|
                                    Tr(Tok = 6, Next = 177)
                                    Tr(Tok = 18, Next = 177)
                                    Tr(Tok = 60, Next = 177)
                                    Tr(Tok = 62, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 230
                                Transitions = [|
                                    Tr(Tok = 18, Next = 233)
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                    Tr(Tok = 44, Next = 177)
                                    Tr(Tok = 45, Next = 237)
                                    Tr(Tok = 46, Next = 247)
                                    Tr(Tok = 47, Next = 177)
                                    Tr(Tok = 63, Next = 254)
                                    Tr(Tok = 65, Next = 177)
                                    Tr(Tok = 66, Next = 177)
                                    Tr(Tok = 67, Next = 177)
                                    Tr(Tok = 68, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 233
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 237
                                Transitions = [|
                                    Tr(Tok = 18, Next = 177)
                                    Tr(Tok = 44, Next = 177)
                                    Tr(Tok = 45, Next = 177)
                                    Tr(Tok = 46, Next = 177)
                                    Tr(Tok = 63, Next = 177)
                                    Tr(Tok = 65, Next = 177)
                                    Tr(Tok = 66, Next = 177)
                                    Tr(Tok = 67, Next = 177)
                                    Tr(Tok = 68, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 247
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 261
                                Transitions = [|
                                    Tr(Tok = 6, Next = 262)
                                    Tr(Tok = 18, Next = 264)
                                    Tr(Tok = 60, Next = 266)
                                    Tr(Tok = 62, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 262
                                Transitions = [|
                                    Tr(Tok = 63, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 264
                                Transitions = [|
                                    Tr(Tok = 63, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 266
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 277
                                Transitions = [|
                                    Tr(Tok = 18, Next = 280)
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                    Tr(Tok = 44, Next = 177)
                                    Tr(Tok = 45, Next = 284)
                                    Tr(Tok = 46, Next = 294)
                                    Tr(Tok = 63, Next = 301)
                                    Tr(Tok = 65, Next = 177)
                                    Tr(Tok = 66, Next = 177)
                                    Tr(Tok = 67, Next = 177)
                                    Tr(Tok = 68, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 280
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 284
                                Transitions = [|
                                    Tr(Tok = 18, Next = 177)
                                    Tr(Tok = 44, Next = 177)
                                    Tr(Tok = 45, Next = 177)
                                    Tr(Tok = 46, Next = 177)
                                    Tr(Tok = 63, Next = 177)
                                    Tr(Tok = 65, Next = 177)
                                    Tr(Tok = 66, Next = 177)
                                    Tr(Tok = 67, Next = 177)
                                    Tr(Tok = 68, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 294
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 303
                                Transitions = [|
                                    Tr(Tok = 55, Next = 177)
                                    Tr(Tok = 56, Next = 177)
                                    Tr(Tok = 57, Next = 177)
                                    Tr(Tok = 58, Next = 177)
                                    Tr(Tok = 59, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 305
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 309
                                Transitions = [|
                                    Tr(Tok = 18, Next = 177)
                                    Tr(Tok = 44, Next = 177)
                                    Tr(Tok = 45, Next = 177)
                                    Tr(Tok = 46, Next = 177)
                                    Tr(Tok = 63, Next = 177)
                                    Tr(Tok = 65, Next = 177)
                                    Tr(Tok = 66, Next = 177)
                                    Tr(Tok = 67, Next = 177)
                                    Tr(Tok = 68, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 319
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 326
                                Transitions = [|
                                    Tr(Tok = 6, Next = 177)
                                    Tr(Tok = 18, Next = 177)
                                    Tr(Tok = 60, Next = 177)
                                    Tr(Tok = 62, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 331
                                Transitions = [|
                                    Tr(Tok = 18, Next = 334)
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                    Tr(Tok = 44, Next = 177)
                                    Tr(Tok = 45, Next = 338)
                                    Tr(Tok = 46, Next = 348)
                                    Tr(Tok = 47, Next = 177)
                                    Tr(Tok = 63, Next = 355)
                                    Tr(Tok = 65, Next = 177)
                                    Tr(Tok = 66, Next = 177)
                                    Tr(Tok = 67, Next = 177)
                                    Tr(Tok = 68, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 334
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 338
                                Transitions = [|
                                    Tr(Tok = 18, Next = 177)
                                    Tr(Tok = 44, Next = 177)
                                    Tr(Tok = 45, Next = 177)
                                    Tr(Tok = 46, Next = 177)
                                    Tr(Tok = 63, Next = 177)
                                    Tr(Tok = 65, Next = 177)
                                    Tr(Tok = 66, Next = 177)
                                    Tr(Tok = 67, Next = 177)
                                    Tr(Tok = 68, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 348
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 362
                                Transitions = [|
                                    Tr(Tok = 6, Next = 363)
                                    Tr(Tok = 18, Next = 365)
                                    Tr(Tok = 60, Next = 367)
                                    Tr(Tok = 62, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 363
                                Transitions = [|
                                    Tr(Tok = 63, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 365
                                Transitions = [|
                                    Tr(Tok = 63, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 367
                                Transitions = [|
                                    Tr(Tok = 42, Next = 177)
                                    Tr(Tok = 43, Next = 177)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "Designator_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Designator_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 202  Rules of "Designator_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Designator_lst1_itm1",
                    Production = [|6; -1|],
                    PTOp = (fun args -> ParseTree.makePT "Designator_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 6, Next = 1)
                                    Tr(Tok = 18, Next = 4)
                                    Tr(Tok = 60, Next = 2)
                                    Tr(Tok = 62, Next = 3)
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
                    Name = "Designator_lst1_itm1",
                    Production = [|60; -131; 61|],
                    PTOp = (fun args -> ParseTree.makePT "Designator_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Designator_lst1_itm1",
                    Production = [|62|],
                    PTOp = (fun args -> ParseTree.makePT "Designator_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "Designator_lst1_itm1",
                    Production = [|18; -63; 19|],
                    PTOp = (fun args -> ParseTree.makePT "Designator_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 206  Rules of "ExprList_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ExprList_lst1",
                    Production = [|-208; -206|],
                    PTOp = (fun args -> ParseTree.collectPT "ExprList_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 19, Next = 2)
                                    Tr(Tok = 61, Next = 2)
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
                    Name = "ExprList_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "ExprList_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 208  Rules of "ExprList_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ExprList_lst1_itm1",
                    Production = [|10; -108|],
                    PTOp = (fun args -> ParseTree.makePT "ExprList_lst1_itm1" args),
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
                // 209  Rules of "IdentList_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "IdentList_lst1",
                    Production = [|-211; -209|],
                    PTOp = (fun args -> ParseTree.collectPT "IdentList_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 15, Next = 2)
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
                    Name = "IdentList_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "IdentList_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 211  Rules of "IdentList_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "IdentList_lst1_itm1",
                    Production = [|10; -39|],
                    PTOp = (fun args -> ParseTree.makePT "IdentList_lst1_itm1" args),
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
                // 212  Rules of "Qualident_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "Qualident_opt1",
                    Production = [|-1; 6|],
                    PTOp = (fun args -> ParseTree.makePT "Qualident_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 63, Next = 1)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 1
                                Transitions = [|
                                    Tr(Tok = 6, Next = 2)
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
                        |]
                )
                ParserRule(
                    Name = "Qualident_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "Qualident_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 214  Rules of "IdentDef_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "IdentDef_opt1",
                    Production = [|55|],
                    PTOp = (fun args -> ParseTree.makePT "IdentDef_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 4, Next = 3)
                                    Tr(Tok = 10, Next = 3)
                                    Tr(Tok = 14, Next = 3)
                                    Tr(Tok = 18, Next = 3)
                                    Tr(Tok = 42, Next = 2)
                                    Tr(Tok = 55, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 2
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
                        |]
                )
                ParserRule(
                    Name = "IdentDef_opt1",
                    Production = [|-154|],
                    PTOp = (fun args -> ParseTree.makePT "IdentDef_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "IdentDef_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "IdentDef_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 217  Rules of "integer"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "integer",
                    Production = [|67|],
                    PTOp = (fun args -> ParseTree.makePT "integer" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 67, Next = 1)
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
                // 218  Rules of "real"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "real",
                    Production = [|65|],
                    PTOp = (fun args -> ParseTree.makePT "real" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 65, Next = 1)
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

    let lexer = Oberon2Lexer.createLexer k

    let parse fileName =
        let stream = provideTokenStream lexer fileName k
        let feedback = provideParserFeedback (printTokenError fileName)
        doParse stream feedback

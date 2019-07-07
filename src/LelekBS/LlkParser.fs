namespace LelekBS

module LlkParser =
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
            fileName (Token.toString token) (Token.typeOf token |> enum<LlkLexer.TokenType>) toExpected
        |> feedback.printError

    let printToken = LlkLexer.tokenTypeToString

    let printTokenError fileName (state: State) token: string =
        (sprintf "Error %s%s <%A> \nExpected one of "
            fileName (Token.toString token) (Token.typeOf token |> enum<LlkLexer.TokenType>)) +
        (state.Transitions
        |> Array.map (fun (Tr(Tok = tok)) ->
            tok |> int |> printToken
        )
        |> String.concat " ")

    let k = 4

    let doParse (stream: TokenStream) (feedback: ParserFeedback) traceMode =
        let mutable userStack : AST.AST list = []
        let mutable parserStack: ParseType<AST.AST> list = [Symbol Token.EndOfInputToken]
        let mutable ruleDepth = 0
        let mutable ptStack: ParseTree list = []

        let parserData: UserParserData =
            [|

                // -----------------------------------------------------------------------------
                // 0    Rules of "grammar"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "grammar",
                    Production = [|-1; -2|],
                    PTOp = (fun args -> ParseTree.makePT "grammar" args),
                    Action = AST.grammar,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 2, Next = 1)
                                    Tr(Tok = 3, Next = 1)
                                    Tr(Tok = 14, Next = 1)
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
                // 1    Rules of "comment_decl"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "comment_decl",
                    Production = [|-3; -15; -17|],
                    PTOp = (fun args -> ParseTree.clipPT "comment_decl" args),
                    Action = AST.commentDecl,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 2, Next = 1)
                                    Tr(Tok = 3, Next = 1)
                                    Tr(Tok = 14, Next = 1)
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
                // 2    Rules of "rules"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "rules",
                    Production = [|-3; -21; -3; -22|],
                    PTOp = (fun args -> ParseTree.clipPT "rules" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 2, Next = 1)
                                    Tr(Tok = 3, Next = 1)
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
                // 3    Rules of "syntactic_comments"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "syntactic_comments",
                    Production = [|-4|],
                    PTOp = (fun args -> ParseTree.clipPT "syntactic_comments" args),
                    Action = AST.syntacticComments,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 2, Next = 1)
                                    Tr(Tok = 3, Next = 1)
                                    Tr(Tok = 14, Next = 1)
                                    Tr(Tok = 15, Next = 1)
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
                // 4    Rules of "syntactic_comments_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "syntactic_comments_lst1",
                    Production = [|-6; -4|],
                    PTOp = (fun args -> ParseTree.collectPT "syntactic_comments_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 2, Next = 1)
                                    Tr(Tok = 3, Next = 1)
                                    Tr(Tok = 14, Next = 3)
                                    Tr(Tok = 15, Next = 3)
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
                    Name = "syntactic_comments_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "syntactic_comments_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 6    Rules of "syntactic_comments_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "syntactic_comments_lst1_itm1",
                    Production = [|-7|],
                    PTOp = (fun args -> ParseTree.makePT "syntactic_comments_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 2, Next = 1)
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
                // 7    Rules of "comment"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "comment",
                    Production = [|-13|],
                    PTOp = (fun args -> ParseTree.makePT "comment" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 2, Next = 1)
                                    Tr(Tok = 3, Next = 2)
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
                    Name = "comment",
                    Production = [|-14|],
                    PTOp = (fun args -> ParseTree.makePT "comment" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 9    Rules of "ignored_comments"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ignored_comments",
                    Production = [|-10|],
                    PTOp = (fun args -> ParseTree.clipPT "ignored_comments" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 2, Next = 1)
                                    Tr(Tok = 3, Next = 1)
                                    Tr(Tok = 8, Next = 1)
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 12, Next = 1)
                                    Tr(Tok = 15, Next = 1)
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
                // 10   Rules of "ignored_comments_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ignored_comments_lst1",
                    Production = [|-12; -10|],
                    PTOp = (fun args -> ParseTree.collectPT "ignored_comments_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 2, Next = 1)
                                    Tr(Tok = 3, Next = 1)
                                    Tr(Tok = 7, Next = 3)
                                    Tr(Tok = 8, Next = 3)
                                    Tr(Tok = 10, Next = 3)
                                    Tr(Tok = 12, Next = 3)
                                    Tr(Tok = 15, Next = 3)
                                    Tr(Tok = 16, Next = 3)
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
                    Name = "ignored_comments_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "ignored_comments_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 12   Rules of "ignored_comments_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "ignored_comments_lst1_itm1",
                    Production = [|-7|],
                    PTOp = (fun args -> ParseTree.makePT "ignored_comments_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 2, Next = 1)
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
                // 13   Rules of "line_comment"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "line_comment",
                    Production = [|2|],
                    PTOp = (fun args -> ParseTree.makePT "line_comment" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 2, Next = 1)
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
                // 14   Rules of "block_comment"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "block_comment",
                    Production = [|3|],
                    PTOp = (fun args -> ParseTree.makePT "block_comment" args),
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
                // 15   Rules of "comment_decl_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "comment_decl_opt1",
                    Production = [|-20|],
                    PTOp = (fun args -> ParseTree.makePT "comment_decl_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 1
                                Transitions = [|
                                    Tr(Tok = 16, Next = 2)
                                |]
                                Accepted = false
                                Prediction = -1
                            }
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 14, Next = 1)
                                |]
                                Accepted = true
                                Prediction = 1
                            }
                            {
                                Id = 2
                                Transitions = [|
                                    Tr(Tok = 14, Next = 3)
                                    Tr(Tok = 16, Next = 4)
                                |]
                                Accepted = true
                                Prediction = 0
                            }
                            {
                                Id = 3
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
                    Name = "comment_decl_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "comment_decl_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 17   Rules of "comment_decl_opt2"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "comment_decl_opt2",
                    Production = [|-19|],
                    PTOp = (fun args -> ParseTree.clipPT "comment_decl_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 2, Next = 2)
                                    Tr(Tok = 3, Next = 2)
                                    Tr(Tok = 14, Next = 1)
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
                    Name = "comment_decl_opt2",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "comment_decl_opt2" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 19   Rules of "block_comment_decl"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "block_comment_decl",
                    Production = [|14; -49; -49|],
                    PTOp = (fun args -> ParseTree.makePT "block_comment_decl" args),
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
                // 20   Rules of "line_comment_decl"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "line_comment_decl",
                    Production = [|14; -49|],
                    PTOp = (fun args -> ParseTree.makePT "line_comment_decl" args),
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
                // 21   Rules of "rule"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "rule",
                    Production = [|-25; 4; -9; -26; -9; 5|],
                    PTOp = (fun args -> ParseTree.makePT "rule" args),
                    Action = AST.rule,
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
                // 22   Rules of "rules_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "rules_lst1",
                    Production = [|-24; -22|],
                    PTOp = (fun args -> ParseTree.collectPT "rules_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 15, Next = 1)
                                    Tr(Tok = 2147483647, Next = 2)
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
                    Name = "rules_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "rules_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 24   Rules of "rules_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "rules_lst1_itm1",
                    Production = [|-21; -3|],
                    PTOp = (fun args -> ParseTree.makePT "rules_lst1_itm1" args),
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
                // 25   Rules of "non_terminal"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "non_terminal",
                    Production = [|-27|],
                    PTOp = (fun args -> ParseTree.makePT "non_terminal" args),
                    Action = AST.nonTerminal,
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
                // 26   Rules of "expression"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "expression",
                    Production = [|-28|],
                    PTOp = (fun args -> ParseTree.makePT "expression" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 7, Next = 1)
                                    Tr(Tok = 8, Next = 1)
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 12, Next = 1)
                                    Tr(Tok = 15, Next = 1)
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
                // 27   Rules of "identifier"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "identifier",
                    Production = [|15|],
                    PTOp = (fun args -> ParseTree.makePT "identifier" args),
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
                // 28   Rules of "alternations"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "alternations",
                    Production = [|-29; -30|],
                    PTOp = (fun args -> ParseTree.clipPT "alternations" args),
                    Action = AST.alternations,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 7, Next = 1)
                                    Tr(Tok = 8, Next = 1)
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 12, Next = 1)
                                    Tr(Tok = 15, Next = 1)
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
                // 29   Rules of "alt"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "alt",
                    Production = [|-33; -35|],
                    PTOp = (fun args -> ParseTree.clipPT "alt" args),
                    Action = AST.alt,
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 7, Next = 1)
                                    Tr(Tok = 8, Next = 1)
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 12, Next = 1)
                                    Tr(Tok = 15, Next = 1)
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
                // 30   Rules of "alternations_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "alternations_lst1",
                    Production = [|-32; -30|],
                    PTOp = (fun args -> ParseTree.collectPT "alternations_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 2, Next = 2)
                                    Tr(Tok = 3, Next = 2)
                                    Tr(Tok = 5, Next = 2)
                                    Tr(Tok = 6, Next = 1)
                                    Tr(Tok = 9, Next = 2)
                                    Tr(Tok = 11, Next = 2)
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
                    Name = "alternations_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "alternations_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 32   Rules of "alternations_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "alternations_lst1_itm1",
                    Production = [|6; -29|],
                    PTOp = (fun args -> ParseTree.makePT "alternations_lst1_itm1" args),
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
                // 33   Rules of "alt_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "alt_lst1",
                    Production = [|-38; -33|],
                    PTOp = (fun args -> ParseTree.collectPT "alt_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 7, Next = 6)
                                    Tr(Tok = 8, Next = 1)
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 12, Next = 1)
                                    Tr(Tok = 15, Next = 1)
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
                                Id = 6
                                Transitions = [||]
                                Accepted = true
                                Prediction = 1
                            }
                        |]
                )
                ParserRule(
                    Name = "alt_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "alt_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 35   Rules of "alt_opt1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "alt_opt1",
                    Production = [|-37|],
                    PTOp = (fun args -> ParseTree.clipPT "alt_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 6, Next = 2)
                                    Tr(Tok = 7, Next = 1)
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
                    Name = "alt_opt1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.clipPT "alt_opt1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 37   Rules of "action"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "action",
                    Production = [|7; -48|],
                    PTOp = (fun args -> ParseTree.makePT "action" args),
                    Action = AST.userAction,
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
                // 38   Rules of "alt_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "alt_lst1_itm1",
                    Production = [|-39|],
                    PTOp = (fun args -> ParseTree.makePT "alt_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 8, Next = 1)
                                    Tr(Tok = 10, Next = 1)
                                    Tr(Tok = 12, Next = 1)
                                    Tr(Tok = 15, Next = 1)
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
                // 39   Rules of "factor"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "factor",
                    Production = [|-44|],
                    PTOp = (fun args -> ParseTree.makePT "factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 8, Next = 1)
                                    Tr(Tok = 10, Next = 2)
                                    Tr(Tok = 12, Next = 3)
                                    Tr(Tok = 15, Next = 4)
                                    Tr(Tok = 16, Next = 5)
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
                    Name = "factor",
                    Production = [|-45|],
                    PTOp = (fun args -> ParseTree.makePT "factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "factor",
                    Production = [|-46|],
                    PTOp = (fun args -> ParseTree.makePT "factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "factor",
                    Production = [|-25|],
                    PTOp = (fun args -> ParseTree.makePT "factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )
                ParserRule(
                    Name = "factor",
                    Production = [|-47|],
                    PTOp = (fun args -> ParseTree.makePT "factor" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 44   Rules of "group"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "group",
                    Production = [|8; -26; 9|],
                    PTOp = (fun args -> ParseTree.makePT "group" args),
                    Action = AST.group,
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
                // 45   Rules of "repeat"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "repeat",
                    Production = [|10; -26; 11|],
                    PTOp = (fun args -> ParseTree.makePT "repeat" args),
                    Action = AST.repeat,
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
                // 46   Rules of "optional"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "optional",
                    Production = [|12; -26; 13|],
                    PTOp = (fun args -> ParseTree.makePT "optional" args),
                    Action = AST.optional,
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
                // 47   Rules of "terminal"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "terminal",
                    Production = [|-49|],
                    PTOp = (fun args -> ParseTree.makePT "terminal" args),
                    Action = AST.terminal,
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
                // 48   Rules of "user_action"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "user_action",
                    Production = [|-27; -50|],
                    PTOp = (fun args -> ParseTree.clipPT "user_action" args),
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
                // 49   Rules of "string"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "string",
                    Production = [|16|],
                    PTOp = (fun args -> ParseTree.makePT "string" args),
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
                // 50   Rules of "user_action_lst1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "user_action_lst1",
                    Production = [|-52; -50|],
                    PTOp = (fun args -> ParseTree.collectPT "user_action_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 6, Next = 2)
                                    Tr(Tok = 17, Next = 1)
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
                    Name = "user_action_lst1",
                    Production = [||],
                    PTOp = (fun args -> ParseTree.makePT "user_action_lst1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = [||]
                )

                // -----------------------------------------------------------------------------
                // 52   Rules of "user_action_lst1_itm1"
                // -----------------------------------------------------------------------------
                ParserRule(
                    Name = "user_action_lst1_itm1",
                    Production = [|17; -27|],
                    PTOp = (fun args -> ParseTree.makePT "user_action_lst1_itm1" args),
                    Action = (fun _ state _ -> state),
                    LookaheadDFA = 
                        [|
                            {
                                Id = 0
                                Transitions = [|
                                    Tr(Tok = 17, Next = 1)
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

    let lexer = LlkLexer.createLexer k

    let parse fileName =
        let stream = provideTokenStream lexer fileName k
        let feedback = provideParserFeedback (printTokenError fileName)
        doParse stream feedback

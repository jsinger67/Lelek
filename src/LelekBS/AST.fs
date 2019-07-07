namespace LelekBS

module AST =
    open LelekParser
    open Utils
    open Token
    open ParseTree
    open ParserFeedback
    open LlkGrammar
    open LlkLexer

    type AST =
        | Cmnts of string list
        | Factr of LlkGrammar.Factor
        | CmtDcl of CommentDecl
        | Action of string
        | Factors of AST list
        | Alt of LlkGrammar.Alternation
        | Expr of LlkGrammar.Expression
        | Rl of LlkGrammar.Rule
        | Gr of LlkGrammar.LlkData
        | Err of string

    let mutable fileName = ""

    let init fn =
        fileName <- fn |> System.IO.Path.GetFullPath

    let private printError (feedback: ParserFeedback) msg (ParseTree(Item = item) as pt) =
        let tokenOpt = ParseTree.digUpToken pt 
        let scope = "Error AST handling"
        match tokenOpt with
        | Some token ->
            feedback.printError (sprintf "%s: '%s' at %s%s" scope msg fileName (token |> Token.toString))
        | None ->
            match item with
            | PTItem.Var v ->
                feedback.printError (sprintf "%s: '%s' at %s%s" scope msg fileName v)
            | _ ->
                feedback.printError (sprintf "%s: '%s' in %s" scope msg fileName)

    let private printStackError (feedback: ParserFeedback) (stack: AST list) msg =
        let scope = "Error stack handling"
        match stack with
        | [] ->
            feedback.printError (sprintf "%s: '%s' at %s" scope msg fileName)
        | h::_ ->
            feedback.printError (sprintf "%s: '%s' in %s TOS: %A" scope msg fileName h)

    let private getSyntacticComments pt =
        pt
        |> ParseTree.digUpVar "syntactic_comments_lst1"
        |> Option.get
        |> ParseTree.childrenOf
        |> List.map (ParseTree.typeAndTextOfToken >> snd)

    // -------------------------------------------------------------------------
    // User actions
    // -------------------------------------------------------------------------
    let syntacticComments (_: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let cmnts =
            match args with
            | [] ->
                []
            | _ ->
                args.[0]
                |> ParseTree.fold (fun acc pt ->
                    match pt with
                    | (ParseTree((Tok(Token(Symbol = txt))), _)) -> txt::acc
                    | _ -> acc) []
                |> List.rev
        Cmnts(cmnts)::stack

    let commentDecl (_: ParserFeedback) (_: AST list) (args: ParseTree list): AST list =
        let optIndices =
            args
            |> ParseTreeList.indices ["syntactic_comments"; "comment_decl_opt1"; "comment_decl_opt2"]

        let comments =
            optIndices.[0]
            |> Option.map (fun i -> args.[i] |> getSyntacticComments)
            |> Option.defaultValue []

        let lnComment =
            optIndices.[1]
            |> Option.map (fun i ->
                args.[i]
                |> ParseTree.digUpVar "line_comment_decl"
                |> Option.get
                |> ParseTree.childrenOf
                |> List.item 1
                |> ParseTree.typeAndTextOfToken
                |> snd
                |> (fun t -> t.Trim([|'"'|])))

        let blComment =
            optIndices.[2]
            |> Option.map (fun i ->
                let children =
                    args.[i]
                    |> ParseTree.digUpVar "block_comment_decl"
                    |> Option.get
                    |> ParseTree.childrenOf
                children.[1]
                |> ParseTree.typeAndTextOfToken
                |> snd
                |> (fun t -> t.Trim([|'"'|])),
                children.[2]
                |> ParseTree.typeAndTextOfToken
                |> snd
                |> (fun t -> t.Trim([|'"'|])))

        [CmtDcl(CommentDecl(comments, lnComment, blComment))]

    let nonTerminal (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let tt, tx = ParseTree.typeAndTextOfToken args.[0]
        if tt <> int TokenType.TkIdentifier then
            printError feedback "nonTerminal: Invalid token type" args.[0]
            Err("nonTerminal: Invalid token type")::stack
        else
            Factr(N(LlkVariable(tx)))::stack

    let terminal (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let tt, tx = ParseTree.typeAndTextOfToken args.[0]
        if tt <> int TokenType.TkString then
            printError feedback "terminal: Invalid token type" args.[0]
            Err("terminal: Invalid token type")::stack
        else
            // Strip quotes
            let trimmedTxt = tx.Substring(1).Substring(0, tx.Length - 2)
            Factr(T(LlkTerminal(trimmedTxt)))::stack

    let alt (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let ruleLenght =
            match args with
            | [] -> 0
            | _ ->  args.[0] |> ParseTree.childrenOf |> List.length

        let factors stack =
            stack
            |> List.take ruleLenght
            |> List.map (fun a ->
                match a with
                | Factr f ->
                    f
                | _ ->
                    printStackError feedback [a] "alt: Invalid stack entry"
                    a |> sprintf "%A" |> LlkVariable |> N)
            |> List.rev

        match stack with
        | Action(act)::t ->
            Alt(A(Factors = factors t, Action = Some(act)))::(t |> List.skip ruleLenght)
        | _ ->
            Alt(A(Factors = factors stack, Action = None))::(stack |> List.skip ruleLenght)

    let group (feedback: ParserFeedback) (stack: AST list) (_: ParseTree list): AST list =
        match stack with
        | Expr(e)::t ->
            Factr(G(Group(e)))::t
        | _ ->
            printStackError feedback stack "group: Invalid stack entry"
            stack

    let repeat (feedback: ParserFeedback) (stack: AST list) (_: ParseTree list): AST list =
        match stack with
        | Expr(e)::t ->
            Factr(R(Repeat(e)))::t
        | _ ->
            printStackError feedback stack "repeat: Invalid stack entry"
            stack

    let optional (feedback: ParserFeedback) (stack: AST list) (_: ParseTree list): AST list =
        match stack with
        | Expr(e)::t ->
            Factr(O(Optional(e)))::t
        | _ ->
            printStackError feedback stack "optional: Invalid stack entry"
            stack

    let userAction (_: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let txt =
            args.[1]
            |> ParseTree.fold (fun acc pt ->
                match pt with
                | (ParseTree(Item = (Tok(Token(Symbol = txt))))) -> acc + txt
                | _ -> acc
            ) ""
        Action(txt)::stack


    let alternations (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let optIndices =
            args
            |> ParseTreeList.indices ["alt"; "alternations_lst1"]
        let altCount =
            match optIndices with
            | [_; Some altLst] ->
                args |> List.item altLst |> ParseTree.childrenOf |> List.length |> (+) 1
            | [_; None] ->
                1
            | _ ->
                "alternations: Internal error" %> feedback.printError
                |> internalError __SOURCE_DIRECTORY__ __SOURCE_FILE__ __LINE__
        let expr =
            stack
            |> List.take altCount
            |> List.map (fun a ->
                match a with
                | Alt alt ->
                    alt
                | _ ->
                    printStackError feedback [a] "alternations: Invalid stack entry"
                    A([], None))
            |> List.rev
            |> E
            |> Expr
        expr :: (stack |> List.skip altCount)

    let rule (feedback: ParserFeedback) (stack: AST list) (_: ParseTree list): AST list =
        match stack with
        | Expr(e)::Factr(N(n))::Cmnts(c)::t ->
            Rl(Rule(c, n, e, PTOperation.Nop))::t
        | Expr(e)::Factr(N(n))::t ->
            Rl(Rule([], n, e, PTOperation.Nop))::t
        | _ ->
            "alternations: Invalid stack entry" %> printStackError feedback stack
            |> internalError __SOURCE_DIRECTORY__ __SOURCE_FILE__ __LINE__

    let grammar (feedback: ParserFeedback) (stack: AST list) (_: ParseTree list): AST list =
        // The stack possibly contains comments at the top.
        // We extract them by splitting the stack at the index of the first rule.
        let idxRules =
            stack
            |> List.findIndex (fun ast -> match ast with | Rl(_) -> true | _ -> false)
        let cmnts, rules =
            stack
            |> List.splitAt idxRules
        let rules =
            rules
            |> List.takeWhile (fun ast -> match ast with | Rl(_) -> true | _ -> false)
            |> List.map (fun ast ->
                match ast with
                | Rl(r) ->
                    r
                | _ ->
                    "grammar: Invalid stack entry" %> printStackError feedback [ast]
                    |> internalError __SOURCE_DIRECTORY__ __SOURCE_FILE__ __LINE__
            )
            |> List.rev
        let stackTail =
            stack
            |> List.skip (cmnts.Length + rules.Length)
        let endComments =
            match cmnts with
            | [] ->
                []
            | [Cmnts(c)] ->
                c
            | _ ->
                internalError __SOURCE_DIRECTORY__ __SOURCE_FILE__ __LINE__ "grammar: Stack not empty after parsing!"
        match stackTail with
        | (CmtDcl(c))::t ->
            Gr(LlkData(c, rules, endComments))::t
        | _ ->
            Gr(LlkData(CommentDecl.empty, rules, endComments))::stackTail

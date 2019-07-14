namespace BP

module AST =
    open LelekParser.Token
    open LelekParser.ParseTree
    open LelekParser.ParserFeedback
    open BPLexer

    type BoolBinOp = | AND | OR | XOR | NOR | NAND | XNOR

    type AST =
        | BOOL of bool
        | Err of string

    let mutable fileName = ""

    let printError (feedback: ParserFeedback) msg (ParseTree(Item = item) as pt) =
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

    let printStackError (feedback: ParserFeedback) msg (stack: AST list) =
        let scope = "Error stack handling"
        match stack with
        | [] ->
            feedback.printError (sprintf "%s: '%s' at %s" scope msg fileName)
        | h::_ ->
            feedback.printError (sprintf "%s: '%s' in %s TOS: %A" scope msg fileName h)



    let init fn =
        fileName <- fn |> System.IO.Path.GetFullPath


    // -------------------------------------------------------------------------
    // User actions
    // -------------------------------------------------------------------------
    let boolean (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let tt, _ = ParseTree.typeAndTextOfToken args.[0]
        match enum tt with
        | TokenType.TkBoolean -> BOOL(true)
        | TokenType.TkBoolean1 -> BOOL(false)
        | _  ->
            printError feedback "number: Invalid token type" args.[0]
            Err("number: Invalid token type")
        :: stack

    let term (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        if args.Length = 2 then
            // we have a NOT here
            match stack with
            | BOOL(b)::s ->
                BOOL(not b)::s
            | _ ->
                printStackError feedback "term: unexpected user stack" stack
                Err("term: unexpected user stack") :: stack
        else
            stack

    let binOpList (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let rec applyBinOp stack items =
            match items with
            | pt::s ->
                let newStack =
                    match pt with
                    | ParseTree(Children = c) ->
                        if c.Length <> 2 then
                            printError feedback "binOpList: unexpected number of children in binary operation!" pt
                            Err("binOpList: unexpected number of children in binary operation!") ::stack
                        else
                            let tt, _ = ParseTree.typeAndTextOfToken c.[0]
                            match enum tt with
                            | TokenType.TkAndOp ->
                                match stack with
                                | BOOL(l)::BOOL(r)::s ->
                                    BOOL(l && r)::s
                                | _ ->
                                    Err("binOpList: !") ::stack
                            | TokenType.TkOrOp ->
                                match stack with
                                | BOOL(l)::BOOL(r)::s ->
                                    BOOL(l || r)::s
                                | _ ->
                                    Err("binOpList: !") ::stack
                            | TokenType.TkXorOp ->
                                match stack with
                                | BOOL(l)::BOOL(r)::s ->
                                    BOOL((l && not r) || (not l && r))::s
                                | _ ->
                                    Err("binOpList: !") ::stack
                            | TokenType.TkNorOp ->
                                match stack with
                                | BOOL(l)::BOOL(r)::s ->
                                    BOOL(not (l || r))::s
                                | _ ->
                                    Err("binOpList: !") ::stack
                            | TokenType.TkNandOp ->
                                match stack with
                                | BOOL(l)::BOOL(r)::s ->
                                    BOOL(not (l && r))::s
                                | _ ->
                                    Err("binOpList: !") ::stack
                            | TokenType.TkXnorOp ->
                                match stack with
                                | BOOL(l)::BOOL(r)::s ->
                                    BOOL((not l && not r) || (l && r))::s
                                | _ ->
                                    Err("binOpList: !") ::stack
                            | _ -> 
                                Err("binOpList: !") ::stack
                applyBinOp newStack s
            | _ ->
                stack
                    
        match args with
        | h::_ ->
            h |> ParseTree.childrenOf
            |> applyBinOp stack
        | _ -> stack

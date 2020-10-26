namespace LelekParser

module LlkParser =

    open FParsec
    open LlkGrammar

    type UserState = unit
    type Parser<'t> = Parser<'t, UserState>

    let private BP (p: Parser<_,_>) stream =
        p stream // set a breakpoint here (for parserX insert (BP parserX))

    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply
    
    let private ignoredComment : Parser<unit, unit> =
        ((pstring "(*") >>. (skipManyTill anyChar (pstring "*)")))
        <|>
        ((regex "//.*?(\r\n|\r|\n)") |>> ignore)

    let private ws = skipSepBy spaces ignoredComment
    
    let private spaceStr : Parser<string, unit> =
        many1 (anyOf "\t\r\n ") >>= (Array.ofList >> System.String >> preturn) //<!> "Space string"
    
    let private blockcomment : Parser<string, unit> =
        (pstring "(*") >>. (manyTill anyChar (pstring "*)")) <?> "Comment content" //<!> "Comment content"
            >>= (fun c -> "(*" + System.String(c |> Array.ofList) + "*)" |> preturn)
    
    let private linecomment : Parser<string, unit> =
        (regex "//.*?(\r\n|\r|\n)") <?> "Comment content" //<!> "Comment content"
            >>= preturn
    
    let private comments : Parser<string list, unit> =
        (many (linecomment <|> blockcomment <|> spaceStr)) <?> "Comments or Whitespaces"

    let private strws s = pstring s .>> ws

    (*
    // This is a subset of EBNF!
    Group           = '(' Expression ')'.
    Repeat          = '{' Expression '}'.
    Optional        = '[' Expression ']'.
    Factor          = Group
                    |   Repeat
                    |   Optional
                    |   NonTerminal
                    |   Terminal.
    Action          = '@' Fun
    Fun             = Indentifier {'.' Indentifier}.
    Sequence        = {Factor} [Action] ';'
    Alternations    = Sequence {'|' Alternations}.
    Expression      = Alternations.
    NonTerminal     = Indentifier.
    Rule            = NonTerminal '=' Expression ';'.
    Rules           = '%grammar' Rule {Rule}.
    LineComment     = '%comment' '"'Start'"'
    BlockComment    = '%comment' '"'Start'"' '"'End'"'
    CommentDecl     = [LineComment] [BlockComment]
    Grammar         = CommentDecl Rules.
    *)


    // -----------------------------------------------------------------------------
    // Forward declaration of Expression parser
    // -----------------------------------------------------------------------------
    let (pExpression : Parser<Expression, unit>) , pExpressionRef = createParserForwardedToRef()

    // -----------------------------------------------------------------------------
    // Identifier parser
    // -----------------------------------------------------------------------------
    let isAsciiIdStart c =
        isAsciiLetter c || c = '_'

    let isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_'

    let pIdentifier : Parser<string, unit> =
        identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart,
                                      isAsciiIdContinue = isAsciiIdContinue))

       
    // -----------------------------------------------------------------------------
    // Terminal (String) parser
    // -----------------------------------------------------------------------------
    let pTerminal : Parser<LlkTerminal, unit> =
        let normalCharSnippetTe = satisfy (fun c -> c <> '\\' && c <> '"') >>= (string >> preturn)
        let unescape c = match c with
                            | 'n' -> "\n"
                            | 'r' -> "\r"
                            | 't' -> "\t"
                            | '"' -> "\\\""
                            | c   -> "\\" + (string c)
        let escapedChar = pstring "\\" >>. (anyChar |>> unescape)
        ((between (pstring "\"") (pstring "\"")
                (manyStrings (normalCharSnippetTe <|> escapedChar))) .>> ws  (* <?> "Terminal" <!> "Terminal" *) 
                    >>= (LlkTerminal >> preturn))


    // -----------------------------------------------------------------------------
    // CommentDecl parsers
    // -----------------------------------------------------------------------------
    let pCommentDecl : Parser<CommentDecl, unit> =
        let pString : Parser<string> =
            (between (pstring "\"") (pstring "\"") (manySatisfy (fun c -> c <> '"'))) >>= (string >> preturn)
        let pLineComment: Parser<string> =
            strws "%comment" >>. pString
        let pBlockComment: Parser<string * string> =
            strws "%comment" >>. pString .>> ws .>>. pString
        comments .>>. (opt pLineComment) .>>. (opt (ws >>. pBlockComment))
            >>= (fun ((c, l), b) -> CommentDecl(c, l, b) |> preturn)


    // -----------------------------------------------------------------------------
    // NonTerminal parsers
    // -----------------------------------------------------------------------------
    let pNonTerminal : Parser<LlkVariable, unit>  =
        pIdentifier .>> ws >>= (fun s -> preturn (s.ToLower() |> LlkVariable))

    // -----------------------------------------------------------------------------
    // Action parser
    // -----------------------------------------------------------------------------
    let pAction: Parser<string, unit>  =
        (pstring "@") >>. pIdentifier .>>. (many (pstring "." .>>. pIdentifier)) .>> ws (* <?> "Action content" <!> "Action content" *)
            >>= (fun (i, il) -> il |> List.fold (fun acc (d,n) -> acc + d + n) i |> preturn)

    // -----------------------------------------------------------------------------
    // Group parser
    // -----------------------------------------------------------------------------
    let pGroup : Parser<Group, unit> =
        pchar '(' >>. ws >>. pExpression .>> pchar ')' .>> ws >>= (Group >> preturn)

    // -----------------------------------------------------------------------------
    // Optional parser
    // -----------------------------------------------------------------------------
    let pOptional : Parser<Optional, unit> =
        pchar '[' >>. ws >>. pExpression .>> pchar ']' .>> ws >>= (Optional >> preturn)

    // -----------------------------------------------------------------------------
    // Repeat parser
    // -----------------------------------------------------------------------------
    let pRepeat : Parser<Repeat, unit> =
        pchar '{' >>. ws >>. pExpression .>> pchar '}' .>> ws >>= (Repeat >> preturn)

    // -----------------------------------------------------------------------------
    // Factor parser
    // -----------------------------------------------------------------------------
    let pFactor : Parser<Factor, unit> =
        choice [
            pGroup >>= (G >> preturn)
            pRepeat >>= (R >> preturn)
            pOptional >>= (O >> preturn)
            pNonTerminal >>= (N >> preturn)
            pTerminal >>= (T >> preturn)] (* <?> "Factor" <!> "Factor" *)  .>> ws

    // -----------------------------------------------------------------------------
    // Sequence parser
    // -----------------------------------------------------------------------------
    let rec pSequence : Parser<Alternation, unit> =
        (many (pFactor .>> ws) <?> "Factor list") .>>. (opt pAction) (* <?> "Alternation" <!> "Alternation" *) >>= (fun (s, a) ->
            match  s with
            | [] -> preturn (A([T LlkEpsilon], a))
            | _ -> preturn (A(s, a)))

    // -----------------------------------------------------------------------------
    // Alternation parser
    // -----------------------------------------------------------------------------
    let pAlternation : Parser<Expression> =
        sepBy1 pSequence ((pchar '|') .>> ws) (* <?> "Expression" <!> "Expression" *) >>= (E >> preturn)

    // -----------------------------------------------------------------------------
    // Expression
    // -----------------------------------------------------------------------------
    pExpressionRef :=
        pAlternation

    // -----------------------------------------------------------------------------
    // Rule start
    // -----------------------------------------------------------------------------
    let pRuleStart =
        pNonTerminal .>> ws

    // -----------------------------------------------------------------------------
    // Rule
    // -----------------------------------------------------------------------------
    let pRule : Parser<Rule, unit> =
        comments .>>. pRuleStart .>> (strws "=") .>>. pExpression .>> ws .>> pchar ';'
            >>= (fun ((c, n), e) -> preturn (Rule (c, n, e, Nop)))

    // -----------------------------------------------------------------------------
    // Rules
    // -----------------------------------------------------------------------------
    let pRules : Parser<Rule list, unit>  = strws "%grammar" >>. (many1 (attempt pRule))

    // -----------------------------------------------------------------------------
    // Grammar
    // -----------------------------------------------------------------------------
    let pGrammar : Parser<LlkData> =
        pCommentDecl .>>. pRules .>>. comments .>> eof
            >>= (fun ((d, r), c) -> (LlkData(CommentDcl = d, Rules = r, CommentRest = c)) |> preturn)
    
    // -----------------------------------------------------------------------------
    // Helper
    // -----------------------------------------------------------------------------
    let parseLlkFile path : Result<LlkData, string> =
        let r = runParserOnFile pGrammar () path System.Text.Encoding.UTF8
        match r with
        | Success (s, _, _) -> Result.Ok s
        | Failure (s, e, _) -> Result.Error (sprintf "Failed to parse: %s: %A" s e)

    let parseLlkString ebnf : Result<LlkData, string> =
        let r = runParserOnString pGrammar () "Input" ebnf
        match r with
        | Success (s, _, _) -> Result.Ok s
        | Failure (s, e, _) -> Result.Error (sprintf "Failed to parse: %s: %A" s e)

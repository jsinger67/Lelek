namespace LelekParser

module LexerTerminals =
    open Utils
    open LlkGrammar
    open LinLlkGrammar

    let generateTokenName (n: string) =
        n
        |> String.collect (fun c ->
            match c with
            | c when System.Char.IsLetterOrDigit(c) -> c |> string
            | '\\' -> "Esc" 
            | '|' -> "Or"
            | '(' -> "LParen"
            | ')' -> "RParen"
            | '[' -> "LBracket"
            | ']' -> "RBracket"
            | '{' -> "LBrace"
            | '}' -> "RBrace"
            | '+' -> "Plus"
            | '-' -> "Minus"
            | '*' -> "Star"
            | '/' -> "Slash"
            | '=' -> "Equ"
            | '!' -> "Bang"
            | '.' -> "Dot"
            | '~' -> "Tilde"
            | '$' -> "Dollar"
            | '%' -> "Percent"
            | '<' -> "LT"
            | '>' -> "GT"
            | '?' -> "Quest"
            | '@' -> "At"
            | ':' -> "Colon"
            | ';' -> "Semicolon"
            | '^' -> "Circumflex"
            | '_' -> "Underscore"
            | '&' -> "Amp"
            | '§' -> "Para"
            | '\'' -> "Tick"
            | '`' -> "Backtick"
            | '´' -> "Accent"
            | ',' -> "Comma"
            | '#' -> "Hash"
            | _ -> "_"
        )

    [<Literal>]
    let private NEWLINE_TOKEN = @"\r\n|\r|\n"
    [<Literal>]
    let private WHITESPACE_TOKEN = @"\s"
    [<Literal>]
    let private RESTOFLINE_TOKEN = @".*?(\r\n|\r|\n|$)"
    [<Literal>]
    let private BLOCKCOMMENTCONTENT_TOKEN = @"(.|\r\n|\r|\n)*?"

    let terminals g =
        LinLlkData.terminals g

    let augmentedTerminals (g: LinLlkData): string list =
        let (LinLlkData(cm, _, _)) = g
        // Add newline rule at index 0!
        // Add spaces rule at index 1!
        // Append Error rule at the end!
        let lineCmnt = CommentDecl.lineComment cm
        let linRx = lineCmnt + RESTOFLINE_TOKEN
        let blockStr = CommentDecl.blockCommentStart cm
        let blockEnd = CommentDecl.blockCommentEnd cm
        let blkRx = blockStr + BLOCKCOMMENTCONTENT_TOKEN + blockEnd
        match CommentDecl.hasLineComment cm, CommentDecl.hasBlockComment cm with
        | false, false ->
            (NEWLINE_TOKEN :: WHITESPACE_TOKEN :: terminals g) @ ["."]
        | false, true ->
            (NEWLINE_TOKEN :: WHITESPACE_TOKEN :: blkRx :: terminals g) @ ["."]
        | true, false ->
            (NEWLINE_TOKEN :: WHITESPACE_TOKEN :: linRx :: terminals g) @ ["."]
        | true, true ->
            (NEWLINE_TOKEN :: WHITESPACE_TOKEN :: linRx :: blkRx :: terminals g) @ ["."]

    let nameToIndex (g: LinLlkData): (string -> int) =
        let terminals = augmentedTerminals g
        (fun name -> terminals |> List.findIndex (fun n -> n = name))

    let indexToName (g: LinLlkData): (int -> string) =
        let terminals = augmentedTerminals g
        (fun idx -> terminals.[idx])

    let terminalTypNames (g: LinLlkData) =
        let terminals = augmentedTerminals g
        let (LinLlkData(cm, rl, _)) = g
        let capitalize (s: string) =
            s.Split([| '_' |], System.StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map (fun p ->
                (System.Char.ToUpper(p.[0]) |> string) + (p.Substring(1)))
            |> String.concat ""

        let hasLineComment = CommentDecl.hasLineComment cm
        let hasBlockComment = CommentDecl.hasBlockComment cm

        terminals
        |> List.mapi (fun i t ->
            match i with
            | 0 -> Some "NewLine", t
            | 1 -> Some "Whitespace", t
            | 2 when hasLineComment -> Some "LineComment", t
            | 2 when (not hasLineComment) && hasBlockComment -> Some "BlockComment", t
            | 3 when (hasLineComment) && hasBlockComment -> Some "BlockComment", t
            | i when i = terminals.Length - 1 -> Some "Error", t
            | _ ->
                (rl
                |> List.tryFind (fun (LinLlkRule(Prod = prod)) ->
                    prod.Length = 1 && prod.Head = LinLlkTerminal t)
                |> Option.map (LinLlkRule.ruleName)), t)
        |> List.map (fun (so, t) ->
            "Tk" +
            capitalize(
                match so with
                | Some s -> s
                | None -> generateTokenName t)
        )
        |> List.fold (fun acc elem ->
            if acc |> List.contains elem then
                acc @ [generateName acc elem]
            else
                acc @ [elem]
        ) List.empty

    let indexToTokenTypeName (g: LinLlkData): (int -> string) =
        let tokenTypeNames = terminalTypNames g
        (fun idx ->
            if idx = System.Int32.MaxValue then
                "$"
            else
                tokenTypeNames.[idx])



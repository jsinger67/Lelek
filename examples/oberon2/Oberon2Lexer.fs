// ---------------------------------------------------------
// This file was generated by Lelek.
// Changes may be lost after next build.
// ---------------------------------------------------------

namespace Oberon2

module Oberon2Lexer =
    open LelekParser
    open Token
    open System.Text.RegularExpressions

    let augmentedTerminals : string list =
        [
            "\r\n|\r|\n"
            "\s"
            "\(\*(.|\r\n|\r|\n)*?\*\)"
            "MODULE"
            ";"
            "END"
            "\."
            "BEGIN"
            "IMPORT"
            ":="
            ","
            "CONST"
            "TYPE"
            "VAR"
            "="
            ":"
            "PROCEDURE"
            "\^"
            "\("
            "\)"
            "ARRAY"
            "OF"
            "RECORD"
            "POINTER"
            "TO"
            "IF"
            "THEN"
            "CASE"
            "WHILE"
            "DO"
            "REPEAT"
            "UNTIL"
            "FOR"
            "LOOP"
            "WITH"
            "EXIT"
            "RETURN"
            "ELSE"
            "ELSIF"
            "\|"
            "BY"
            "\.\."
            "-"
            "\+"
            "NIL"
            "~"
            "\{"
            "\}"
            "#"
            "<"
            "<="
            ">"
            ">="
            "IS"
            "OR"
            "\*"
            "/"
            "DIV"
            "MOD"
            "&"
            "\["
            "\]"
            "^"
            "[a-zA-Z_]\w*"
            "IN"
            "[0-9][0-9]*\.[0-9]*(ED[+-]?[0-9][0-9]*)?"
            "[0-9][0-9A-F]X"
            "[0-9][0-9]*|[0-9][0-9A-F]H"
            "\"[^\"]*\"|'[^']*'"
            "."
        ]
    
    type TokenType = 
        | TkNewLine                =   0 // "\r\n|\r|\n"
        | TkWhitespace             =   1 // "\s"
        | TkBlockComment           =   2 // "\(\*(.|\r\n|\r|\n)*?\*\)"
        | TkMODULE                 =   3 // "MODULE"
        | TkSemicolon              =   4 // ";"
        | TkEND                    =   5 // "END"
        | TkEscDot                 =   6 // "\."
        | TkBEGIN                  =   7 // "BEGIN"
        | TkIMPORT                 =   8 // "IMPORT"
        | TkColonEqu               =   9 // ":="
        | TkComma                  =  10 // ","
        | TkCONST                  =  11 // "CONST"
        | TkTYPE                   =  12 // "TYPE"
        | TkFPSectionOpt1          =  13 // "VAR"
        | TkRelation               =  14 // "="
        | TkColon                  =  15 // ":"
        | TkPROCEDURE              =  16 // "PROCEDURE"
        | TkEscCircumflex          =  17 // "\^"
        | TkEscLParen              =  18 // "\("
        | TkEscRParen              =  19 // "\)"
        | TkARRAY                  =  20 // "ARRAY"
        | TkOF                     =  21 // "OF"
        | TkRECORD                 =  22 // "RECORD"
        | TkPOINTER                =  23 // "POINTER"
        | TkTO                     =  24 // "TO"
        | TkIF                     =  25 // "IF"
        | TkTHEN                   =  26 // "THEN"
        | TkCASE                   =  27 // "CASE"
        | TkWHILE                  =  28 // "WHILE"
        | TkDO                     =  29 // "DO"
        | TkREPEAT                 =  30 // "REPEAT"
        | TkUNTIL                  =  31 // "UNTIL"
        | TkFOR                    =  32 // "FOR"
        | TkLOOP                   =  33 // "LOOP"
        | TkWITH                   =  34 // "WITH"
        | TkStatementOpt1          =  35 // "EXIT"
        | TkRETURN                 =  36 // "RETURN"
        | TkELSE                   =  37 // "ELSE"
        | TkELSIF                  =  38 // "ELSIF"
        | TkEscOr                  =  39 // "\|"
        | TkBY                     =  40 // "BY"
        | TkEscDotEscDot           =  41 // "\.\."
        | TkMinus                  =  42 // "-"
        | TkAddOpPlus              =  43 // "\+"
        | TkFactor                 =  44 // "NIL"
        | TkTilde                  =  45 // "~"
        | TkEscLBrace              =  46 // "\{"
        | TkEscRBrace              =  47 // "\}"
        | TkRelation1              =  48 // "#"
        | TkRelation2              =  49 // "<"
        | TkRelation3              =  50 // "<="
        | TkRelation4              =  51 // ">"
        | TkRelation5              =  52 // ">="
        | TkRelation6              =  53 // "IS"
        | TkAddOp                  =  54 // "OR"
        | TkMulOp                  =  55 // "\*"
        | TkMulOp1                 =  56 // "/"
        | TkMulOp2                 =  57 // "DIV"
        | TkMulOp3                 =  58 // "MOD"
        | TkMulOp4                 =  59 // "&"
        | TkEscLBracket            =  60 // "\["
        | TkEscRBracket            =  61 // "\]"
        | TkDesignatorLst1Itm1     =  62 // "^"
        | TkIdent                  =  63 // "[a-zA-Z_]\w*"
        | TkIn                     =  64 // "IN"
        | TkReal                   =  65 // "[0-9][0-9]*\.[0-9]*(ED[+-]?[0-9][0-9]*)?"
        | TkCharacter              =  66 // "[0-9][0-9A-F]X"
        | TkInteger                =  67 // "[0-9][0-9]*|[0-9][0-9A-F]H"
        | TkString                 =  68 // "\"[^\"]*\"|'[^']*'"
        | TkError                  =  69 // "."

    let indexToName idx =
            if idx = System.Int32.MaxValue then
                "$"
            else
                augmentedTerminals.[idx]

    let tokenTypeToString tt =
        tt |> enum<TokenType> |> sprintf "%O"

    let createLexer (k: int): (string -> Token seq) =
        let rxStr = @"(?<G0>\r\n|\r|\n)|(?<G1>\s)|(?<G2>\(\*(.|\r\n|\r|\n)*?\*\))|(?<G3>MODULE)|(?<G4>;)|(?<G5>END)|(?<G6>\.)|(?<G7>BEGIN)|(?<G8>IMPORT)|(?<G9>:=)|(?<G10>,)|(?<G11>CONST)|(?<G12>TYPE)|(?<G13>VAR)|(?<G14>=)|(?<G15>:)|(?<G16>PROCEDURE)|(?<G17>\^)|(?<G18>\()|(?<G19>\))|(?<G20>ARRAY)|(?<G21>OF)|(?<G22>RECORD)|(?<G23>POINTER)|(?<G24>TO)|(?<G25>IF)|(?<G26>THEN)|(?<G27>CASE)|(?<G28>WHILE)|(?<G29>DO)|(?<G30>REPEAT)|(?<G31>UNTIL)|(?<G32>FOR)|(?<G33>LOOP)|(?<G34>WITH)|(?<G35>EXIT)|(?<G36>RETURN)|(?<G37>ELSE)|(?<G38>ELSIF)|(?<G39>\|)|(?<G40>BY)|(?<G41>\.\.)|(?<G42>-)|(?<G43>\+)|(?<G44>NIL)|(?<G45>~)|(?<G46>\{)|(?<G47>\})|(?<G48>#)|(?<G49><)|(?<G50><=)|(?<G51>>)|(?<G52>>=)|(?<G53>IS)|(?<G54>OR)|(?<G55>\*)|(?<G56>/)|(?<G57>DIV)|(?<G58>MOD)|(?<G59>&)|(?<G60>\[)|(?<G61>\])|(?<G62>^)|(?<G63>[a-zA-Z_]\w*)|(?<G64>IN)|(?<G65>[0-9][0-9]*\.[0-9]*(ED[+-]?[0-9][0-9]*)?)|(?<G66>[0-9][0-9A-F]X)|(?<G67>[0-9][0-9]*|[0-9][0-9A-F]H)|(?<G68>\""[^\""]*\""|'[^']*')|(?<G69>.)"

        let hasLineComment = false

        let hasBlockComment = true

        let lexerRx = Regex(rxStr, RegexOptions.Compiled ||| RegexOptions.ExplicitCapture)

        let isNewLine (mat: Match) : bool =
            mat.Groups.["G0"].Success

        let isSpace (mat: Match) : bool =
            mat.Groups.["G1"].Success

        let isLineComment (mat: Match) : bool =
            hasLineComment && mat.Groups.["G2"].Success

        let isBlockComment (mat: Match) : bool =
            ((not hasLineComment) && hasBlockComment && mat.Groups.["G2"].Success) ||
            (hasLineComment && hasBlockComment && mat.Groups.["G3"].Success)

        let tokenFromMatch (mat: Match) (line: int) (nlPos: int) : Token =
            let tt =
                mat.Groups
                |> Seq.tryFind (fun g -> g.Success && g.Name.StartsWith("G"))       // Don't consider sub groups.
                |> Option.map (fun g -> g.Name.Substring(1) |> System.Int32.Parse)  // Extract the index from the group name!
                |> Option.defaultValue -1
            Token(Symbol = mat.Value,
                    TokenType = tt,
                    Location = TokenLocation(Line = line,
                                            Column = mat.Index - nlPos - 1,
                                            Length = mat.Length))

        let rxNl = Regex(@"\r\n|\r|\n", RegexOptions.Compiled, System.TimeSpan(0, 0, 5))

        let lineCountAndIndexOfLastNewLine (mat: Match) =
            let matches = rxNl.Matches(mat.Value)
            if matches.Count = 0 then
                0, mat.Index
            else
                matches.Count, mat.Index + (matches |> Seq.rev |> Seq.head).Index

        (fun input ->
            seq {
                let mutable mat = lexerRx.Match(input)
                let mutable line = 1
                let mutable nlPos = -1
                while mat.Success do
                    if isNewLine mat then
                        line <- line + 1
                        nlPos <- mat.Index
                    else
                        if isLineComment mat || isBlockComment mat then
                            let lines, nlIndex = lineCountAndIndexOfLastNewLine mat
                            line <- line + lines
                            nlPos <- nlIndex
                        else
                            if isSpace mat |> not then
                                yield tokenFromMatch mat line nlPos
                    mat <- mat.NextMatch()

                // Append k + 1 End tokens
                for _ in [0..k] do
                    yield EndOfInput
            }
        )

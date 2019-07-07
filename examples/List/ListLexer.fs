namespace SrcNamespace

module LexerModule =
    open LelekParser
    open Token
    open System.Text.RegularExpressions

    let augmentedTerminals : string list =
        [
            "\r\n|\r|\n"
            "\s"
            ","
            "\d+"
            "."
        ]
    
    type TokenType = 
        | TkNewLine        =   0 // "\r\n|\r|\n"
        | TkWhitespace     =   1 // "\s"
        | TkComma          =   2 // ","
        | TkNumber         =   3 // "\d+"
        | TkError          =   4 // "."

    let indexToName idx =
            if idx = System.Int32.MaxValue then
                "$"
            else
                augmentedTerminals.[idx]

    let tokenTypeToString tt =
        tt |> enum<TokenType> |> sprintf "%O"

    let createLexer (k: int): (string -> Token seq) =
        let rxStr = @"(?<G0>\r\n|\r|\n)|(?<G1>\s)|(?<G2>,)|(?<G3>\d+)|(?<G4>.)"

        let hasLineComment = false

        let hasBlockComment = false

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

        let lineCountAndIndexOfLastNewLine (mat: Match) =
            let rxNl = Regex(@"\r\n|\r|\n", RegexOptions.Compiled, System.TimeSpan(0, 0, 5))
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

                // Append k End tokens!
                for _ in [0..k] do
                    yield EndOfInput
            }
        )

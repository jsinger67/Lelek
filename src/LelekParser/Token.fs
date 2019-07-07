namespace LelekParser

module Token =

    [<Literal>]
    let EndOfInputToken = System.Int32.MaxValue

    type TokenLocation =
        | TokenLocation of Line: int * Column: int * Length: int

    module TokenLocation =
        let empty = TokenLocation(Line = 0, Column = 0, Length = 0)

        let toString (TokenLocation(Line = li; Column = cl; Length = le)) =
            sprintf "(%d,%d) Len:%d" li cl le

        let toShortString (TokenLocation(Line = li; Column = cl)) =
            sprintf "(%d,%d)" li cl

    type Token =
        | Token of
            // The matched string
            Symbol: string *
            // The index of the terminal in the terminal list
            TokenType: int *
            // The location in the input string
            Location: TokenLocation
        | EndOfInput

    module Token =
        let typeOf = function
            | Token(TokenType = tt) -> tt
            | EndOfInput -> EndOfInputToken

        let textOf = function
            | Token(Symbol = sym) -> sym
            | EndOfInput -> "<EOF>"

        let toString = function
            | Token(Symbol = sym; TokenType = tt; Location = loc) -> 
                sprintf "%s: '%s' (%d)" (TokenLocation.toShortString loc) sym tt
            | EndOfInput -> "<EOF>"

        let toShortString = function
            | Token(Symbol = sym) -> sym
            | EndOfInput -> "<EOF>"

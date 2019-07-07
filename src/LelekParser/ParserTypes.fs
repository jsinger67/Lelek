namespace LelekParser

// -------------------------------------------------------------------------------------------------
// Data for the parser conatins
//
// * A RULE ARRAY where the index within the array is the number of the rule
//   Each rule mainly contains its production (right hand side) as an array of symbols
//   A symbol is an integer where
//     negative ones (incl. 0) are non-terminals (i.e. negated indexes into the rule array)
//   or
//     posistive ones are terminals (token 0 is only internally used as TkNewLine)
//   The rule array's first entry (index 0) is the start rule.
//   The rules are sorted so that rules of the same non-terminal are grouped together.
//   Additionally each rule contains its rule name for diagnosis purposis,
//   a ParseTreeOperation, a UserAction and a lookahead automaton
//
//   0  | StartRule
//   1  | RuleA (Base index is 1)
//   2  | RuleA
//   ...
//   n  | RuleX (Base index is n)
//   n+1| RuleX
//
//   The rule base index is the index of the first rule belonging to a non-terminal within the
//   rule array.
//   The lookahead automaton calculates the offset of the actual rule to be parsed according to
//   the lookahead tokens
// -------------------------------------------------------------------------------------------------

module ParserTypes =
    open CompiledLADfa
    open ParserFeedback
    open ParseTree

    // We mark terminals by using positive values (without 0) - it's the token type
    // Negative values (incl. 0) are negated and used as indices of non-terminals within the rule array.
    type ParserSymbol = int

    // A parse tree operation is a transformation the parser applies automatically on the
    // intermediate parse tree in order to get a handier structure.
    // They are inserted during the parser generation when the structure of the grammar
    // allows to infer the kind of transformation.
    type ParseTreeOperation = ParseTree list -> ParseTree

    // Besides the parse tree the parser provides a stack of user defined elements which
    // usually constitute the AST (abstract syntax tree).
    // A user action can maintain this AST stack according to the current stack's content
    // and the given parse tree list.
    // Both are provided as parameters to the user action.
    // The user action should return the new stack.
    // A default action is inserted by the parser generator that simply ignores the parse
    // tree list (i.e the third parameter) and returns the stack (the second parameter)
    // The very first parameter allows the user action to output errors via the ParserFeedback
    // interface.
    type UserAction<'a> = ParserFeedback -> 'a list -> ParseTree list -> 'a list

    // The type that contains all data to process a rule within the parser.
    type ParserRule<'a> =
        | ParserRule of
            Name: string *
            Production: ParserSymbol array *
            PTOp: ParseTreeOperation *
            Action: UserAction<'a> *
            LookaheadDFA: CompiledDfa

    // The type of the elements in the parser stack.
    type ParseType<'a> =
        | Symbol of ParserSymbol
        | PTOp of Operation: ParseTreeOperation * RuleLength: int
        | Action of UserAction<'a>



module CLIArguments

open Argu

type CLIArguments =
    | [<Mandatory; Unique>] File of string
    | [<Unique>] Find_Prefixes
    | [<Unique>] Find_LL1Conflicts
    | [<Unique>] Save_Linear_Grammar of string option
    | [<Unique>] MaxK of int
    | [<Unique>] TraceMode of bool
    | [<Unique>] Generate_LexerSrc of string
    | [<Unique>] Generate_ParserSrc of string
    | [<Unique>] SrcNameSpaceName of string
    | [<Unique>] LexerModuleName of string
    | [<Unique>] ParserModuleName of string
    | [<Unique>] ASTType of string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _                -> "specifiy an imput EBNF file."
            | Find_Prefixes         -> "test for left prefixes in the grammar."
            | Find_LL1Conflicts     -> "detects rules with LL(1) conflicts."
            | Save_Linear_Grammar _ -> "save processed grammar to file. If path is ommitted the file name is deduced from input file name."
            | MaxK _                -> "Maximum number of lookahead tokens, defaults to 5"
            | TraceMode _           -> "Trace details during parse process"
            | Generate_LexerSrc _   -> "output file for lexer source"
            | Generate_ParserSrc _  -> "output file for parser source"
            | SrcNameSpaceName _    -> @"Namespace name of generated lexer and parser, defaults to ""SrcNamespace"""
            | LexerModuleName _     -> @"Module name of generated lexer, defaults to ""LexerModule"""
            | ParserModuleName _    -> @"Module name of generated parser, defaults to ""ParserModule"""
            | ASTType _             -> @"Name of the user AST type"

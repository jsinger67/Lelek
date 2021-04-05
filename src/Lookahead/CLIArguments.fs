module CLIArguments

open Argu

type CLIArguments =
    | [<Mandatory; Unique>] File of string
    | [<Unique>] MaxK of int
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _                -> "specifiy an imput EBNF file."
            | MaxK _                -> "Maximum number of lookahead tokens, defaults to 5"

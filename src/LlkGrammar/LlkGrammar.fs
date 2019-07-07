namespace LelekParser

/// The 'LLK' Grammer which is a specialized form of EBNF.
module LlkGrammar =

    type LlkTerminal =
        /// Represents an empty string (an empty alternative) in Llk
        | LlkEpsilon
        /// Represents the end of file token
        | LlkTerminal of string

    type LlkVariable =
        /// The string represents the name of the nonterminal
        | LlkVariable of string

    // This reflects the 'Grouping' Llk semantics
    type Group =
        | Group of Expression

    // This reflects the 'Optional' Llk semantics
    and Optional =
        | Optional of Expression

    // This reflects the 'Star' Llk semantics
    and Repeat =
        | Repeat of Expression

    and Factor =
        | G of Group        // ()
        | R of Repeat       // {}
        | O of Optional     // []
        | N of LlkVariable  // id
        | T of LlkTerminal  // "<regex>"

    and Alternation =
        | A of Factors: Factor list * Action: string option  // Concatenation with action

    and Expression =
        | E of Alternation list     // Alternations (|)

    type PTOperation =
        | Nop
        | Clip
        | Collect
        | Lift

    type Rule =
        | Rule of Comments: (string list) * LHS: LlkVariable * RHS: Expression * PTOp: PTOperation

    type CommentDecl =
        | CommentDecl of Comments: (string list) * LineComment: string option * Block: (string * string) option

    module CommentDecl =
        let empty = CommentDecl([], None, None)

        let toString (CommentDecl(c, l, b)): string =
            (c
            |> String.concat "")
            +
            (l |> Option.map (sprintf "%%comment \"%s\"\n") |> Option.defaultValue "")
            +
            (b |> Option.map (fun (s, e) -> sprintf "%%comment \"%s\" \"%s\"\n" s e) |> Option.defaultValue "")

        let hasLineComment (CommentDecl(_, l, _)): bool = l.IsSome

        let lineComment (CommentDecl(_, l, _)): string =
            l |> Option.defaultValue ""

        let hasBlockComment (CommentDecl(_, _, b)): bool = b.IsSome

        let blockCommentStart (CommentDecl(_, _, b)): string =
            b |> Option.map (fun (s, _) -> s) |> Option.defaultValue ""

        let blockCommentEnd (CommentDecl(_, _, b)): string =
            b |> Option.map (fun (_, e) -> e) |> Option.defaultValue ""

    // The initially parsed grammer
    type LlkData =
        | LlkData of CommentDcl: CommentDecl * Rules: Rule list * CommentRest: string list


    module Factor =
        /// -------------------------------------------------------------------------
        /// Predicates to match a certain factor kind.
        let isRep = function
            | R _   -> true
            | _     -> false

        let isOpt = function
            | O _   -> true
            | _     -> false

        let isGrp = function
            | G _   -> true
            | _     -> false

        let isVar = function
            | N _   -> true
            | _     -> false

        let isTrm = function
            | T _   -> true
            | _     -> false

        /// -------------------------------------------------------------------------
        /// Extracts the name of a variable factor.
        let varName = function
            | N (LlkVariable s) -> s
            | _ -> failwith "Not a variable!"

        /// -------------------------------------------------------------------------
        /// Construction of a variable factor.
        let mkVar vName =
            N (LlkVariable vName)


    module LlkVariable =
        /// -------------------------------------------------------------------------
        /// Extracts the name of a variable.
        let varName = function
            | LlkVariable s -> s

        /// -------------------------------------------------------------------------
        /// Construction of a variable.
        let mkVar vName =
            LlkVariable vName


    module LlkData =

        /// -------------------------------------------------------------------------
        /// Collects all variable names of a given Llk grammar.
        let variableNames (g: LlkData) : string list =
            let (LlkData(_, rl, _)) = g
            rl
            |> List.fold (fun acc (Rule(RHS = rhs)) ->
                let (E alts) = rhs
                alts
                |> List.fold (fun acc alt ->
                    let (A(factors, _)) = alt
                    factors
                    |> List.fold (fun acc factor ->
                        if Factor.isVar factor then
                            let vn = Factor.varName factor
                            vn :: acc
                        else
                            acc
                    ) acc
                ) acc
            ) List.empty
            |> List.distinct


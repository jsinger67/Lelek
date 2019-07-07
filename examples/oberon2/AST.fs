namespace Oberon2

module AST =
    open LelekParser.Token
    open LelekParser.ParseTree
    open LelekParser.ParserFeedback
    open Oberon2Lexer

    type AST =
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

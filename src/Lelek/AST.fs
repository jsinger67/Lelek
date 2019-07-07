namespace SrcNamespace

module AST =
    open LelekParser.Token
    open LelekParser.ParseTree
    open LelekParser.ParserFeedback
    open LexerModule

    type Num =
        | Num of int
            with
                static member (+) (Num(n1), Num(n2)) = Num(n1 + n2)
                static member (-) (Num(n1), Num(n2)) = Num(n1 - n2)
                static member (*) (Num(n1), Num(n2)) = Num(n1 * n2)
                static member (/) (Num(n1), Num(n2)) = Num(n1 / n2)
                static member (%) (Num(n1), Num(n2)) = Num(n1 % n2)
                static member op_LeftShift (Num(n1), Num(n2)) = Num(n1 <<< n2)
                static member op_RightShift (Num(n1), Num(n2)) = Num(n1 >>> n2)
                static member (&&&) (Num(n1), Num(n2)) = Num(n1 &&& n2)
                static member (|||) (Num(n1), Num(n2)) = Num(n1 ||| n2)
                static member (^^^) (Num(n1), Num(n2)) = Num(n1 ^^^ n2)
                static member op_BooleanAnd (Num(n1), Num(n2)) = Num(if (n1 <> 0) && (n2 <> 0) then 1 else 0)
                static member op_BooleanOr (Num(n1), Num(n2)) = Num(if (n1 <> 0) || (n2 <> 0) then 1 else 0)
                static member op_Equality (Num(n1), Num(n2)) = Num(if (n1 = n2) then 1 else 0)
                static member op_Inequality (Num(n1), Num(n2)) = Num(if (n1 <> n2) then 1 else 0)
                static member op_LessThan (Num(n1), Num(n2)) = Num(if (n1 < n2) then 1 else 0)
                static member op_LessThanOrEqual (Num(n1), Num(n2)) = Num(if (n1 <= n2) then 1 else 0)
                static member op_GreaterThan (Num(n1), Num(n2)) = Num(if (n1 > n2) then 1 else 0)
                static member op_GreaterThanOrEqual (Num(n1), Num(n2)) = Num(if (n1 >= n2) then 1 else 0)


    type Id = | Id of string

    type BinIntOp = int -> int -> int

    type Symbols = Map<Id, Num>

    type AST =
        | Var of Id
        | Res of Num
        | AssignItem of Id * string
        | Assign of Id * Num
        | Sum of (BinIntOp * Num)
        | Mul of (BinIntOp * Num)
        | Err of string

    let mutable env: Symbols = Map.empty

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
        env <- Map.empty


    let varVal (feedback: ParserFeedback) (Id(vname) as id) =
        if env |> Map.containsKey id |> not then
            feedback.printError (sprintf "Variable read: undeclared variable '%s'" vname)
            Num(0)
        else
            env.[id]

    // -------------------------------------------------------------------------
    // User actions
    // -------------------------------------------------------------------------
    let number (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let tt, tx = ParseTree.typeAndTextOfToken args.[0]
        if tt <> int TokenType.TkNumber then
            printError feedback "number: Invalid token type" args.[0]
            Err("number: Invalid token type") :: stack
        else
            Res(Num(tx |> int)) :: stack

    let id (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let tt, tx = ParseTree.typeAndTextOfToken args.[0]
        if tt <> int TokenType.TkId then
            printError feedback "id: Invalid token type" args.[0]
            Err("id: Invalid token type") :: stack
        else
            Var(Id(tx)) :: stack

    let assignItem (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        match args with
        | [ParseTree(PTItem.Var("id"), _); ParseTree(PTItem.Var("assign_op"), c)] ->
            let _, op = ParseTree.typeAndTextOfToken args.[1]
            match stack with
            | [Var(Id(_) as id)] ->
                [AssignItem(id, op)]
            | Var(Id(_) as id) :: s ->
                AssignItem(id, op) :: s
            | _ ->
                printStackError feedback "assignItem: unexpected user stack" stack
                Err("assignItem: unexpected user stack") :: stack

        | _ ->
            printError feedback "assignItem: unexpected arguments" args.[0]
            Err("assignItem: unexpected arguments") :: stack

    let assign (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let applyAssignOp id num op =
            let varval = if op = "=" then num else (varVal feedback id)
            let rval =
                match op with
                | "=" ->
                    env <- env |> Map.add id num
                    num
                | "+=" -> varval + num
                | "-=" -> varval - num
                | "*=" -> varval * num
                | "/=" -> varval / num
                | "%=" -> varval % num
                | "<<=" -> Num.op_LeftShift (varval, num)
                | ">>=" -> Num.op_RightShift (varval, num)
                | "&=" -> varval &&& num
                | "^=" -> varval ^^^ num
                | "|=" -> varval ||| num
                | _ ->
                    feedback.printError (sprintf "applyAssignOp: Not supported assignment operator '%s'" op)
                    num
            env <- env |> Map.add id rval
            Assign(id, rval)

        let rec calcAssignments stack count: AST list =
            if count = 0 then
                stack
            else
                match stack with
                | Res(n) :: AssignItem(i, op) :: t ->
                    let assgn = applyAssignOp i n op
                    calcAssignments (assgn::t) (count - 1)
                | Assign(_, n) :: AssignItem(i, op) :: t ->
                    let assgn = applyAssignOp i n op
                    calcAssignments (assgn::t) (count - 1)
                | Assign(_) :: _ ->
                    stack
                | _ ->
                    printStackError feedback "calcAssignments: unexpected user stack" stack
                    Err("calcAssignments: unexpected user stack") :: stack

        calcAssignments stack (args.Length)

    let logicalOrItem (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        match stack with
        | Res(Num(_) as a) :: (Res(Num(_) as b) :: t) ->
            Res(Num.op_BooleanOr (a, b)) :: t
        | _ -> 
            printError feedback "logicalOrItem: unexpected arguments" args.[0]
            Err("logicalOrItem: unexpected arguments") :: stack

    let logicalAndItem (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        match stack with
        | Res(Num(_) as a) :: (Res(Num(_) as b) :: t) ->
            Res(Num.op_BooleanAnd (a, b)) :: t
        | _ -> 
            printError feedback "logicalOrItem: unexpected arguments" args.[0]
            Err("logicalOrItem: unexpected arguments") :: stack

    let bitwiseOrItem (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        match stack with
        | Res(Num(_) as a) :: (Res(Num(_) as b) :: t) ->
            Res(a ||| b) :: t
        | _ -> 
            printError feedback "bitwiseOrItem: unexpected arguments" args.[0]
            Err("bitwiseOrItem: unexpected arguments") :: stack

    let bitwiseAndItem (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        match stack with
        | Res(Num(_) as a) :: (Res(Num(_) as b) :: t) ->
            Res(a &&& b) :: t
        | _ -> 
            printError feedback "bitwiseAndItem: unexpected arguments" args.[0]
            Err("bitwiseAndItem: unexpected arguments") :: stack

    let equalityItem (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let ttOp, txOp = ParseTree.typeAndTextOfToken args.[0]
        match stack with
        | Res(Num(_) as a) :: (Res(Num(_) as b) :: t) ->
            match enum ttOp, txOp with
            | TokenType.TkEqualityOp, "==" -> Res(Num.op_Equality (a, b)) :: t
            | TokenType.TkEqualityOp, "!=" -> Res(Num.op_Inequality (a, b)) :: t
            | _ ->
                printError feedback "equalityItem: unexpected operator" args.[0]
                failwith "equalityItem: unexpected operator"
        | _ -> 
            printError feedback "equalityItem: unexpected arguments" args.[0]
            Err("equalityItem: unexpected arguments") :: stack

    let relationalItem (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let ttOp, txOp = ParseTree.typeAndTextOfToken args.[0]
        match stack with
        | Res(Num(_) as a) :: (Res(Num(_) as b) :: t) ->
            match enum ttOp, txOp with
            | TokenType.TkRelationalOp, "<" -> Res(Num.op_LessThan(a, b)) :: t
            | TokenType.TkRelationalOp, "<=" -> Res(Num.op_LessThanOrEqual (a, b)) :: t
            | TokenType.TkRelationalOp, ">" -> Res(Num.op_GreaterThan(a, b)) :: t
            | TokenType.TkRelationalOp, ">=" -> Res(Num.op_GreaterThanOrEqual (a, b)) :: t
            | _ ->
                printError feedback "relationalItem: unexpected operator" args.[0]
                failwith "relationalItem: unexpected operator"
        | _ -> 
            printError feedback "relationalItem: unexpected arguments" args.[0]
            Err("relationalItem: unexpected arguments") :: stack

    let bitwiseShiftItem (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let ttOp, txOp = ParseTree.typeAndTextOfToken args.[0]
        match stack with
        | Res(Num(_) as a) :: (Res(Num(_) as b) :: t) ->
            match enum ttOp, txOp with
            | TokenType.TkBitwiseShiftOp, "<<" -> Res(Num.op_LeftShift(a, b)) :: t
            | TokenType.TkBitwiseShiftOp, ">>" -> Res(Num.op_RightShift (a, b)) :: t
            | _ ->
                printError feedback "bitwiseShiftItem: unexpected operator" args.[0]
                failwith "bitwiseShiftItem: unexpected operator"
        | _ -> 
            printError feedback "bitwiseShiftItem: unexpected arguments" args.[0]
            Err("bitwiseShiftItem: unexpected arguments") :: stack

    let summItem (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let ttOp, txOp = ParseTree.typeAndTextOfToken args.[0]
        let binOp =
            match enum ttOp, txOp with
            | TokenType.TkPlus, "+" -> (fun a b -> a + b)
            | TokenType.TkMinus, "-" -> (fun a b -> a - b)
            | _ ->
                printError feedback "summItem: unexpected operator" args.[0]
                failwith "summItem: unexpected operator"
        match stack with
        | Res(Num(b)) :: (Res(Num(a)) :: t) ->
            Res(Num(binOp a b)) :: t
        | _ -> 
            printError feedback "summItem: unexpected arguments" args.[0]
            Err("summItem: unexpected arguments") :: stack

    let multItem (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let ttOp, txOp = ParseTree.typeAndTextOfToken args.[0]
        let binOp =
            match enum ttOp, txOp with
            | TokenType.TkMultOp, "*" -> (fun a b -> a * b)
            | TokenType.TkMultOp, "/" -> (fun a b -> a / b)
            | TokenType.TkMultOp, "%" -> (fun a b -> a % b)
            | _ ->
                printError feedback "multItem: unexpected arguments" args.[0]
                failwith "multItem: unexpected arguments"
        match stack with
        | Res(Num(b)) :: (Res(Num(a)) :: t) ->
            Res(Num(binOp a b)) :: t
        | _ ->
            printError feedback "multItem: unexpected arguments" args.[0]
            Err("multItem: unexpected arguments")  :: stack

    let idRef (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        let tt, tx = ParseTree.typeAndTextOfToken args.[0]
        if tt <> int TokenType.TkId then
            printError feedback "idRef: Invalid token type" args.[0]
            Err("idRef: Invalid token type") :: stack
        else
            if env |> Map.containsKey (Id(tx)) |> not then
                printError feedback "idRef: undeclared variable" args.[0]
                Err(sprintf "idRef: undeclared variable %s" tx)  :: stack
            else
                match stack with
                | Var(Id(i)) :: t ->
                    assert (i = tx)
                    Res(env.[Id(tx)]) :: t
                | _ ->
                    printError feedback "idRef: Id on top of user stack expected" args.[0]
                    Err("idRef: Id on top of user stack expected") :: stack

    let power (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =        
        let rec calcpow (nums: AST list) res =
            match nums with
            | []    ->
                res
            | Res(Num(i)) :: t   ->
                (pown i res) |> calcpow t
            | _     ->
                printStackError feedback "calcPow: unexpected user stack" nums
                0
                
        match args with
        | [ParseTree(PTItem.Var("factor"), _); ParseTree(PTItem.Var("power_lst1"), c)] ->
            Res(Num(calcpow (stack |> List.take (c.Length + 1)) 1)) :: (stack |> List.skip (c.Length + 1))
        | [ParseTree(PTItem.Var("factor"), _)] ->
            Res(Num(calcpow (stack |> List.take 1) 1)) :: (stack |> List.skip 1)
        | _ ->
            printError feedback "power: unexpected arguments" args.[0]
            Err("power: unexpected arguments") :: stack

    let negate (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
        match stack with
        | Res(Num(i)) :: t ->
            Res(Num(-i)) :: t
        | _ -> 
            printStackError feedback "negate: unexpected user stack" stack
            Err("negate: unexpected user stack") :: stack

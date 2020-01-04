namespace LelekParser

module ParseTree =
    open Token

    type PTItem =
        | Tok of Token
        | Var of string

    module PTItem =
        let tokenOf = function
            | Tok t -> t
            | Var _ -> failwith "No token!"

        let varOf = function
            | Tok _ -> failwith "No var!"
            | Var v -> v

        let toString = function
            | Tok t -> Token.toString t
            | Var v -> sprintf @"<%s>" v

        let toShortString = function
            | Tok t -> Token.toShortString t
            | Var v -> sprintf @"%s" v

        let isToken = function
            | Tok _ -> true
            | Var _ -> false

        let isVar = function
            | Tok _ -> false
            | Var _ -> true

    type ParseTree =
        | ParseTree of Item: PTItem * Children: ParseTree list

    [<RequireQualifiedAccess>]
    module ParseTree =
        let empty =
            ParseTree(Var(""), [])

        let itemOf (ParseTree(Item = item)) =
            item

        let tokenOf (ParseTree(Item = item)) =
            PTItem.tokenOf item

        let childrenOf (ParseTree(Children = children)) =
            children

        /// Depth first folding
        /// Can be used to traverse a parse tree with a visitor as folder
        let rec fold (folder: 'State -> ParseTree -> 'State) (state: 'State) (ptree: ParseTree) =
            let children = childrenOf ptree
            let state' =
                children
                |> List.fold (fun acc elem -> fold folder acc elem) state
            let state'' = folder state' ptree
            state''

        let rec hasAnyToken (ParseTree(Item = item; Children = children)) =
            let isToken = item |> PTItem.isToken
            isToken || (children |> List.isEmpty |> not) //(children |> List.exists hasAnyToken)

        let removeNonproductiveSubtrees (ParseTree(Item = item; Children = children)) =
            ParseTree(item, children |> List.filter hasAnyToken)

        /// Search the tree until the item is a token.
        /// If it is a variable and has only one child we dig deeper.
        /// Otherwise the result would be undefined thus we return None.
        let rec digUpToken (ParseTree(Item = item; Children = children)) =
            match item with
            | Tok token ->
                Some token
            | PTItem.Var _ ->
                match children with
                | [c] -> digUpToken c
                | _ -> None

        /// Search the tree until the item is a non-terminal with the given Name.
        /// If it is another variable and has only one child we dig deeper.
        /// Otherwise the result would be undefined thus we return None.
        let rec digUpVar varName (ParseTree(Item = item; Children = children) as var) =
            match item with
            | Tok _ ->
                None
            | PTItem.Var v when v = varName ->
                Some var
            | PTItem.Var _ ->
                match children with
                | [c] -> digUpVar varName c
                | _ -> None

        let typeAndTextOfToken (pt: ParseTree) =
            let tokenOpt = digUpToken pt 
            match tokenOpt with
            | Some token ->
                let tt = token |> Token.typeOf
                let tx = token |> Token.textOf
                tt, tx
            | None ->
                failwith "Can't find direct child token"


        let extractRightMostElem l =
            match l with
            | [] -> failwith "Empty list!"
            | _ ->
                let len = l |> List.length
                (l.[len - 1]), l |> List.take (len - 1)

        // We have a parsetree like that:
        //            a_lst1
        //          /        \
        //    a_lst1_itm1    a_lst1
        //         :        /      \
        //            a_lst1_itm1  a_lst1
        //                 :       /     \
        //                  a_lst1_itm1  a_lst1
        //                        :       <NUL>
        //
        // We want to transform it to this:
        //                    a_lst1
        //          /           |            \
        //    a_lst1_itm1  a_lst1_itm1  a_lst1_itm1
        //         :            :            :

        // If we see a rightmots child that has the same name as this node and this child further
        // has a child with the same name we place the grandchilds into our child list.
        // Because this method is called by the parser for the deepest child first we end up with
        // the desired structure.
        let raiseList (pt: ParseTree): ParseTree =
            let (ParseTree(Item = item; Children = children)) = pt
            match children with
            | [] ->
                pt
            | _ ->
                let rM, rest = children |> extractRightMostElem
                let (ParseTree(Item = childitem; Children = grandchildren)) = rM
                if item <> childitem then   // Not the same name
                    pt
                else
                    match grandchildren with
                    | [] ->
                        pt
                    | _ ->
                        ParseTree(item, rest @ grandchildren)


        let makePT (ruleName: string) (args: ParseTree list): ParseTree =
            ParseTree(Var ruleName, args)
            |> removeNonproductiveSubtrees

        let clipPT (ruleName: string) (args: ParseTree list): ParseTree =
            ParseTree(Var ruleName, args)
            |> removeNonproductiveSubtrees

        let collectPT (ruleName: string) (args: ParseTree list): ParseTree =
            ParseTree(Var ruleName, args)
            |> removeNonproductiveSubtrees
            |> raiseList


    module ParseTreeList =
        let indices vars pts =
            vars
            |> List.map (
                fun v ->
                    pts
                    |> List.tryFindIndex((ParseTree.digUpVar v) >> Option.isSome)
            )



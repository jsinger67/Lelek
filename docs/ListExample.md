List - A more detailed example
==
If you want you can build the lexer and parser for the *List* example with the following command lines:

    cd src\Lelek
    dotnet run --file ..\..\Grammars\lst.llk  --generate-lexersrc ..\..\examples\List\ListLexer.fs --generate-parsersrc ..\..\examples\List\ListParser.fs --save-linear-grammar --asttype AST.AST

Then you can start and test the *List* examples executable by executing the following two commands:

    cd ..\..\examples\List
    dotnet run .\ListTest.txt


You should get the following output
> Parsing succeeded  
  Warning(s): 0, Error(s): 0  
  [Num 1; Num 2; Num 33]

The last line shows the result the parser has returned from the parsing of the content '1, 2, 33', a list of numbers just as expected.

Now you should read the [SemanticActions.md](SemanticActions.md) if you haven't done so yet because it describes some fundamentals about semantic actions and does this by using the *List* example.

Here we will have a closer look at the implementation of the semantic action `number`.

But first we recap where our `number` action is located in the grammar description [List.llk](../grammars/lst.llk).  
Here is the complete content of this grammar description:

```ebnf
(* A simple comma separated list *)
start               = (* epsilon *)
                    | number { "," number }
;

number              = "\d+"             @AST.number
;

```

What we know is that the parser will call this function with the current number token in the `args` parameter as the single entry of this list. We extract the tokens text from this `args.[0]` or `args.Head` with the help of the helper function `ParseTree.typeAndTextOfToken`. Then after successfully checking the tokens type we push the new AST Num item on the user stack.  
The last line of this method does exactly this:

```fsharp
let number (feedback: ParserFeedback) (stack: AST list) (args: ParseTree list): AST list =
    let tt, tx = ParseTree.typeAndTextOfToken args.[0]
    if tt <> int TokenType.TkNumber then
        printError feedback "number: Invalid token type" args.[0]
        Err("number: Invalid token type") :: stack
    else
        Num(tx |> int) :: stack
```

At the end of the parsing our stack will contain all 'pushed in' AST Num items, but in reversed order.
In the `main` method of the *List* example we take the user stack, reverse it and print the result to the console:

```fsharp
us |> List.rev |> printfn "%A"
```

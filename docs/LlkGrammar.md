The syntax of LLK Grammar definition
==
Here I provide the definition of the LLK grammar in EBNF.

```ebnf
(* LLK Grammar defined in EBNF *)
Grammar         = CommentDecl, Rules;                   (* The start symbol of the LLK grammar *)
CommentDecl     = [LineComment], [BlockComment];        (* Comment declarations are optional *)
LineComment     = '%comment', String;                   (* Defines the start sequence of a line comment *)
BlockComment    = '%comment', String, String;           (* Defines the start and end sequences of a block comment *)
Rules           = Rule, {Rule};                         (* There must be at least one rule - the start rule *)
Rule            = NonTerminal, '=', Expression, ';';
Expression      = Alternations;
Alternations    = Sequence, {'|', Alternations};
Sequence        = {Factor}, [Action], ';';
Factor          = Group                                 (* A grouping *)
                | Repeat                                (* A repetition *)
                | Optional                              (* An optional expression *)
                | NonTerminal                           (* EBNF: Meta-identifier *)
                | Terminal;                             (* EBNF: Terminal-string, always treated as a regular expression! *)
Group           = '(', Expression, ')';
Repeat          = '{', Expression, '}';
Optional        = '[', Expression, ']';
NonTerminal     = Identifier;
Terminal        = String;
String          = "\"([^\\]|(\\.))*?\"";
Action          = '@', Fun;
Fun             = Identifier, {'.', Identifier};        (* A qualified name of an F# function *)
Identifier      = "[a-zA-Z_]\w*";
```

This is a very concise grammar that most programmers should be familiar with. There are several specialties described here. First please notice the built-in support for language comments.

Using the `%comment` construct you can easily define your languages comments. For example you can define comments like it's done in the calculator example [Calc.llk](..\grammars\Calc.llk):

```ebnf
%comment "//"
%comment "\(\*" "\*\)"
```

Also - as an extension to EBNF - you can use C-like line comments starting with two slashes (//) in LLK files.

The start symbol
--
It is important to note that _the very first rule is always treated as the start symbol of the grammar_.


Lexer control
--
A lexer is automatically created from all used terminal symbols.

Since Lelek creates a lexer on the base of .NET Regex class all terminals are treated as if they were regular expressions.
Thus you have to consider the following caveats.

* If you want to use a character that is a regex meta-character you have to escape it
* In case of conflicts between different terminals _the first seen will win_

The last point needs a more detailed explanation.
It's best to show an example for such a situation.
Say you have two terminals "-" and "--", _minus_ and _decrement_. The generated lexer is then based on the following regular expression:

    "-|--"

The .NET Regex class will now match two times _minus_ when actually a _decrement_ operator should be detected.
It behaves here differently than a classic lexer like Lex that obeys the _longest match_ strategy.

Fortunately there is a simple way to achieve what we want. We just need a resulting regular expression with a different order:

    "--|-"

This will perfectly do the job.

To get such an order the _decrement_ terminal has to be defined `before` the _minus_ terminal as in the following snippet.

```ebnf
decrement = "--"
;
(* ... *)
minus = "-"
;
```

Thats all.

With this simple but effective means you have the control over lexer conflicts.

Placing semantic actions
--
You can place semantic action at the end of each `Sequence`. This means each alternative (syntactically a sequence) can have at most one action.

Lets look at an example.

```ebnf
rule1 = a b c @action1
      | d e f @action2
;
```

You could say that this is a restriction if you wanted to place an action between lets say *a* and *b*.

But there is no great effort to achieve this because you can introduce an intermediate rule like this:

```ebnf
rule1 = intermediateRule b c @action1
      | d e f @action2
;

intermediateRule = a @actionBetween_a_and_b
;
```

More on implementing sematic actions see
* [Sematic actions](./SemanticActions.md)

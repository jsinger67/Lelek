<!-- markdownlint-disable first-line-h1 -->
![.NET Core](https://github.com/jsinger67/Lelek/workflows/.NET/badge.svg)
<!-- markdownlint-enable first-line-h1 -->
# About Lelek

Lelek is a parser generator `for F# written in F#` with the following features

* Generated parsers are `table driven`
* Generated parsers are predictive, i.e. they implement a `non-backtracking` parsing technique
* Uses a special `LL(k)` technique; my approach is explained [here](docs/Approach.md)
* Generated parsers use a `flexible lookahead` for each non-terminal; they use only as much lookahead as needed
* Rule selection is done by a deterministic finite `lookahead automaton` for each non-terminal
* Lexers and parsers are generated from `one single grammar description` file
* The grammar description (coined 'LLK grammar') is provided in an `EBNF-like` style
* `Sematic actions` can be embedded in this LLK grammar description as function calls
* This is the only coupling you have to specify between your language definition and the language processing
* The grammar description supports definition of language comment declaration via a `%comment` construct
* Provides tools for `parse tree visualization` to support your grammar implementation
* It `detects direct and indirect left recursions` in your grammar description
* Comments are part of the grammar and they are bound to language constructs like rules. This way the comments keep their place within the structure of the grammar and can be reproduced in transformed grammar files.

Lelek currently generates simple lexers based on .NET Regex. These lexers automatically skip whitespaces to support the most common use cases directly. But it is planned to make this property configurable.

This project contains some introductory grammar examples from entry level up to a complete Oberon-2 LLK grammar.
There are also examples that describe the principles of language processing possible with the Lelek tool using semantic actions.  
But of course there exists another opportunity to process the parse result.
The concrete syntax tree the parser spits out can be processed by user created tools too. So no one is tied to Lelek's approach to Semantic Actions.

## Current state of the project

Lelek isn't actively developed anymore. All work is now dedicated to the new [`parol`](https://github.com/jsinger67/parol) parser generator written in Rust.
Lelek was a proof of concept and I learnt a lot while working on it. Many concepts have been taken over to `parol` and some have been abandoned.

For instance, the concept of using regular expressions to describe scanners is closely kept. On the other hand the way of calculating the lookahead was completely reworked in `parol`. Also the detection of left-recursions was changed.

## Changes in V0.2.1

With version V0.2.1 this project switched over to **.NET 5.0**.

## Changes in V0.2.0

The LLK grammar had to be changed in a minor detail - the rules section must be started by a **%grammar** token. The possibly following syntactic comment is then bound to the first rule.

But this effectively renders all previous grammar files invalid.
Fortunately the change is easy to apply.

The cause was an indirect left recursion in the old grammar that was not correctly detected in V0.1.0. The recursion also introduced an ambiguity regarding the syntactic comments has to be bound either to the whole grammar file or to the first rule.

## Further readings

* [Introduction to Lelek](docs/Introduction.md)
* [Project Structure](src/ProjectStructure.md)

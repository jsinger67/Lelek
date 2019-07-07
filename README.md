## About Lelek

Lelek is a parser generator `for F# written in F#` with the following features

* Generated parsers are `table driven`
* Generated parsers are predictive, i.e. they implement a `non-backtracking` parsing technique
* Uses a special `LL(k)` technique: LLm(n), i.e.
* Generated parsers use a `flexible lookahead` for each non-terminal, i.e. only as much lookahead as needed
* Rule selection is done by a deterministic finite `lookahead automaton` for each non-terminal
* Lexers and parsers are generated from `one single grammar description` file
* The grammar description (coined 'LLK grammar') is provided in an `EBNF-like` style
* `Sematic actions` can be embedded in this LLK grammar description as function calls
* This is the only coupling you have to specify between your language definition and the language processing
* The grammar description supports definition of language comment declaration via a `%comment` construct
* Provides tools to `visualize parse trees` to support your grammar implementation
* It `detects direct and indirect left recursions` in your grammar description

Lelek currently generates simple lexers based on .NET Regex. These lexers automatically skip whitespaces to support the most common use cases directly. But it is planned to make this property configurable.

This project contains some introductory grammar examples from entry level up to a complete Oberon-2 LLK grammar.
There are also examples that describe the principles of language processing possible with the Lelek tool using semantic actions.  
But there also exists another opportunity to process the parse result.
The concrete syntax tree the parser spits out can be processed by user created tools too, of course. So no one is tied to the Lelek's approach to Semantic Actions.

Lelek is a proof of concept.
I decided to us .NET Core for this project to make it available to a greater group of developers.


Further readings

* [Introduction to Lelek](docs/Introduction.md)
* [Project Structure](src/ProjectStructure.md)

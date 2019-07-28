Lelek can generate Lelek
==

It is possible to generate Lelek with itself.


    cd src\Lelek
    dotnet run --file ..\LelekBS\llk.llk  --generate-lexersrc ..\LelekBS\LlkLexer.fs --generate-parsersrc ..\LelekBS\LlkParser.fs --save-linear-grammar --asttype AST.AST --lexermodulename LlkLexer --parsermodulename LlkParser --srcnamespacename LelekBS

This command line generates the tools own LLK lexer and parser source files by interpreting the LLK language description in [Llk.llk](../src/LelekBS/Llk.llk).

But to be safe the normal way to generate Lelek lexer and parser is to use the LlkBootstrap executable that is built upon the great **FParsec** parser generator.

This can be done explicitly using this command line.

    cd src\LlkBootstrap
    dotnet run <Path to solution>\src\LlkBootstrap\

Actually no such generation is necessary at all because the generated lexer and parser for LLK are added to source control.  
This has been done intentionally to be able to easily detect changes in the generated sources.

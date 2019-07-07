Project structure and dependencies
===

                                    LlkGrammar Lib
                                            |
                        +-----------------------------------+
                        v                                   |
                FParsec Package                             |   DotLiquid Package               Argu Package
                        |                                   |       |                               |
                        |                                   |       |                               |
                        v                                   v       v                               |
            LlkParserBootstrap Lib                      LelekParser Lib                             |
                            |                           |   |   |   |                               |
                            |                           |   |   |   +---------------+               |
                            |   +-----------------------+   |   +-------+           |               |
                            v   v                           v           |           v               |
    Llk.llk     ===>>   LlkBootstrap Exe    ===>>       LelekBS Lib     |   ParseTreeVisualization  |
                        ======>>>>>>                        |           |           |               |
    L e x e r  a n d  P a r s e r  G e n e r a t i o n      +-------+   |   +-------+               |
                                                                    |   |   |   +-------------------+
                                                                    |   |   |   |
                                                                    v   v   v   v
                                                                      Lelek Exe


Used Third-Party Packages
---

* **FParsec** - 2-clause BSD

  Was used as LLK frontend for the first development of the Lelek system.
  Later it was possible to generate an LLK Parser with Lelek itself.

* **DotLiquid** - Apache License, Version 2.0

  Used as template machine for the source code generation.

* **Argu** - MIT Licence

  Command line parser of choice for the command line tools.


Project libraries
---

* LlkGrammar

  It basically provides the types related to the output of the LLK frontends.

  There exist two different LLK frontends:

  + LlkParserBootstrap Lib/LlkBootstrap Exe
    
  + LelekBS Lib/Lelek Exe
  

* LlkParserBootstrap

  Based on FParsec, used to generate the second LLK frontend (LelekBS Lib/Lelek Exe).

* LelekParser

  All the stuff to process the LlkGrammar provided by the LLK frontend and eventually generate the lexers and parsers that in combination can process languages of the given grammar description.

* LelekBS

  Bootstrapped LLK frontend library that was build from an Llk.llk language grammar description. It is capable of generating itself by parsing the Llk.llk.

* ParseTreeVisualization

  Useful tool when implementing your own LLK grammar. It generates abstract embeddings of parse trees which can be processed by graphical format generation tools on a later stage.
  
  This library provides two implementation of such a graphical format generation tool for
  
  + PNG and
  + SVG


Command line tools
---

* LlkBootstrap

  Internal tool to create the bootstrapped LLK frontend in LelekBS library.

* Lelek

  The actual Llk Parser generator.



[README](./README.md)